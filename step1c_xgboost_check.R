
# QUICK CHECK: XGBoost robustness test (default settings, ~2 min)

# Purpose: confirm the income/race sensitivity gap found by Random Forest
# isn't an artifact of that specific algorithm. Uses the SAME train_df/test_df
# objects already built by nhanes_disability_step1.R -- no data re-cleaning.
# Run this AFTER sourcing the full step1 script (train_df/test_df must exist).


required_pkgs <- c("xgboost", "Matrix")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) install.packages(new_pkgs)

library(xgboost)
library(Matrix)

# Use the SAME feature set as the race-included RF run (Step 1 main script)
feature_cols_xgb <- c("sex", "age", "race_eth_label", "education", "marital_status",
                      "poverty_ratio", "household_size", "uses_special_equipment",
                      "chronic_condition_count", "has_insurance", "general_health_rating",
                      "health_worse_than_last_yr", "has_usual_healthcare_place",
                      "n_healthcare_visits_12mo", "work_status")

# xgboost needs numeric matrices, not factors -- one-hot encode via model.matrix
train_xgb_df <- train_df %>% select(all_of(feature_cols_xgb), disability_label) %>%
  mutate(across(where(is.factor), as.factor))

test_xgb_df <- test_df %>% select(all_of(feature_cols_xgb), disability_label, seqn) %>%
  mutate(across(where(is.factor), as.factor))

# Align factor levels between train/test before one-hot encoding (same issue
# we hit with randomForest -- xgboost has the same requirement)
for (col in feature_cols_xgb) {
  if (is.factor(train_xgb_df[[col]])) {
    test_xgb_df[[col]] <- factor(test_xgb_df[[col]], levels = levels(train_xgb_df[[col]]))
  }
}

train_matrix <- sparse.model.matrix(disability_label ~ . - 1,
                                    data = train_xgb_df %>% select(all_of(feature_cols_xgb), disability_label))
test_matrix  <- sparse.model.matrix(disability_label ~ . - 1,
                                    data = test_xgb_df %>% select(all_of(feature_cols_xgb), disability_label))

train_label_xgb <- as.numeric(train_xgb_df$disability_label) - 1  # 0/1
test_label_xgb  <- as.numeric(test_xgb_df$disability_label) - 1

dtrain <- xgb.DMatrix(data = train_matrix, label = train_label_xgb)
dtest  <- xgb.DMatrix(data = test_matrix, label = test_label_xgb)

# Default-ish settings -- this is a robustness check, not a tuning exercise
xgb_model <- xgb.train(
  params = list(objective = "binary:logistic", eval_metric = "auc",
                max_depth = 6, eta = 0.1),
  data = dtrain,
  nrounds = 200,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 15,
  verbose = 1
)

cat(sprintf("\nBest iteration: %d | Best test AUC: %.4f\n",
            xgb_model$best_iteration, xgb_model$best_score))

# Predict on held-out test set
test_xgb_df$predicted_prob_yes <- predict(xgb_model, dtest)
test_xgb_df$predicted_label <- factor(ifelse(test_xgb_df$predicted_prob_yes > 0.5, "Yes", "No"),
                                      levels = c("No", "Yes"))

# Reattach race/income for subgroup scoring
test_xgb_scored <- test_xgb_df %>%
  left_join(test_df %>% select(seqn, race_eth_label_check = race_eth_label, income_tier), by = "seqn")

cat("\n===== XGBOOST SUBGROUP ACCURACY: RACE/ETHNICITY =====\n")
print(subgroup_accuracy(test_xgb_scored %>% rename(race_eth_label_orig = race_eth_label_check),
                        "race_eth_label_orig"))

cat("\n===== XGBOOST SUBGROUP ACCURACY: INCOME TIER =====\n")
print(subgroup_accuracy(test_xgb_scored, "income_tier"))

cat("\n===== COMPARISON: Did the pattern survive a different algorithm? =====\n")
cat("Compare these sensitivity/FNR numbers directly against your Random Forest\n")
cat("results from step1_subgroup_accuracy_race.csv / _income.csv.\n")
cat("If 'Above 4x poverty line' and 'Non-Hispanic Asian' still show the worst\n")
cat("sensitivity/highest FNR here, the pattern is algorithm-independent.\n")

write_csv(test_xgb_scored, "data/step1_xgboost_test_predictions.csv")
cat("\nSaved: step1_xgboost_test_predictions.csv\n")