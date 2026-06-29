
# STEP 3D: Refit on Pooled Data (Random Forest + XGBoost + Bootstrap CIs)

# Loads the pooled dataset from Step 3c and re-runs the full Step 1 + Step 1B
# pipeline on it: train/test split, Random Forest fit, subgroup scoring,
# XGBoost robustness check, and bootstrap confidence intervals - all on the
# ~3x larger, race-subgroup-richer pooled data instead of single-cycle J.
#
# PREREQUISITE: run step3c_pool_cycles.R first, OR load the saved CSV below.


library(tidyverse)
library(randomForest)
library(caret)
library(pROC)
library(xgboost)
library(Matrix)


# 0. LOAD POOLED DATA

if (!exists("pooled_df")) {
  cat("pooled_df not found in memory - loading from saved CSV\n")
  pooled_df <- read_csv("data/step3c_pooled_analytic_df.csv", show_col_types = FALSE)
  
  # Re-apply factor typing lost on CSV round-trip
  pooled_df <- pooled_df %>%
    mutate(
      sex = factor(sex),
      education = factor(education),
      marital_status = factor(marital_status),
      work_status = factor(work_status),
      has_insurance = factor(has_insurance),
      has_usual_healthcare_place = factor(has_usual_healthcare_place),
      uses_special_equipment = factor(uses_special_equipment),
      general_health_rating = factor(general_health_rating),
      n_healthcare_visits_12mo = factor(n_healthcare_visits_12mo,
                                        levels = c("None", "1", "2 to 3", "4 to 5", "6 to 7", "8 to 9",
                                                   "10 to 12", "13 to 15", "16 or more", "Missing"),
                                        ordered = TRUE),
      disability_label = factor(disability_label, levels = c("No", "Yes"))
    )
}

cat(sprintf("Pooled data loaded: %d rows\n", nrow(pooled_df)))
cat(sprintf("Label balance: %.1f%% Yes\n", 100 * mean(pooled_df$disability_label == "Yes")))


# 1. TRAIN/TEST SPLIT (same stratification logic as Step 1)

set.seed(42)

pooled_df <- pooled_df %>% mutate(strata_var = paste(disability_label, race_eth_label))

train_idx <- createDataPartition(pooled_df$strata_var, p = 0.75, list = FALSE)
train_df  <- pooled_df[train_idx, ]
test_df   <- pooled_df[-train_idx, ]

cat(sprintf("\nTrain n = %d | Test n = %d\n", nrow(train_df), nrow(test_df)))


# 2. RANDOM FOREST - same feature set as Step 1

feature_cols <- c("sex", "age", "race_eth_label", "education", "marital_status",
                  "poverty_ratio", "household_size", "uses_special_equipment",
                  "chronic_condition_count", "has_insurance", "general_health_rating",
                  "health_worse_than_last_yr", "has_usual_healthcare_place",
                  "n_healthcare_visits_12mo", "work_status")

train_model_df <- train_df %>% select(disability_label, all_of(feature_cols)) %>%
  mutate(race_eth_label = factor(race_eth_label), work_status = factor(work_status))

test_model_df <- test_df %>% select(disability_label, all_of(feature_cols), seqn) %>%
  mutate(
    race_eth_label = factor(race_eth_label, levels = levels(train_model_df$race_eth_label)),
    work_status = factor(work_status, levels = levels(train_model_df$work_status)),
    general_health_rating = factor(general_health_rating, levels = levels(train_model_df$general_health_rating)),
    n_healthcare_visits_12mo = factor(n_healthcare_visits_12mo,
                                      levels = levels(train_model_df$n_healthcare_visits_12mo), ordered = TRUE)
  )

cat("\nFitting Random Forest on pooled data (this will take longer than single-cycle - 3x the rows)...\n")
rf_model_pooled <- randomForest(
  disability_label ~ .,
  data = train_model_df,
  ntree = 500,
  importance = TRUE,
  na.action = na.omit
)

print(rf_model_pooled)

test_model_df$predicted_label <- predict(rf_model_pooled, newdata = test_model_df)
test_model_df$predicted_prob_yes <- predict(rf_model_pooled, newdata = test_model_df, type = "prob")[, "Yes"]

test_scored_pooled <- test_model_df %>%
  left_join(test_df %>% select(seqn, race_eth_label, income_tier, education), by = "seqn", suffix = c("", "_orig"))

cat("\n===== OVERALL CONFUSION MATRIX (POOLED, RF) =====\n")
print(confusionMatrix(test_scored_pooled$predicted_label, test_scored_pooled$disability_label, positive = "Yes"))

roc_pooled <- roc(test_scored_pooled$disability_label, test_scored_pooled$predicted_prob_yes,
                  levels = c("No", "Yes"), direction = "<")
cat(sprintf("\nOverall AUC (pooled): %.3f\n", auc(roc_pooled)))

# --------------------------------------
# Subgroup tables - the main payoff of pooling: tighter race estimates
# --------------------------------------
subgroup_accuracy <- function(df, group_var) {
  df %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      n = n(),
      accuracy = mean(predicted_label == disability_label, na.rm = TRUE),
      sensitivity = sum(predicted_label == "Yes" & disability_label == "Yes") / pmax(sum(disability_label == "Yes"), 1),
      specificity = sum(predicted_label == "No" & disability_label == "No") / pmax(sum(disability_label == "No"), 1),
      false_negative_rate = 1 - sensitivity,
      false_positive_rate = 1 - specificity,
      .groups = "drop"
    ) %>%
    arrange(desc(false_negative_rate))
}

cat("\n===== SUBGROUP (POOLED, RF): RACE/ETHNICITY =====\n")
race_pooled <- subgroup_accuracy(test_scored_pooled, "race_eth_label")
print(race_pooled)

cat("\n===== SUBGROUP (POOLED, RF): INCOME TIER =====\n")
income_pooled <- subgroup_accuracy(test_scored_pooled, "income_tier")
print(income_pooled)


# 3. XGBOOST ROBUSTNESS CHECK ON POOLED DATA

cat("\n\n-- XGBoost on pooled data --\n")

train_xgb_df <- train_df %>% select(all_of(feature_cols), disability_label) %>%
  mutate(across(where(is.factor), as.factor))
test_xgb_df <- test_df %>% select(all_of(feature_cols), disability_label, seqn) %>%
  mutate(across(where(is.factor), as.factor))

for (col in feature_cols) {
  if (is.factor(train_xgb_df[[col]])) {
    test_xgb_df[[col]] <- factor(test_xgb_df[[col]], levels = levels(train_xgb_df[[col]]))
  }
}

train_matrix <- sparse.model.matrix(disability_label ~ . - 1,
                                    data = train_xgb_df %>% select(all_of(feature_cols), disability_label))
test_matrix  <- sparse.model.matrix(disability_label ~ . - 1,
                                    data = test_xgb_df %>% select(all_of(feature_cols), disability_label))

train_label_xgb <- as.numeric(train_xgb_df$disability_label) - 1
test_label_xgb  <- as.numeric(test_xgb_df$disability_label) - 1

dtrain <- xgb.DMatrix(data = train_matrix, label = train_label_xgb)
dtest  <- xgb.DMatrix(data = test_matrix, label = test_label_xgb)

xgb_model_pooled <- xgb.train(
  params = list(objective = "binary:logistic", eval_metric = "auc", max_depth = 6, eta = 0.1),
  data = dtrain, nrounds = 200,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 15, verbose = 0
)

cat(sprintf("XGBoost (pooled) best test AUC: %.4f at iteration %d\n",
            xgb_model_pooled$best_score, xgb_model_pooled$best_iteration))

test_xgb_df$predicted_prob_yes <- predict(xgb_model_pooled, dtest)
test_xgb_df$predicted_label <- factor(ifelse(test_xgb_df$predicted_prob_yes > 0.5, "Yes", "No"), levels = c("No", "Yes"))

test_xgb_scored <- test_xgb_df %>%
  left_join(test_df %>% select(seqn, race_eth_label, income_tier), by = "seqn", suffix = c("", "_orig"))

cat("\n===== XGBOOST SUBGROUP (POOLED): RACE/ETHNICITY =====\n")
print(subgroup_accuracy(test_xgb_scored, "race_eth_label_orig"))

cat("\n===== XGBOOST SUBGROUP (POOLED): INCOME TIER =====\n")
print(subgroup_accuracy(test_xgb_scored, "income_tier"))


# 4. BOOTSTRAP CIs ON POOLED DATA (500 iterations, same as single-cycle)

cat("\n\n-- Bootstrap CIs on pooled data (500 iterations - larger n means each\n")
cat("    fit takes longer than single-cycle, but CIs should be substantially\n")
cat("    tighter, especially for race subgroups) --\n")

N_BOOTSTRAP <- 2000

bootstrap_subgroup_metrics_pooled <- function(seed_val) {
  set.seed(seed_val)
  
  df_boot <- pooled_df %>% mutate(strata_var = paste(disability_label, race_eth_label))
  idx <- createDataPartition(df_boot$strata_var, p = 0.75, list = FALSE)
  train_b <- df_boot[idx, ]
  test_b  <- df_boot[-idx, ]
  
  train_model_b <- train_b %>% select(disability_label, all_of(feature_cols)) %>%
    mutate(race_eth_label = factor(race_eth_label), work_status = factor(work_status))
  
  test_model_b <- test_b %>% select(disability_label, all_of(feature_cols), seqn) %>%
    mutate(
      race_eth_label = factor(race_eth_label, levels = levels(train_model_b$race_eth_label)),
      work_status = factor(work_status, levels = levels(train_model_b$work_status)),
      general_health_rating = factor(general_health_rating, levels = levels(train_model_b$general_health_rating)),
      n_healthcare_visits_12mo = factor(n_healthcare_visits_12mo,
                                        levels = levels(train_model_b$n_healthcare_visits_12mo), ordered = TRUE)
    )
  
  rf_b <- randomForest(disability_label ~ ., data = train_model_b, ntree = 500, na.action = na.omit)
  test_model_b$predicted_label <- predict(rf_b, newdata = test_model_b)
  
  test_scored_b <- test_model_b %>%
    left_join(test_b %>% select(seqn, race_eth_label, income_tier), by = "seqn", suffix = c("", "_orig"))
  
  race_metrics <- test_scored_b %>%
    group_by(race_eth_label_orig) %>%
    summarise(sensitivity = sum(predicted_label == "Yes" & disability_label == "Yes") / pmax(sum(disability_label == "Yes"), 1),
              .groups = "drop") %>%
    mutate(group_type = "race", boot_id = seed_val) %>%
    rename(group = race_eth_label_orig)
  
  income_metrics <- test_scored_b %>%
    group_by(income_tier) %>%
    summarise(sensitivity = sum(predicted_label == "Yes" & disability_label == "Yes") / pmax(sum(disability_label == "Yes"), 1),
              .groups = "drop") %>%
    mutate(group_type = "income", boot_id = seed_val) %>%
    rename(group = income_tier)
  
  bind_rows(race_metrics, income_metrics)
}

cat(sprintf("Starting %d bootstrap iterations at %s\n", N_BOOTSTRAP, Sys.time()))

results_list <- vector("list", N_BOOTSTRAP)
for (i in 1:N_BOOTSTRAP) {
  results_list[[i]] <- bootstrap_subgroup_metrics_pooled(seed_val = 2000 + i)
  if (i %% 25 == 0) {
    cat(sprintf("  Completed %d / %d at %s\n", i, N_BOOTSTRAP, Sys.time()))
    saveRDS(bind_rows(results_list[1:i]), "step3d_bootstrap_progress_checkpoint.rds")
  }
}

all_boot_pooled <- bind_rows(results_list)
write_csv(all_boot_pooled, "data/step3d_bootstrap_pooled_raw.csv")

ci_summary_pooled <- all_boot_pooled %>%
  group_by(group_type, group) %>%
  summarise(
    n_iterations = n(),
    mean_sensitivity = mean(sensitivity, na.rm = TRUE),
    ci_low_sensitivity = quantile(sensitivity, 0.025, na.rm = TRUE),
    ci_high_sensitivity = quantile(sensitivity, 0.975, na.rm = TRUE),
    mean_fnr = 1 - mean_sensitivity,
    ci_low_fnr = 1 - ci_high_sensitivity,
    ci_high_fnr = 1 - ci_low_sensitivity,
    .groups = "drop"
  ) %>%
  arrange(group_type, mean_fnr)

write_csv(ci_summary_pooled, "data/step3d_bootstrap_ci_summary_pooled.csv")

cat("\n\n===== BOOTSTRAPPED 95% CIs (POOLED DATA) =====\n")
print(ci_summary_pooled, n = 50)


# SAVE EVERYTHING

write_csv(test_scored_pooled, "data/step3d_test_predictions_pooled.csv")
write_csv(race_pooled, "data/step3d_subgroup_race_pooled.csv")
write_csv(income_pooled, "data/step3d_subgroup_income_pooled.csv")

cat("\n\nStep 3d complete. Compare these files directly against your single-cycle\n")
cat("Step 1 results to see how much pooling tightened the race-subgroup CIs:\n")
cat("  - step3d_subgroup_race_pooled.csv\n")
cat("  - step3d_subgroup_income_pooled.csv\n")
cat("  - step3d_bootstrap_ci_summary_pooled.csv   (THE key comparison file)\n")