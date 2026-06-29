
# STEP 3A: Class Imbalance Sanity Check (single-cycle data, pre-pooling)

# Purpose: your label is imbalanced (~30% Yes / 70% No). A naive 0.5
# probability cutoff on imbalanced data is KNOWN to favor the majority class,
# which could explain the low sensitivity (~52-57%) seen across all model
# variants so far - independent of whether there's a genuine signal-
# availability problem in the data. This script checks whether the
# SUBGROUP GAPS (the actual finding that matters for your paper) survive
# once the threshold is chosen properly, rather than left at a naive 0.5.
#
# Two corrections tested:
#   (1) Optimal threshold via Youden's J statistic (maximizes sensitivity +
#       specificity - 1) - the standard imbalance-aware threshold choice,
#       computed from the ROC curve rather than assumed at 0.5.
#   (2) Class-weighted Random Forest (classwt parameter) - penalizes
#       misclassifying the minority class more heavily DURING TRAINING,
#       a different fix than just moving the decision threshold post-hoc.
#
# PREREQUISITE: requires `test_scored`, `train_model_df`, `test_model_df`,
# and `rf_model` in your environment from Step 1.


library(tidyverse)
library(randomForest)
library(pROC)
library(caret)

stopifnot("rf_model not found - re-run Step 1 first" = exists("rf_model"))
stopifnot("test_scored not found - re-run Step 1 first" = exists("test_scored"))

cat(sprintf("Original label balance: %.1f%% Yes, %.1f%% No\n",
            100 * mean(test_scored$disability_label == "Yes"),
            100 * mean(test_scored$disability_label == "No")))


# CORRECTION 1: Optimal threshold via Youden's J (instead of naive 0.5)


roc_obj <- roc(test_scored$disability_label, test_scored$predicted_prob_yes,
               levels = c("No", "Yes"), direction = "<")

optimal_coords <- coords(roc_obj, "best", best.method = "youden",
                          ret = c("threshold", "sensitivity", "specificity"))

cat(sprintf("\n===== OPTIMAL THRESHOLD (Youden's J) =====\n"))
cat(sprintf("Naive threshold: 0.500\n"))
cat(sprintf("Optimal threshold: %.3f\n", optimal_coords$threshold))
cat(sprintf("At optimal threshold - Sensitivity: %.3f | Specificity: %.3f\n",
            optimal_coords$sensitivity, optimal_coords$specificity))

# Re-derive predicted labels at the optimal threshold instead of 0.5
test_scored_corrected <- test_scored %>%
  mutate(predicted_label_corrected = factor(
    ifelse(predicted_prob_yes > optimal_coords$threshold, "Yes", "No"),
    levels = c("No", "Yes")
  ))

cat("\n===== OVERALL CONFUSION MATRIX AT OPTIMAL THRESHOLD =====\n")
print(confusionMatrix(test_scored_corrected$predicted_label_corrected,
                       test_scored_corrected$disability_label, positive = "Yes"))

# --------------------------------------
# Subgroup sensitivity at the CORRECTED threshold - the key comparison
# --------------------------------------

subgroup_sensitivity_corrected <- function(df, group_var, label_col) {
  df %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      n = n(),
      n_true_yes = sum(disability_label == "Yes"),
      sensitivity = sum(.data[[label_col]] == "Yes" & disability_label == "Yes") /
                    pmax(sum(disability_label == "Yes"), 1),
      false_negative_rate = 1 - sensitivity,
      .groups = "drop"
    ) %>%
    arrange(desc(false_negative_rate))
}

cat("\n===== SUBGROUP SENSITIVITY AT OPTIMAL THRESHOLD: INCOME TIER =====\n")
income_corrected <- subgroup_sensitivity_corrected(test_scored_corrected, "income_tier",
                                                     "predicted_label_corrected")
print(income_corrected)

cat("\n===== SUBGROUP SENSITIVITY AT OPTIMAL THRESHOLD: RACE/ETHNICITY =====\n")
race_corrected <- subgroup_sensitivity_corrected(test_scored_corrected, "race_eth_label",
                                                   "predicted_label_corrected")
print(race_corrected)


# CORRECTION 2: Class-weighted Random Forest (penalize minority-class errors
# during training itself, not just post-hoc threshold adjustment)

# classwt is set INVERSELY proportional to class frequency - this tells
# randomForest to weight a missed "Yes" case more heavily than a missed "No"
# case during tree-building, addressing imbalance at the source rather than
# the symptom.

class_freq <- table(train_model_df$disability_label)
class_weights <- c(No = 1 / as.numeric(class_freq["No"]), Yes = 1 / as.numeric(class_freq["Yes"]))
class_weights <- class_weights / sum(class_weights) * 2
names(class_weights) <- c("No", "Yes")  # force clean names, just in case

cat(sprintf("\n===== CLASS-WEIGHTED MODEL =====\n"))
cat(sprintf("Class weights - No: %.4f | Yes: %.4f\n", class_weights["No"], class_weights["Yes"]))

rf_model_weighted <- randomForest(
  disability_label ~ .,
  data = train_model_df,
  ntree = 500,
  classwt = as.list(class_weights),
  importance = TRUE,
  na.action = na.omit
)

print(rf_model_weighted)

test_model_df$predicted_label_weighted <- predict(rf_model_weighted, newdata = test_model_df)

test_scored_weighted <- test_model_df %>%
  select(seqn, predicted_label_weighted) %>%
  left_join(test_scored, by = "seqn")

cat("\n===== OVERALL CONFUSION MATRIX: CLASS-WEIGHTED MODEL =====\n")
print(confusionMatrix(test_scored_weighted$predicted_label_weighted,
                       test_scored_weighted$disability_label, positive = "Yes"))

cat("\n===== SUBGROUP SENSITIVITY: CLASS-WEIGHTED MODEL, INCOME TIER =====\n")
income_weighted <- subgroup_sensitivity_corrected(test_scored_weighted, "income_tier",
                                                    "predicted_label_weighted")
print(income_weighted)

cat("\n===== SUBGROUP SENSITIVITY: CLASS-WEIGHTED MODEL, RACE/ETHNICITY =====\n")
race_weighted <- subgroup_sensitivity_corrected(test_scored_weighted, "race_eth_label",
                                                  "predicted_label_weighted")
print(race_weighted)


# SIDE-BY-SIDE: ORIGINAL (0.5 cutoff) vs OPTIMAL-THRESHOLD vs CLASS-WEIGHTED

# This is the key output: does the income/race FNR gap shrink, stay the same,
# or persist across all three correction approaches? If it persists, you have
# strong grounds to claim this is a signal-availability problem, not a naive-
# threshold artifact.

original_income <- subgroup_accuracy_simple <- test_scored %>%
  group_by(income_tier) %>%
  summarise(fnr_original = 1 - sum(predicted_label == "Yes" & disability_label == "Yes") /
                                pmax(sum(disability_label == "Yes"), 1), .groups = "drop")

comparison_income_thresholds <- original_income %>%
  left_join(income_corrected %>% select(income_tier, fnr_optimal_threshold = false_negative_rate),
            by = "income_tier") %>%
  left_join(income_weighted %>% select(income_tier, fnr_class_weighted = false_negative_rate),
            by = "income_tier")

cat("\n\n===== KEY COMPARISON: INCOME-TIER FNR ACROSS THREE CORRECTIONS =====\n")
print(comparison_income_thresholds)

original_race <- test_scored %>%
  group_by(race_eth_label) %>%
  summarise(fnr_original = 1 - sum(predicted_label == "Yes" & disability_label == "Yes") /
                                pmax(sum(disability_label == "Yes"), 1), .groups = "drop")

comparison_race_thresholds <- original_race %>%
  left_join(race_corrected %>% select(race_eth_label, fnr_optimal_threshold = false_negative_rate),
            by = "race_eth_label") %>%
  left_join(race_weighted %>% select(race_eth_label, fnr_class_weighted = false_negative_rate),
            by = "race_eth_label")

cat("\n===== KEY COMPARISON: RACE FNR ACROSS THREE CORRECTIONS =====\n")
print(comparison_race_thresholds)


# SAVE

write_csv(comparison_income_thresholds, "data/step3a_imbalance_check_income.csv")
write_csv(comparison_race_thresholds, "data/step3a_imbalance_check_race.csv")

cat("\n\nStep 3a complete. Files saved:\n")
cat("  - step3a_imbalance_check_income.csv\n")
cat("  - step3a_imbalance_check_race.csv\n")
