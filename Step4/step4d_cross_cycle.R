
# STEP 4D: Cross-Cycle Generalization Check

# Purpose: Step 3d's train/test split was a RANDOM split across the pooled
# H+I+J data -- meaning the model could see people from the SAME cycle as
# the test set during training. This doesn't actually test whether the model
# generalizes across TIME; it just tests generalization across a shuffled
# sample of the same three years combined.
#
# This script runs the stricter, more honest version: train on cycles H+I
# (2013-2014, 2015-2016) ONLY, then test on cycle J (2017-2018) ENTIRELY
# held out -- a respondent in J was NEVER seen during training, regardless
# of how similar they might be to someone in H or I. This tests whether the
# income/race sensitivity gaps are a stable property of the underlying
# relationships, or whether they were partly an artifact of pooling/training
# on the same years being predicted.
#
# PREREQUISITE: requires `pooled_df` in your environment (from step3c, or
# reload via step3d's Section 0 fallback).


library(tidyverse)
library(randomForest)
library(caret)
library(pROC)

if (!exists("pooled_df")) {
  cat("pooled_df not found -- loading from saved CSV\n")
  pooled_df <- read_csv("data/step3c_pooled_analytic_df.csv", show_col_types = FALSE) %>%
    mutate(
      sex = factor(sex), education = factor(education), marital_status = factor(marital_status),
      work_status = factor(work_status), has_insurance = factor(has_insurance),
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

cat(sprintf("Pooled data loaded: %d rows across cycles %s\n",
            nrow(pooled_df), paste(unique(pooled_df$cycle), collapse = ", ")))


# 1. SPLIT BY CYCLE, NOT RANDOMLY: train on H+I, test ENTIRELY on J

train_df_temporal <- pooled_df %>% filter(cycle %in% c("H", "I"))
test_df_temporal  <- pooled_df %>% filter(cycle == "J")

cat(sprintf("\nTemporal split -- Train (H+I): %d rows | Test (J only, fully held out): %d rows\n",
            nrow(train_df_temporal), nrow(test_df_temporal)))

cat("\nLabel balance check (confirms the known cycle drift from Step 3c):\n")
cat(sprintf("  Train (H+I) disability rate: %.1f%%\n",
            100 * mean(train_df_temporal$disability_label == "Yes")))
cat(sprintf("  Test (J) disability rate: %.1f%%\n",
            100 * mean(test_df_temporal$disability_label == "Yes")))
cat("Note: if these rates differ notably, that's the cycle-drift effect\n")
cat("already observed in Step 3c -- it makes this a GENUINELY harder test\n")
cat("than the random split, since the model trains on one label distribution\n")
cat("and is tested on a different one, exactly as a real deployed model\n")
cat("would face in practice (trained on past data, applied to future data).\n")


# 2. FIT MODEL ON H+I ONLY

feature_cols <- c("sex", "age", "race_eth_label", "education", "marital_status",
                  "poverty_ratio", "household_size", "uses_special_equipment",
                  "chronic_condition_count", "has_insurance", "general_health_rating",
                  "health_worse_than_last_yr", "has_usual_healthcare_place",
                  "n_healthcare_visits_12mo", "work_status")

train_model_temporal <- train_df_temporal %>% select(disability_label, all_of(feature_cols)) %>%
  mutate(race_eth_label = factor(race_eth_label), work_status = factor(work_status))

test_model_temporal <- test_df_temporal %>% select(disability_label, all_of(feature_cols), seqn) %>%
  mutate(
    race_eth_label = factor(race_eth_label, levels = levels(train_model_temporal$race_eth_label)),
    work_status = factor(work_status, levels = levels(train_model_temporal$work_status)),
    general_health_rating = factor(general_health_rating, levels = levels(train_model_temporal$general_health_rating)),
    n_healthcare_visits_12mo = factor(n_healthcare_visits_12mo,
                                      levels = levels(train_model_temporal$n_healthcare_visits_12mo), ordered = TRUE)
  )

cat("\nFitting Random Forest on H+I only (testing on fully held-out J)...\n")
set.seed(42)
rf_temporal <- randomForest(
  disability_label ~ ., data = train_model_temporal,
  ntree = 500, importance = TRUE, na.action = na.omit
)
print(rf_temporal)

test_model_temporal$predicted_label <- predict(rf_temporal, newdata = test_model_temporal)
test_model_temporal$predicted_prob_yes <- predict(rf_temporal, newdata = test_model_temporal, type = "prob")[, "Yes"]

test_scored_temporal <- test_model_temporal %>%
  left_join(test_df_temporal %>% select(seqn, race_eth_label, income_tier, education),
            by = "seqn", suffix = c("", "_orig"))


# 3. OVERALL PERFORMANCE: train-on-past, test-on-future

cat("\n===== OVERALL PERFORMANCE: TRAINED ON H+I, TESTED ON J (held out) =====\n")
print(confusionMatrix(test_scored_temporal$predicted_label, test_scored_temporal$disability_label, positive = "Yes"))

roc_temporal <- roc(test_scored_temporal$disability_label, test_scored_temporal$predicted_prob_yes,
                    levels = c("No", "Yes"), direction = "<")
cat(sprintf("\nAUC (trained H+I, tested on J): %.3f\n", auc(roc_temporal)))
cat("Compare this directly against Step 3d's pooled-random-split AUC (0.852)\n")
cat("-- a notable drop here would indicate the model partly relies on cycle-\n")
cat("specific patterns rather than stable underlying relationships.\n")


# 4. SUBGROUP RESULTS UNDER THE STRICTER TEMPORAL SPLIT

subgroup_accuracy <- function(df, group_var) {
  df %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      n = n(),
      sensitivity = sum(predicted_label == "Yes" & disability_label == "Yes") / pmax(sum(disability_label == "Yes"), 1),
      specificity = sum(predicted_label == "No" & disability_label == "No") / pmax(sum(disability_label == "No"), 1),
      false_negative_rate = 1 - sensitivity,
      false_positive_rate = 1 - specificity,
      .groups = "drop"
    ) %>%
    arrange(desc(false_negative_rate))
}

cat("\n===== SUBGROUP (TEMPORAL SPLIT): RACE/ETHNICITY =====\n")
race_temporal <- subgroup_accuracy(test_scored_temporal, "race_eth_label_orig")
print(race_temporal)

cat("\n===== SUBGROUP (TEMPORAL SPLIT): INCOME TIER =====\n")
income_temporal <- subgroup_accuracy(test_scored_temporal, "income_tier")
print(income_temporal)


# 5. DIRECT COMPARISON: random-split (Step 3d) vs. temporal-split (this script)

# NOTE: paste in Step 3d's race/income FNR numbers manually below if you want
# an automatic side-by-side table -- left as a manual step since those live
# in a different script's environment. The printed tables above are designed
# to be visually compared against step3d_subgroup_race_pooled.csv /
# step3d_subgroup_income_pooled.csv directly.

cat("\n\nHow to read this for your paper:\n")
cat("- If the income/race FNR gradient (richest/Asian respondents worst) is\n")
cat("  STILL PRESENT here, under the strictest possible test (no cycle overlap\n")
cat("  between train and test at all), that's your strongest possible general-\n")
cat("  ization claim -- the pattern isn't a property of random-split pooling,\n")
cat("  it holds when predicting genuinely unseen future respondents.\n")
cat("- If overall AUC drops notably vs. Step 3d's 0.852, report that honestly --\n")
cat("  it would mean some performance was inflated by cycle-specific patterns,\n")
cat("  even if the SUBGROUP GAP itself still holds in relative terms.\n")


# SAVE

write_csv(test_scored_temporal, "data/step4d_test_predictions_temporal.csv")
write_csv(race_temporal, "data/step4d_subgroup_race_temporal.csv")
write_csv(income_temporal, "data/step4d_subgroup_income_temporal.csv")

cat("\n\nStep 4d complete. Files saved:\n")
cat("  - step4d_test_predictions_temporal.csv\n")
cat("  - step4d_subgroup_race_temporal.csv\n")
cat("  - step4d_subgroup_income_temporal.csv\n")