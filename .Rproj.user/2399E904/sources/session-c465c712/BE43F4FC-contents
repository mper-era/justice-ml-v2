
# STEP 4A: Calibration Check (by subgroup)

# Purpose: AUC/sensitivity/specificity measure DISCRIMINATION -- can the model
# rank-order people correctly. They say nothing about CALIBRATION -- when the
# model says "70% probability of disability," is that actually right ~70% of
# the time, separately for each subgroup? A model can have identical AUC
# across groups while being systematically miscalibrated for one of them
# (predicted probabilities too high or too low) -- and since Step 2's
# decision rules are threshold-based on predicted_prob_yes, miscalibration
# would mean the SAME percentile cutoff doesn't mean the same thing for
# every subgroup, undermining the comparability of Rule A/Rule B harm rates
# across groups.
#
# METHOD: bin predicted probabilities into deciles, compute the OBSERVED
# disability rate within each bin, separately per subgroup. A well-calibrated
# model has observed rate ≈ mean predicted probability within each bin. We
# also compute a single summary number (mean calibration error) per subgroup
# for an easy at-a-glance comparison.
#
# PREREQUISITE: requires `test_scored_pooled` (from step3d_refit_pooled.R).


library(tidyverse)

stopifnot("test_scored_pooled not found -- re-run step3d_refit_pooled.R first" =
            exists("test_scored_pooled"))

cat(sprintf("Loaded test_scored_pooled: %d rows\n", nrow(test_scored_pooled)))


# 1. OVERALL CALIBRATION (sanity check before splitting by subgroup)

calibration_table <- function(df, n_bins = 10) {
  df %>%
    mutate(prob_bin = ntile(predicted_prob_yes, n_bins)) %>%
    group_by(prob_bin) %>%
    summarise(
      n = n(),
      mean_predicted_prob = mean(predicted_prob_yes),
      observed_rate = mean(disability_label == "Yes"),
      calibration_gap = observed_rate - mean_predicted_prob,
      .groups = "drop"
    ) %>%
    arrange(prob_bin)
}

cat("\n===== OVERALL CALIBRATION (10 bins, all respondents) =====\n")
overall_calibration <- calibration_table(test_scored_pooled)
print(overall_calibration)

cat(sprintf("\nMean absolute calibration gap (overall): %.4f\n",
            mean(abs(overall_calibration$calibration_gap))))
cat("Interpretation: a gap near 0 in every bin means predicted probabilities\n")
cat("track observed rates well. Large positive gaps = model UNDER-predicts in\n")
cat("that range (observed rate higher than predicted). Large negative gaps =\n")
cat("model OVER-predicts.\n")


# 2. CALIBRATION BY SUBGROUP -- the key question: does a 0.5 predicted
#    probability mean the same thing for every race/income group?

# Using fewer bins (5, not 10) for subgroup-level calibration since smaller
# subgroups would have very few people per bin at 10 bins -- 5 bins keeps
# enough people per bin to give a stable observed-rate estimate even for the
# smaller race categories.

calibration_by_group <- function(df, group_var, n_bins = 5) {
  df %>%
    group_by(.data[[group_var]]) %>%
    mutate(prob_bin = ntile(predicted_prob_yes, n_bins)) %>%
    group_by(.data[[group_var]], prob_bin) %>%
    summarise(
      n = n(),
      mean_predicted_prob = mean(predicted_prob_yes),
      observed_rate = mean(disability_label == "Yes"),
      calibration_gap = observed_rate - mean_predicted_prob,
      .groups = "drop"
    )
}

cat("\n\n===== CALIBRATION BY RACE (5 bins per group) =====\n")
race_calibration <- calibration_by_group(test_scored_pooled, "race_eth_label_orig")
print(race_calibration, n = 50)

cat("\n===== CALIBRATION BY INCOME TIER (5 bins per group) =====\n")
income_calibration <- calibration_by_group(test_scored_pooled, "income_tier")
print(income_calibration, n = 50)


# 3. SUMMARY: mean absolute calibration error per subgroup

# This collapses each subgroup's 5-bin calibration table into ONE number --
# the easiest thing to report and compare across groups. Larger values mean
# predicted probabilities are less trustworthy for that subgroup, REGARDLESS
# of how good that subgroup's AUC/sensitivity looked in Step 1/3d.

cat("\n\n===== SUMMARY: MEAN ABSOLUTE CALIBRATION ERROR BY RACE =====\n")
race_cal_summary <- race_calibration %>%
  group_by(race_eth_label_orig) %>%
  summarise(
    total_n = sum(n),
    mean_abs_calibration_error = mean(abs(calibration_gap)),
    max_abs_calibration_error = max(abs(calibration_gap)),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_abs_calibration_error))
print(race_cal_summary)

cat("\n===== SUMMARY: MEAN ABSOLUTE CALIBRATION ERROR BY INCOME TIER =====\n")
income_cal_summary <- income_calibration %>%
  group_by(income_tier) %>%
  summarise(
    total_n = sum(n),
    mean_abs_calibration_error = mean(abs(calibration_gap)),
    max_abs_calibration_error = max(abs(calibration_gap)),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_abs_calibration_error))
print(income_cal_summary)

cat("\n\nHow to read this for your paper:\n")
cat("- If mean_abs_calibration_error is similar (e.g. all under ~0.05-0.08)\n")
cat("  across every subgroup, predicted probabilities are reasonably trust-\n")
cat("  worthy everywhere, and your Step 2 percentile thresholds are comparing\n")
cat("  groups on a fair common scale.\n")
cat("- If one subgroup (e.g. Non-Hispanic Asian, given Step 1's sensitivity\n")
cat("  gap) shows a MUCH larger calibration error than others, that means the\n")
cat("  model isn't just less SENSITIVE for that group -- its probability\n")
cat("  estimates are less MEANINGFUL for them, which is an additional, more\n")
cat("  fundamental problem than a sensitivity gap alone, and should be named\n")
cat("  explicitly as its own finding.\n")


# SAVE

write_csv(overall_calibration, "data/step4a_calibration_overall.csv")
write_csv(race_calibration, "data/step4a_calibration_by_race.csv")
write_csv(income_calibration, "data/step4a_calibration_by_income.csv")
write_csv(race_cal_summary, "data/step4a_calibration_summary_race.csv")
write_csv(income_cal_summary, "data/step4a_calibration_summary_income.csv")

cat("\n\nStep 4a complete. Files saved:\n")
cat("  - step4a_calibration_overall.csv\n")
cat("  - step4a_calibration_by_race.csv          (full 5-bin detail)\n")
cat("  - step4a_calibration_by_income.csv        (full 5-bin detail)\n")
cat("  - step4a_calibration_summary_race.csv     (ONE number per group -- start here)\n")
cat("  - step4a_calibration_summary_income.csv   (ONE number per group -- start here)\n")