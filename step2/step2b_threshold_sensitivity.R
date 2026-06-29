
# STEP 2B: Threshold Sensitivity Check

# Purpose: Step 2's main finding (a strong NEGATIVE correlation between
# Rule A harm and Rule B harm across groups - r = -0.783 at the 25th/75th
# percentile thresholds) is a striking result. Before treating it as a real
# structural finding rather than an artifact of WHERE exactly the threshold
# was drawn, this script re-runs the same logic across a RANGE of threshold
# pairs and checks whether the inversion holds throughout.
#
# If the negative correlation persists across multiple threshold choices
# (not just 25/75), that's strong evidence the inversion is a genuine
# property of how error is distributed across groups in this model -
# not a coincidence of one specific cutoff choice.
#
# PREREQUISITE: requires `test_scored` in your environment (from Step 1).


library(tidyverse)

stopifnot("test_scored not found - re-source Step 1 or load from CSV" =
            exists("test_scored"))


# 1. DEFINE A RANGE OF THRESHOLD PAIRS TO TEST

# Each pair: (Rule A percentile - the "wide net" outreach cutoff, LOWER means
# wider net / more people flagged) and (Rule B percentile - the "narrow net"
# review cutoff, HIGHER means narrower net / fewer people flagged).
# We test from a mild contrast (40/60) to the original (25/75) to a more
# extreme contrast (10/90), to see if the inversion is specific to one
# choice or holds across the whole range.

threshold_pairs <- tibble(
  rule_a_percentile = c(0.40, 0.30, 0.25, 0.20, 0.10),
  rule_b_percentile = c(0.60, 0.70, 0.75, 0.80, 0.90)
)

cat("Testing threshold pairs:\n")
print(threshold_pairs)


# 2. FUNCTION: run the Step 2 logic for one threshold pair, return the
#    race-subgroup correlation (the headline summary statistic)


run_decision_rules_at_thresholds <- function(df, a_pct, b_pct) {
  
  q_a <- quantile(df$predicted_prob_yes, a_pct)
  q_b <- quantile(df$predicted_prob_yes, b_pct)
  
  decision_df <- df %>%
    mutate(
      flagged_outreach = predicted_prob_yes > q_a,
      flagged_review    = predicted_prob_yes > q_b,
      rule_a_missed_outreach = (disability_label == "Yes") & (flagged_outreach == FALSE),
      rule_b_wrongly_flagged = (disability_label == "No")  & (flagged_review  == TRUE)
    )
  
  rule_a_race <- decision_df %>%
    filter(disability_label == "Yes") %>%
    group_by(race_eth_label) %>%
    summarise(n_base = n(), harm_rate_a = mean(rule_a_missed_outreach), .groups = "drop")
  
  rule_b_race <- decision_df %>%
    filter(disability_label == "No") %>%
    group_by(race_eth_label) %>%
    summarise(n_base = n(), harm_rate_b = mean(rule_b_wrongly_flagged), .groups = "drop")
  
  combined <- rule_a_race %>%
    select(race_eth_label, harm_rate_a) %>%
    inner_join(rule_b_race %>% select(race_eth_label, harm_rate_b), by = "race_eth_label")
  
  race_corr <- cor(combined$harm_rate_a, combined$harm_rate_b, use = "complete.obs")
  
  # Also pull income-tier correlation, since the income pattern was your
  # cleanest result in the original run
  rule_a_income <- decision_df %>%
    filter(disability_label == "Yes") %>%
    group_by(income_tier) %>%
    summarise(n_base = n(), harm_rate_a = mean(rule_a_missed_outreach), .groups = "drop")
  
  rule_b_income <- decision_df %>%
    filter(disability_label == "No") %>%
    group_by(income_tier) %>%
    summarise(n_base = n(), harm_rate_b = mean(rule_b_wrongly_flagged), .groups = "drop")
  
  combined_income <- rule_a_income %>%
    select(income_tier, harm_rate_a) %>%
    inner_join(rule_b_income %>% select(income_tier, harm_rate_b), by = "income_tier")
  
  income_corr <- cor(combined_income$harm_rate_a, combined_income$harm_rate_b, use = "complete.obs")
  
  tibble(
    rule_a_pct = a_pct,
    rule_b_pct = b_pct,
    n_flagged_outreach = sum(decision_df$flagged_outreach),
    n_flagged_review    = sum(decision_df$flagged_review),
    race_correlation   = race_corr,
    income_correlation = income_corr
  )
}


# 3. RUN ACROSS ALL THRESHOLD PAIRS


sensitivity_results <- map2_dfr(
  threshold_pairs$rule_a_percentile,
  threshold_pairs$rule_b_percentile,
  ~ run_decision_rules_at_thresholds(test_scored, .x, .y)
)

cat("\n\n===== THRESHOLD SENSITIVITY RESULTS =====\n")
cat("If race_correlation and income_correlation stay consistently negative\n")
cat("across this whole range, the harm-inversion finding is robust to your\n")
cat("specific threshold choice - not an artifact of picking 25/75 exactly.\n\n")
print(sensitivity_results)


# 4. SAVE

write_csv(sensitivity_results, "data/step2b_threshold_sensitivity.csv")
cat("\nSaved: step2b_threshold_sensitivity.csv\n")

cat("\n\nHow to read this for your paper:\n")
cat("- If correlations are negative and roughly similar magnitude across all\n")
cat("  five threshold pairs, you can confidently claim the harm-inversion is a\n")
cat("  structural property of the model's error distribution, not a one-off\n")
cat("  result from arbitrarily choosing the 25th/75th percentiles.\n")
cat("- If correlations weaken substantially at milder thresholds (e.g. 40/60),\n")
cat("  that's still useful - it tells you the inversion is most pronounced when\n")
cat("  institutions set AGGRESSIVE thresholds in either direction, which is\n")
cat("  itself a real and reportable finding about when this harm pattern is\n")
cat("  worst.\n")