
# STEP 3E: Decision-Rule Overlay on Pooled Data

# Re-runs Step 2's exact logic (outreach-prioritization vs. utilization-
# review-flagging, harm-rate cross-tabs, the harm-inversion correlation
# check) using the POOLED model's predictions (test_scored_pooled) instead
# of the single-cycle test_scored. Same design, same thresholds, now with
# the larger, statistically-validated sample behind it.
#
# PREREQUISITE: requires `test_scored_pooled` in your environment from
# step3d_refit_pooled.R. If you've closed RStudio since then, re-source
# step3d first (it will auto-load pooled_df from CSV, then refit).


library(tidyverse)

stopifnot("test_scored_pooled not found -- re-run step3d_refit_pooled.R first" =
            exists("test_scored_pooled"))

cat(sprintf("Loaded test_scored_pooled: %d rows\n", nrow(test_scored_pooled)))


# 1. DEFINE THE TWO DECISION RULES (same logic as Step 2)

q25 <- quantile(test_scored_pooled$predicted_prob_yes, 0.25)
q75 <- quantile(test_scored_pooled$predicted_prob_yes, 0.75)

cat(sprintf("\nThreshold reference points (pooled data):\n"))
cat(sprintf("  25th percentile: %.3f\n", q25))
cat(sprintf("  75th percentile: %.3f\n", q75))

decision_df <- test_scored_pooled %>%
  mutate(
    flagged_outreach = predicted_prob_yes > q25,   # Rule A: wide net
    flagged_review    = predicted_prob_yes > q75    # Rule B: narrow net
  )

cat(sprintf("\nRule A (outreach) flags %d / %d people (%.1f%%)\n",
            sum(decision_df$flagged_outreach), nrow(decision_df),
            100 * mean(decision_df$flagged_outreach)))
cat(sprintf("Rule B (review) flags %d / %d people (%.1f%%)\n",
            sum(decision_df$flagged_review), nrow(decision_df),
            100 * mean(decision_df$flagged_review)))


# 2. HARM-RELEVANT MISMATCH POPULATIONS

decision_df <- decision_df %>%
  mutate(
    rule_a_missed_outreach = (disability_label == "Yes") & (flagged_outreach == FALSE),
    rule_b_wrongly_flagged = (disability_label == "No")  & (flagged_review  == TRUE)
  )

n_true_disabled <- sum(decision_df$disability_label == "Yes")
n_true_nondisabled <- sum(decision_df$disability_label == "No")

cat(sprintf("\n===== RULE A: Outreach-Prioritization (pooled) =====\n"))
cat(sprintf("Of %d people who TRULY have a disability: %d (%.1f%%) missed\n",
            n_true_disabled, sum(decision_df$rule_a_missed_outreach),
            100 * sum(decision_df$rule_a_missed_outreach) / n_true_disabled))

cat(sprintf("\n===== RULE B: Utilization-Review-Flagging (pooled) =====\n"))
cat(sprintf("Of %d people who TRULY do NOT have a disability: %d (%.1f%%) wrongly flagged\n",
            n_true_nondisabled, sum(decision_df$rule_b_wrongly_flagged),
            100 * sum(decision_df$rule_b_wrongly_flagged) / n_true_nondisabled))


# 3. HARM RATE BY DEMOGRAPHIC SUBGROUP

harm_rate_by_group <- function(df, group_var, rule_col, base_filter_label) {
  df %>%
    filter(disability_label == base_filter_label) %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      n_in_base_population = n(),
      n_harmed = sum(.data[[rule_col]]),
      harm_rate = n_harmed / n_in_base_population,
      .groups = "drop"
    ) %>%
    arrange(desc(harm_rate))
}

cat("\n\n===== RULE A HARM RATE (missed outreach) BY RACE (pooled) =====\n")
rule_a_race <- harm_rate_by_group(decision_df, "race_eth_label_orig", "rule_a_missed_outreach", "Yes")
print(rule_a_race)

cat("\n===== RULE A HARM RATE BY INCOME TIER (pooled) =====\n")
rule_a_income <- harm_rate_by_group(decision_df, "income_tier", "rule_a_missed_outreach", "Yes")
print(rule_a_income)

cat("\n\n===== RULE B HARM RATE (wrongly flagged) BY RACE (pooled) =====\n")
rule_b_race <- harm_rate_by_group(decision_df, "race_eth_label_orig", "rule_b_wrongly_flagged", "No")
print(rule_b_race)

cat("\n===== RULE B HARM RATE BY INCOME TIER (pooled) =====\n")
rule_b_income <- harm_rate_by_group(decision_df, "income_tier", "rule_b_wrongly_flagged", "No")
print(rule_b_income)


# 4. SIDE-BY-SIDE COMPARISON: does harm flip depending on use?

cat("\n\n===== SIDE-BY-SIDE: RACE (pooled) =====\n")
comparison_race <- rule_a_race %>%
  rename(rule_a_harm_rate = harm_rate, rule_a_n = n_in_base_population) %>%
  full_join(rule_b_race %>% rename(rule_b_harm_rate = harm_rate, rule_b_n = n_in_base_population),
            by = "race_eth_label_orig") %>%
  select(race_eth_label_orig, rule_a_harm_rate, rule_b_harm_rate, rule_a_n, rule_b_n) %>%
  arrange(desc(rule_a_harm_rate))
print(comparison_race)

cat("\n===== SIDE-BY-SIDE: INCOME TIER (pooled) =====\n")
comparison_income <- rule_a_income %>%
  rename(rule_a_harm_rate = harm_rate, rule_a_n = n_in_base_population) %>%
  full_join(rule_b_income %>% rename(rule_b_harm_rate = harm_rate, rule_b_n = n_in_base_population),
            by = "income_tier") %>%
  select(income_tier, rule_a_harm_rate, rule_b_harm_rate, rule_a_n, rule_b_n) %>%
  arrange(desc(rule_a_harm_rate))
print(comparison_income)


# 5. CORRELATION CHECK: harm inversion, pooled data

cat("\n\n===== CORRELATION: Rule A vs Rule B harm rate, by race (pooled) =====\n")
race_corr_pooled <- cor(comparison_race$rule_a_harm_rate, comparison_race$rule_b_harm_rate,
                        use = "complete.obs")
cat(sprintf("Pearson correlation (race subgroups, pooled): r = %.3f\n", race_corr_pooled))

income_corr_pooled <- cor(comparison_income$rule_a_harm_rate, comparison_income$rule_b_harm_rate,
                          use = "complete.obs")
cat(sprintf("Pearson correlation (income tiers, pooled): r = %.3f\n", income_corr_pooled))

cat("\nCompare these r-values directly against your single-cycle Step 2 result\n")
cat("(r = -0.783 for race, strongly negative for income) to see if the harm-\n")
cat("inversion finding holds on the larger, pooled sample.\n")


# SAVE

write_csv(decision_df, "data/step3e_decision_rule_row_level_pooled.csv")
write_csv(comparison_race, "data/step3e_comparison_race_pooled.csv")
write_csv(comparison_income, "data/step3e_comparison_income_pooled.csv")

cat("\n\nStep 3e complete. Files saved:\n")
cat("  - step3e_decision_rule_row_level_pooled.csv\n")
cat("  - step3e_comparison_race_pooled.csv\n")
cat("  - step3e_comparison_income_pooled.csv\n")