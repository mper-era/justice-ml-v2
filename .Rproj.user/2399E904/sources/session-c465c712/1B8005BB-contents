
# STEP 2: Decision-Rule Overlay
# Two-Axis Epistemic Reliability Framework — The Justice Payoff

# Design:
#   Takes the validated Step 1 model's predictions (test_scored, with
#   predicted_prob_yes and true disability_label) and applies TWO contrasting
#   decision rules that mirror real institutional uses of a disability proxy:
#
#   RULE A -- Outreach-prioritization (barrier-removing)
#     Logic: cast a wide net; missing someone who needs help is the bigger
#     harm. LOW threshold (flag top 75% by predicted probability, i.e.
#     anyone above the 25th percentile). The harm of concern: FALSE
#     NEGATIVES -- people with real disability who never get flagged for
#     outreach/support and fall through the cracks.
#
#   RULE B -- Utilization-review-flagging (barrier-imposing)
#     Logic: only flag people the model is quite confident about; wrongly
#     flagging someone for review is itself a burden. HIGH threshold (flag
#     only top 25% by predicted probability, i.e. above the 75th
#     percentile). The harm of concern: FALSE POSITIVES -- people without
#     a disability (or unfairly suspected) who get dragged into burdensome
#     review.
#
#   For each rule, we identify the MISMATCH POPULATION (people the proxy
#   gets wrong in the harm-relevant direction) and cross-tabulate against
#   race, income tier, and education -- this answers: does proxy error
#   concentrate on the same populations under both directions of use, or
#   does the harm shift depending on what the proxy is used for?
#
# PREREQUISITE: requires `test_scored` in your environment, produced by
# nhanes_disability_step1.R. If you've closed RStudio since then, re-source
# that script first (or load test_scored from the saved CSV -- see Section 0).


library(tidyverse)
library(janitor)


# 0. LOAD test_scored IF NOT ALREADY IN MEMORY

# Uncomment if you're starting a fresh R session rather than continuing
# directly from Step 1:
#
# test_scored <- read_csv("data/step1_test_predictions_scored.csv") %>%
#   mutate(disability_label = factor(disability_label, levels = c("No", "Yes")),
#          predicted_label   = factor(predicted_label, levels = c("No", "Yes")))

stopifnot("test_scored not found -- re-source Step 1 or load from CSV (see Section 0)" =
            exists("test_scored"))
stopifnot("predicted_prob_yes column missing from test_scored" =
            "predicted_prob_yes" %in% names(test_scored))

cat(sprintf("Loaded test_scored: %d rows\n", nrow(test_scored)))


# 1. DEFINE THE TWO DECISION RULES

# Thresholds are set as PERCENTILES of the predicted probability distribution
# in this test set, rather than fixed probability cutoffs (e.g. "0.5"). This
# is deliberate: it mirrors how a real institution with a fixed administrative
# capacity (e.g. "we can only review the top 25% most-flagged claims this
# quarter") would actually set a threshold, rather than an arbitrary absolute
# cutoff. Report both the percentile AND the resulting probability cutoff in
# your writeup, since the latter is what a reader will want to sanity-check.

q25 <- quantile(test_scored$predicted_prob_yes, 0.25)
q75 <- quantile(test_scored$predicted_prob_yes, 0.75)

cat(sprintf("\nThreshold reference points:\n"))
cat(sprintf("  25th percentile of predicted probability: %.3f\n", q25))
cat(sprintf("  75th percentile of predicted probability: %.3f\n", q75))

decision_df <- test_scored %>%
  mutate(
    # RULE A: Outreach-prioritization -- flag anyone ABOVE the 25th percentile
    # (i.e. exclude only the bottom quartile of predicted probability).
    # Wide net, low bar to be flagged for outreach.
    flagged_outreach = predicted_prob_yes > q25,
    
    # RULE B: Utilization-review-flagging -- flag only the TOP quartile
    # (above the 75th percentile). Narrow net, high bar to be flagged for review.
    flagged_review = predicted_prob_yes > q75
  )

cat(sprintf("\nRule A (outreach) flags %d / %d people (%.1f%%)\n",
            sum(decision_df$flagged_outreach), nrow(decision_df),
            100 * mean(decision_df$flagged_outreach)))
cat(sprintf("Rule B (review) flags %d / %d people (%.1f%%)\n",
            sum(decision_df$flagged_review), nrow(decision_df),
            100 * mean(decision_df$flagged_review)))


# 2. IDENTIFY THE HARM-RELEVANT MISMATCH POPULATION FOR EACH RULE

# RULE A's harm of concern: FALSE NEGATIVES under the outreach rule --
#   true disability_label == "Yes" but flagged_outreach == FALSE.
#   These are people who needed outreach and didn't get it.
#
# RULE B's harm of concern: FALSE POSITIVES under the review rule --
#   true disability_label == "No" but flagged_review == TRUE.
#   These are people wrongly dragged into burdensome review.

decision_df <- decision_df %>%
  mutate(
    rule_a_missed_outreach = (disability_label == "Yes") & (flagged_outreach == FALSE),
    rule_b_wrongly_flagged = (disability_label == "No")  & (flagged_review  == TRUE)
  )

n_true_disabled <- sum(decision_df$disability_label == "Yes")
n_true_nondisabled <- sum(decision_df$disability_label == "No")

cat(sprintf("\n===== RULE A: Outreach-Prioritization =====\n"))
cat(sprintf("Of %d people who TRULY have a disability:\n", n_true_disabled))
cat(sprintf("  %d (%.1f%%) were missed by the outreach flag -- the harm population\n",
            sum(decision_df$rule_a_missed_outreach),
            100 * sum(decision_df$rule_a_missed_outreach) / n_true_disabled))

cat(sprintf("\n===== RULE B: Utilization-Review-Flagging =====\n"))
cat(sprintf("Of %d people who TRULY do NOT have a disability:\n", n_true_nondisabled))
cat(sprintf("  %d (%.1f%%) were wrongly flagged for review -- the harm population\n",
            sum(decision_df$rule_b_wrongly_flagged),
            100 * sum(decision_df$rule_b_wrongly_flagged) / n_true_nondisabled))


# 3. CROSS-TABULATE EACH HARM POPULATION AGAINST DEMOGRAPHICS

# This is the core justice output: for each rule, what fraction of EACH
# demographic subgroup falls into the harm population? Reported as a rate
# WITHIN each subgroup's relevant base population (true disabled for Rule A,
# true non-disabled for Rule B), so groups of different sizes are comparable.

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

cat("\n\n===== RULE A HARM RATE (missed outreach) BY RACE =====\n")
cat("Base population: people who TRULY have a disability. harm_rate = fraction\n")
cat("of that subgroup who needed outreach and didn't get flagged.\n\n")
rule_a_race <- harm_rate_by_group(decision_df, "race_eth_label", "rule_a_missed_outreach", "Yes")
print(rule_a_race)

cat("\n===== RULE A HARM RATE (missed outreach) BY INCOME TIER =====\n\n")
rule_a_income <- harm_rate_by_group(decision_df, "income_tier", "rule_a_missed_outreach", "Yes")
print(rule_a_income)

cat("\n===== RULE A HARM RATE (missed outreach) BY EDUCATION =====\n\n")
rule_a_education <- harm_rate_by_group(decision_df, "education_orig", "rule_a_missed_outreach", "Yes")
print(rule_a_education)

cat("\n\n===== RULE B HARM RATE (wrongly flagged for review) BY RACE =====\n")
cat("Base population: people who TRULY do NOT have a disability. harm_rate =\n")
cat("fraction of that subgroup wrongly flagged for burdensome review.\n\n")
rule_b_race <- harm_rate_by_group(decision_df, "race_eth_label", "rule_b_wrongly_flagged", "No")
print(rule_b_race)

cat("\n===== RULE B HARM RATE (wrongly flagged) BY INCOME TIER =====\n\n")
rule_b_income <- harm_rate_by_group(decision_df, "income_tier", "rule_b_wrongly_flagged", "No")
print(rule_b_income)

cat("\n===== RULE B HARM RATE (wrongly flagged) BY EDUCATION =====\n\n")
rule_b_education <- harm_rate_by_group(decision_df, "education_orig", "rule_b_wrongly_flagged", "No")
print(rule_b_education)


# 4. THE KEY COMPARISON: DOES HARM CONCENTRATE ON THE SAME GROUPS UNDER
#    BOTH RULES, OR DOES IT FLIP DEPENDING ON DIRECTION OF USE?

# Joins Rule A and Rule B harm rates side by side per group, so you can see
# directly whether (e.g.) the group most harmed by under-outreach is ALSO
# the group most harmed by over-flagging, or whether these are different
# populations entirely -- this is the central empirical question of Step 2.

cat("\n\n===== SIDE-BY-SIDE COMPARISON: RACE =====\n")
comparison_race <- rule_a_race %>%
  rename(rule_a_harm_rate = harm_rate, rule_a_n = n_in_base_population, rule_a_harmed = n_harmed) %>%
  full_join(
    rule_b_race %>% rename(rule_b_harm_rate = harm_rate, rule_b_n = n_in_base_population, rule_b_harmed = n_harmed),
    by = "race_eth_label"
  ) %>%
  select(race_eth_label, rule_a_harm_rate, rule_b_harm_rate, rule_a_n, rule_b_n) %>%
  arrange(desc(rule_a_harm_rate))
print(comparison_race)

cat("\n===== SIDE-BY-SIDE COMPARISON: INCOME TIER =====\n")
comparison_income <- rule_a_income %>%
  rename(rule_a_harm_rate = harm_rate, rule_a_n = n_in_base_population, rule_a_harmed = n_harmed) %>%
  full_join(
    rule_b_income %>% rename(rule_b_harm_rate = harm_rate, rule_b_n = n_in_base_population, rule_b_harmed = n_harmed),
    by = "income_tier"
  ) %>%
  select(income_tier, rule_a_harm_rate, rule_b_harm_rate, rule_a_n, rule_b_n) %>%
  arrange(desc(rule_a_harm_rate))
print(comparison_income)

cat("\n===== SIDE-BY-SIDE COMPARISON: EDUCATION =====\n")
comparison_education <- rule_a_education %>%
  rename(rule_a_harm_rate = harm_rate, rule_a_n = n_in_base_population, rule_a_harmed = n_harmed) %>%
  full_join(
    rule_b_education %>% rename(rule_b_harm_rate = harm_rate, rule_b_n = n_in_base_population, rule_b_harmed = n_harmed),
    by = "education_orig"
  ) %>%
  select(education_orig, rule_a_harm_rate, rule_b_harm_rate, rule_a_n, rule_b_n) %>%
  arrange(desc(rule_a_harm_rate))
print(comparison_education)


# 5. SIMPLE CORRELATION CHECK: do groups badly harmed by Rule A tend to also
#    be badly harmed by Rule B, or is the relationship weak/inverse?

# A positive correlation = harm concentrates on the same groups regardless of
# institutional purpose (the proxy is just bad for them, full stop).
# A weak/negative correlation = harm DEPENDS on what the proxy is used for --
# a group well-served by one use case could be poorly served by the other.

cat("\n\n===== CORRELATION: Rule A harm rate vs. Rule B harm rate, by group =====\n")
cat("(Across race subgroups -- income/education have too few groups for a\n")
cat(" meaningful correlation, but the side-by-side tables above let you eyeball it.)\n\n")

race_corr <- cor(comparison_race$rule_a_harm_rate, comparison_race$rule_b_harm_rate,
                 use = "complete.obs")
cat(sprintf("Pearson correlation (race subgroups): r = %.3f\n", race_corr))
cat("Interpretation guide:\n")
cat("  r close to +1: harm concentrates on the same groups under both rules\n")
cat("  r close to 0 or negative: harm shifts to DIFFERENT groups depending on\n")
cat("    how the proxy is used -- a group protected under one use case may be\n")
cat("    exposed under the other\n")


# 6. SAVE ALL OUTPUTS

write_csv(decision_df, "data/step2_decision_rule_row_level.csv")
write_csv(comparison_race, "data/step2_comparison_race.csv")
write_csv(comparison_income, "data/step2_comparison_income.csv")
write_csv(comparison_education, "data/step2_comparison_education.csv")

cat("\n\nStep 2 complete. Files saved:\n")
cat("  - step2_decision_rule_row_level.csv   (every person, both rules, both harm flags)\n")
cat("  - step2_comparison_race.csv           (Rule A vs Rule B harm rate by race)\n")
cat("  - step2_comparison_income.csv         (Rule A vs Rule B harm rate by income)\n")
cat("  - step2_comparison_education.csv      (Rule A vs Rule B harm rate by education)\n")
cat("\nThe race/income/education comparison tables are your core Step 2 result --\n")
cat("they show concretely who is helped or harmed by each institutional use of\n")
cat("this proxy, and whether that harm shifts depending on purpose.\n")