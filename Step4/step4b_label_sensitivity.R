
# STEP 4B: Label-Construction Sensitivity Check

# Purpose: your label so far has been "disability_label = 1 if ANY of 6 DLQ
# domains = Yes". This is a defensible choice, but it's still A choice. This
# script checks whether the income/race sensitivity gaps are an artifact of
# that specific choice by testing two alternatives:
#
#   (1) STRICTER LABEL: require 2+ domains = Yes (more severe/unambiguous
#       disability), rather than just 1+. If the gap holds under a stricter
#       definition, that's stronger evidence it's not a labeling artifact.
#
#   (2) PER-DOMAIN BREAKDOWN: instead of one combined label, fit separate
#       models for EACH of the 6 domains individually (hearing, vision,
#       concentration, mobility, self-care, errands). If the income/race
#       gap is driven by just ONE domain (e.g. mobility) rather than being
#       consistent across all 6, that's an important nuance - it would mean
#       some disability TYPES are far less legible to your available
#       correlates than others, which is itself a citable, specific finding.
#
# PREREQUISITE: requires `pooled_df` in your environment (auto-reloads from
# CSV if missing, same pattern as prior scripts). Note: the saved pooled CSV
# only contains the COMBINED label, not the original 6 domain columns, so
# this script re-pulls DLQ directly to reconstruct per-domain labels.


library(nhanesA)
library(tidyverse)
library(randomForest)
library(caret)
library(janitor)

if (!exists("pooled_df")) {
  cat("pooled_df not found - loading from saved CSV\n")
  pooled_df <- read_csv("step3c_pooled_analytic_df.csv", show_col_types = FALSE) %>%
    mutate(
      sex = factor(sex), education = factor(education), marital_status = factor(marital_status),
      work_status = factor(work_status), has_insurance = factor(has_insurance),
      has_usual_healthcare_place = factor(has_usual_healthcare_place),
      uses_special_equipment = factor(uses_special_equipment),
      general_health_rating = factor(general_health_rating),
      n_healthcare_visits_12mo = factor(n_healthcare_visits_12mo,
                                        levels = c("None", "1", "2 to 3", "4 to 5", "6 to 7", "8 to 9",
                                                   "10 to 12", "13 to 15", "16 or more", "Missing"),
                                        ordered = TRUE)
    )
}

cat(sprintf("Pooled data loaded: %d rows\n", nrow(pooled_df)))


# 1. RE-PULL DLQ PER CYCLE TO RECONSTRUCT PER-DOMAIN LABELS

# The pooled CSV only kept the combined label - domain-level detail wasn't
# saved. Re-pulling DLQ is fast (7 columns, 3 cycles) compared to re-pulling
# everything, so this is cheap to redo rather than re-engineering Step 3c.

cycles_to_pool <- c(H = "2013-2014", I = "2015-2016", J = "2017-2018")

pull_dlq_domains <- function(suffix) {
  dlq <- nhanes(paste0("DLQ_", suffix))
  dlq %>%
    clean_names() %>%
    select(seqn, dlq010, dlq020, dlq040, dlq050, dlq060, dlq080) %>%
    mutate(across(c(dlq010, dlq020, dlq040, dlq050, dlq060, dlq080), as.character)) %>%
    mutate(across(c(dlq010, dlq020, dlq040, dlq050, dlq060, dlq080),
                  ~ ifelse(.x %in% c("Refused", "Don't know"), NA, .x))) %>%
    mutate(across(c(dlq010, dlq020, dlq040, dlq050, dlq060, dlq080),
                  ~ ifelse(.x == "Yes", 1, ifelse(.x == "No", 0, NA)))) %>%
    rename(
      hearing_difficulty       = dlq010,
      vision_difficulty        = dlq020,
      concentration_difficulty = dlq040,
      mobility_difficulty      = dlq050,
      selfcare_difficulty      = dlq060,
      errands_difficulty       = dlq080
    ) %>%
    mutate(cycle = suffix)
}

cat("\nRe-pulling DLQ domain detail across cycles...\n")
domain_data <- map_dfr(names(cycles_to_pool), pull_dlq_domains)
cat(sprintf("Domain data pulled: %d rows\n", nrow(domain_data)))

# Merge domain detail onto the existing pooled_df (which has the features
# and combined label already built)
pooled_with_domains <- pooled_df %>%
  left_join(domain_data %>% select(-cycle), by = "seqn")

n_domains_summed <- pooled_with_domains %>%
  rowwise() %>%
  mutate(n_domains_yes = sum(c(hearing_difficulty, vision_difficulty, concentration_difficulty,
                               mobility_difficulty, selfcare_difficulty, errands_difficulty),
                             na.rm = TRUE)) %>%
  ungroup() %>%
  pull(n_domains_yes)

pooled_with_domains$n_domains_yes <- n_domains_summed

cat("\nDistribution of number of domains affected (among those with disability_label == Yes):\n")
print(table(pooled_with_domains$n_domains_yes[pooled_with_domains$disability_label == "Yes"]))


# 2. STRICTER LABEL: 2+ domains required

pooled_strict <- pooled_with_domains %>%
  mutate(disability_label_strict = factor(ifelse(n_domains_yes >= 2, "Yes", "No"), levels = c("No", "Yes")))

cat(sprintf("\nStrict label (2+ domains) balance: %.1f%% Yes (vs. %.1f%% Yes under original 1+ domain label)\n",
            100 * mean(pooled_strict$disability_label_strict == "Yes"),
            100 * mean(pooled_df$disability_label == "Yes")))

feature_cols <- c("sex", "age", "race_eth_label", "education", "marital_status",
                  "poverty_ratio", "household_size", "uses_special_equipment",
                  "chronic_condition_count", "has_insurance", "general_health_rating",
                  "health_worse_than_last_yr", "has_usual_healthcare_place",
                  "n_healthcare_visits_12mo", "work_status")

set.seed(42)
pooled_strict <- pooled_strict %>% mutate(strata_var = paste(disability_label_strict, race_eth_label))
train_idx <- createDataPartition(pooled_strict$strata_var, p = 0.75, list = FALSE)
train_strict <- pooled_strict[train_idx, ]
test_strict  <- pooled_strict[-train_idx, ]

train_model_strict <- train_strict %>% select(disability_label_strict, all_of(feature_cols)) %>%
  rename(disability_label = disability_label_strict) %>%
  mutate(race_eth_label = factor(race_eth_label), work_status = factor(work_status))

test_model_strict <- test_strict %>% select(disability_label_strict, all_of(feature_cols), seqn) %>%
  rename(disability_label = disability_label_strict) %>%
  mutate(
    race_eth_label = factor(race_eth_label, levels = levels(train_model_strict$race_eth_label)),
    work_status = factor(work_status, levels = levels(train_model_strict$work_status)),
    general_health_rating = factor(general_health_rating, levels = levels(train_model_strict$general_health_rating)),
    n_healthcare_visits_12mo = factor(n_healthcare_visits_12mo,
                                      levels = levels(train_model_strict$n_healthcare_visits_12mo), ordered = TRUE)
  )

cat("\nFitting Random Forest on STRICT (2+ domain) label...\n")
rf_strict <- randomForest(disability_label ~ ., data = train_model_strict, ntree = 500, na.action = na.omit)
print(rf_strict)

test_model_strict$predicted_label <- predict(rf_strict, newdata = test_model_strict)
test_scored_strict <- test_model_strict %>%
  left_join(test_strict %>% select(seqn, race_eth_label, income_tier), by = "seqn", suffix = c("", "_orig"))

subgroup_fnr <- function(df, group_var) {
  df %>%
    group_by(.data[[group_var]]) %>%
    summarise(n = n(),
              false_negative_rate = 1 - sum(predicted_label == "Yes" & disability_label == "Yes") /
                pmax(sum(disability_label == "Yes"), 1),
              .groups = "drop") %>%
    arrange(desc(false_negative_rate))
}

cat("\n===== STRICT LABEL (2+ domains): FNR BY INCOME TIER =====\n")
income_strict <- subgroup_fnr(test_scored_strict, "income_tier")
print(income_strict)

cat("\n===== STRICT LABEL (2+ domains): FNR BY RACE =====\n")
race_strict <- subgroup_fnr(test_scored_strict, "race_eth_label_orig")
print(race_strict)


# 3. PER-DOMAIN BREAKDOWN: fit separate models for each of the 6 domains

cat("\n\n-- Per-domain models (this fits 6 separate Random Forests, will take a few minutes) --\n")

domain_cols <- c("hearing_difficulty", "vision_difficulty", "concentration_difficulty",
                 "mobility_difficulty", "selfcare_difficulty", "errands_difficulty")

run_domain_model <- function(domain_col) {
  cat(sprintf("\n  Fitting model for domain: %s\n", domain_col))
  
  domain_df <- pooled_with_domains %>%
    filter(!is.na(.data[[domain_col]])) %>%
    mutate(domain_label = factor(.data[[domain_col]], labels = c("No", "Yes")))
  
  domain_df <- domain_df %>% mutate(strata_var = paste(domain_label, race_eth_label))
  idx <- createDataPartition(domain_df$strata_var, p = 0.75, list = FALSE)
  train_d <- domain_df[idx, ]
  test_d  <- domain_df[-idx, ]
  
  train_model_d <- train_d %>% select(domain_label, all_of(feature_cols)) %>%
    mutate(race_eth_label = factor(race_eth_label), work_status = factor(work_status))
  
  test_model_d <- test_d %>% select(domain_label, all_of(feature_cols), seqn) %>%
    mutate(
      race_eth_label = factor(race_eth_label, levels = levels(train_model_d$race_eth_label)),
      work_status = factor(work_status, levels = levels(train_model_d$work_status)),
      general_health_rating = factor(general_health_rating, levels = levels(train_model_d$general_health_rating)),
      n_healthcare_visits_12mo = factor(n_healthcare_visits_12mo,
                                        levels = levels(train_model_d$n_healthcare_visits_12mo), ordered = TRUE)
    )
  
  rf_d <- randomForest(domain_label ~ ., data = train_model_d, ntree = 300, na.action = na.omit)
  test_model_d$predicted_label <- predict(rf_d, newdata = test_model_d)
  
  test_scored_d <- test_model_d %>%
    left_join(test_d %>% select(seqn, race_eth_label, income_tier), by = "seqn", suffix = c("", "_orig"))
  
  income_fnr_d <- subgroup_fnr(test_scored_d, "income_tier") %>% mutate(domain = domain_col)
  race_fnr_d   <- subgroup_fnr(test_scored_d, "race_eth_label_orig") %>% mutate(domain = domain_col)
  
  list(income = income_fnr_d, race = race_fnr_d)
}

domain_results <- map(domain_cols, run_domain_model)
names(domain_results) <- domain_cols

all_domain_income <- map_dfr(domain_results, ~ .x$income)
all_domain_race    <- map_dfr(domain_results, ~ .x$race)

cat("\n\n===== PER-DOMAIN FNR BY INCOME TIER (all 6 domains) =====\n")
print(all_domain_income %>% select(domain, income_tier, n, false_negative_rate), n = 30)

cat("\n===== PER-DOMAIN FNR BY RACE (all 6 domains) =====\n")
print(all_domain_race %>% select(domain, race_eth_label_orig, n, false_negative_rate), n = 40)


# SAVE

write_csv(income_strict, "step4b_strict_label_income.csv")
write_csv(race_strict, "step4b_strict_label_race.csv")
write_csv(all_domain_income, "step4b_per_domain_income.csv")
write_csv(all_domain_race, "step4b_per_domain_race.csv")

cat("\n\nStep 4b complete. Files saved:\n")
cat("  - step4b_strict_label_income.csv / _race.csv   (2+ domain definition)\n")
cat("  - step4b_per_domain_income.csv / _race.csv     (each of 6 domains separately)\n")