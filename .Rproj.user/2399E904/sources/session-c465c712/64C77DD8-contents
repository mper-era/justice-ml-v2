
# STEP 1: NHANES Disability-Status Held-Out Prediction
# Two-Axis Epistemic Reliability Framework — Novel Worked Example

# Design:
#   - Ground truth: NHANES 2017-2018 Disability Questionnaire (DLQ_J)
#     collapsed into a binary "has >=1 disability domain" label.
#   - Features: indirect correlates ONLY (demographics, chronic conditions,
#     healthcare access/utilization, employment, income). We deliberately
#     EXCLUDE the most circular PFQ items (direct walking/mobility limitation
#     questions) so the model is genuinely inferring disability status from
#     indirect signal, not reading a paraphrase of the answer.
#   - Output: subgroup-stratified accuracy / error rates by race, income
#     tier, and education -- this is your Axis 1 / Axis 2 evidence base,
#     and the input to Step 2's decision-rule overlay.


library(nhanesA)
library(tidyverse)
library(janitor)
library(randomForest)
library(caret)
library(survey)
library(pROC)

set.seed(42)  # reproducibility -- report this in your methods section


# 1. PULL RAW TABLES (2017-2018 cycle, suffix _J)

# NOTE: nhanesA scrapes CDC's site live each call. If a request times out,
# just re-run that one line -- no need to redo the whole script.
demo <- nhanes("DEMO_J")   # demographics + sample weights
dlq  <- nhanes("DLQ_J")    # disability questionnaire -- GROUND TRUTH lives here
pfq  <- nhanes("PFQ_J")    # physical functioning -- used CAREFULLY, see Section 3
mcq  <- nhanes("MCQ_J")    # medical conditions questionnaire
hiq  <- nhanes("HIQ_J")    # health insurance
huq  <- nhanes("HUQ_J")    # healthcare utilization/access
ocq  <- nhanes("OCQ_J")    # occupation/employment

# Quick sanity check on row counts before merging -- catch silent pull failures early
cat("Row counts as pulled:\n")
for (nm in c("demo","dlq","pfq","mcq","hiq","huq","ocq")) {
  cat(sprintf("  %-6s %d rows\n", nm, nrow(get(nm))))
}


# 2. BUILD THE LABEL (Y) -- the variable we will withhold and predict

# DLQ_J domains (per CDC documentation):
#   DLQ010 - serious difficulty hearing
#   DLQ020 - serious difficulty seeing (even with glasses)
#   DLQ040 - serious difficulty concentrating/remembering/making decisions
#   DLQ050 - serious difficulty walking/climbing stairs
#   DLQ060 - serious difficulty dressing/bathing
#   DLQ080 - difficulty doing errands alone (e.g. visiting a doctor, shopping)
# Coding: 1 = Yes, 2 = No, 7 = Refused, 9 = Don't know, NA = Missing

dlq_label <- dlq %>%
  clean_names() %>%
  select(seqn, dlq010, dlq020, dlq040, dlq050, dlq060, dlq080) %>%
  mutate(across(c(dlq010, dlq020, dlq040, dlq050, dlq060, dlq080),
                ~ ifelse(.x %in% c(7, 9), NA, .x))) %>%
  rowwise() %>%
  mutate(
    n_domains_answered = sum(!is.na(c(dlq010, dlq020, dlq040, dlq050, dlq060, dlq080))),
    n_domains_yes       = sum(c(dlq010, dlq020, dlq040, dlq050, dlq060, dlq080) == 1, na.rm = TRUE),
    disability_label     = case_when(
      n_domains_answered == 0 ~ NA_real_,
      n_domains_yes > 0       ~ 1,
      TRUE                    ~ 0
    )
  ) %>%
  ungroup() %>%
  select(seqn, disability_label, n_domains_yes, n_domains_answered)

cat("\nLabel distribution (before merge):\n")
print(table(dlq_label$disability_label, useNA = "always"))


# 3. BUILD THE FEATURE SET (X) -- indirect correlates ONLY

# DELIBERATE EXCLUSION: PFQ items that directly ask about walking/mobility
# limitation (PFQ061-series) are NOT included as predictors -- they are near-
# paraphrases of DLQ050 and would make this a circular relabeling task rather
# than a genuine held-out prediction. We use only PFQ items that are adjacent
# but not redundant: use of special equipment, which is a behavioral/material
# correlate rather than a restatement of the limitation itself.

demo_feat <- demo %>%
  clean_names() %>%
  select(seqn, riagendr, ridageyr, ridreth3, dmdeduc2, dmdmartl,
         indfmpir, dmdhhsiz) %>%
  rename(
    sex             = riagendr,
    age             = ridageyr,
    race_eth        = ridreth3,
    education       = dmdeduc2,
    marital_status  = dmdmartl,
    poverty_ratio   = indfmpir,
    household_size  = dmdhhsiz
  )

pfq_feat <- pfq %>%
  clean_names() %>%
  select(seqn, pfq054) %>%   # PFQ054: uses special equipment (brace/wheelchair/hearing aid)
  rename(uses_special_equipment = pfq054) %>%
  mutate(uses_special_equipment = ifelse(uses_special_equipment %in% c(7, 9), NA, uses_special_equipment))

mcq_feat <- mcq %>%
  clean_names() %>%
  select(seqn, mcq010, mcq160a, mcq160b, mcq160c, mcq160f, mcq220, mcq160l, mcq160m) %>%
  rename(
    asthma_ever      = mcq010,
    arthritis_ever   = mcq160a,
    chf_ever         = mcq160b,
    chd_ever         = mcq160c,
    stroke_ever      = mcq160f,
    cancer_ever      = mcq220,
    liver_ever       = mcq160l,
    thyroid_ever     = mcq160m
  ) %>%
  mutate(across(-seqn, ~ ifelse(.x %in% c(7, 9), NA, .x))) %>%
  rowwise() %>%
  mutate(chronic_condition_count = sum(c(asthma_ever, arthritis_ever, chf_ever,
                                         chd_ever, stroke_ever, cancer_ever,
                                         liver_ever, thyroid_ever) == 1, na.rm = TRUE)) %>%
  ungroup() %>%
  select(seqn, chronic_condition_count)

hiq_feat <- hiq %>%
  clean_names() %>%
  select(seqn, hiq011) %>%
  rename(has_insurance = hiq011) %>%
  mutate(has_insurance = ifelse(has_insurance %in% c(7, 9), NA, has_insurance))

huq_feat <- huq %>%
  clean_names() %>%
  select(seqn, huq010, huq020, huq030, huq051) %>%
  rename(
    general_health_rating    = huq010,
    health_worse_than_last_yr = huq020,
    has_usual_healthcare_place = huq030,
    n_healthcare_visits_12mo  = huq051
  ) %>%
  mutate(across(c(health_worse_than_last_yr, has_usual_healthcare_place),
                ~ ifelse(.x %in% c(7, 9), NA, .x)))

ocq_feat <- ocq %>%
  clean_names() %>%
  select(seqn, ocq380) %>%   # work status last week
  rename(work_status = ocq380) %>%
  mutate(work_status = ifelse(work_status %in% c(7, 9), NA, work_status))


# 4. MERGE EVERYTHING ON SEQN

analytic_df <- demo_feat %>%
  left_join(pfq_feat, by = "seqn") %>%
  left_join(mcq_feat, by = "seqn") %>%
  left_join(hiq_feat, by = "seqn") %>%
  left_join(huq_feat, by = "seqn") %>%
  left_join(ocq_feat, by = "seqn") %>%
  left_join(dlq_label, by = "seqn")

# Restrict to adults (16+, matching DLQ's direct-interview population --
# under-16 responses are proxy-reported and behave differently; keeping them
# in would muddy the subgroup error analysis)
analytic_df <- analytic_df %>% filter(age >= 16)

cat(sprintf("\nFinal analytic sample before NA-handling: %d rows\n", nrow(analytic_df)))

analytic_df <- analytic_df %>% filter(!is.na(disability_label))

cat(sprintf("Final analytic sample after dropping missing labels: %d rows\n", nrow(analytic_df)))
cat("\nLabel balance:\n")
print(prop.table(table(analytic_df$disability_label)))


# 5. RECODE CATEGORICALS AND HANDLE MISSINGNESS IN FEATURES

# We do NOT silently drop rows with any missing feature -- that would bias
# the sample toward people who answer every question, which is itself a
# demographic pattern. Instead: impute categorical NAs as an explicit
# "Missing" level, and median-impute numerics, so missingness becomes a
# modeled signal rather than a silent exclusion.

analytic_df <- analytic_df %>%
  mutate(
    sex            = factor(sex, labels = c("Male", "Female")),
    education      = factor(ifelse(is.na(education) | education %in% c(7,9), "Missing", as.character(education))),
    marital_status = factor(ifelse(is.na(marital_status) | marital_status %in% c(77,99), "Missing", as.character(marital_status))),
    work_status    = factor(ifelse(is.na(work_status), "Missing", as.character(work_status))),
    has_insurance  = factor(ifelse(is.na(has_insurance), "Missing", as.character(has_insurance))),
    has_usual_healthcare_place = factor(ifelse(is.na(has_usual_healthcare_place), "Missing", as.character(has_usual_healthcare_place))),
    uses_special_equipment = factor(ifelse(is.na(uses_special_equipment), "Missing", as.character(uses_special_equipment))),
    disability_label = factor(disability_label, labels = c("No", "Yes"))
  )

numeric_feats <- c("age", "poverty_ratio", "household_size", "chronic_condition_count",
                   "health_worse_than_last_yr")

# Some NHANES columns come back as factors even though they're numeric codes —
# coerce explicitly before imputing, going through character first to avoid
# factor levels being silently reinterpreted as their integer position
analytic_df <- analytic_df %>%
  mutate(
    household_size = as.numeric(as.character(household_size)),
    race_eth_label = as.character(race_eth),
    general_health_rating = factor(ifelse(
      as.character(general_health_rating) %in% c("Refused", "Don't know"),
      "Missing",
      as.character(general_health_rating)
    ))
  )

analytic_df <- analytic_df %>%
  mutate(
    n_healthcare_visits_12mo = factor(
      ifelse(as.character(n_healthcare_visits_12mo) == "Don't know",
             "Missing",
             as.character(n_healthcare_visits_12mo)),
      levels = c("None", "1", "2 to 3", "4 to 5", "6 to 7", "8 to 9",
                 "10 to 12", "13 to 15", "16 or more", "Missing"),
      ordered = TRUE
    )
  )

for (col in numeric_feats) {
  na_flag_col <- paste0(col, "_was_missing")
  analytic_df[[na_flag_col]] <- is.na(analytic_df[[col]])
  med <- median(analytic_df[[col]], na.rm = TRUE)
  analytic_df[[col]][is.na(analytic_df[[col]])] <- med
}

analytic_df <- analytic_df %>%
  mutate(income_tier = case_when(
    poverty_ratio < 1.0 ~ "Below poverty line",
    poverty_ratio < 2.0 ~ "Near poverty (1-2x)",
    poverty_ratio < 4.0 ~ "Middle (2-4x)",
    TRUE ~ "Above 4x poverty line"
  ))


# 6. TRAIN / TEST SPLIT

analytic_df <- analytic_df %>% mutate(strata_var = paste(disability_label, race_eth_label))

train_idx <- createDataPartition(analytic_df$strata_var, p = 0.75, list = FALSE)
train_df  <- analytic_df[train_idx, ]
test_df   <- analytic_df[-train_idx, ]

cat(sprintf("\nTrain n = %d | Test n = %d\n", nrow(train_df), nrow(test_df)))


# 7. MODEL -- full Random Forest, no shortcuts on complexity

feature_cols <- c("sex", "age", "education", "marital_status",
                  "poverty_ratio", "household_size", "uses_special_equipment",
                  "chronic_condition_count", "has_insurance", "general_health_rating",
                  "health_worse_than_last_yr", "has_usual_healthcare_place",
                  "n_healthcare_visits_12mo", "work_status")

train_model_df <- train_df %>% select(disability_label, all_of(feature_cols)) %>%
  mutate(work_status = factor(work_status))

test_model_df <- test_df %>% select(disability_label, all_of(feature_cols), seqn = seqn) %>%
  mutate(work_status = factor(work_status, levels = levels(train_model_df$work_status)),
         general_health_rating = factor(general_health_rating, levels = levels(train_model_df$general_health_rating)),
         n_healthcare_visits_12mo = factor(n_healthcare_visits_12mo, levels = levels(train_model_df$n_healthcare_visits_12mo), ordered = TRUE))

# Note: race_eth_label is intentionally available to the model here. If you want
# a stricter "race-blind" version to compare against (does excluding race as an
# input feature change subgroup error rates?), duplicate this block with
# feature_cols minus race_eth_label -- a useful robustness check for your paper.

rf_model <- randomForest(
  disability_label ~ .,
  data = train_model_df,
  ntree = 500,
  importance = TRUE,
  na.action = na.omit
)

print(rf_model)
cat("\nVariable importance:\n")
print(importance(rf_model))


# 8. PREDICT ON HELD-OUT TEST SET

test_model_df$predicted_label <- predict(rf_model, newdata = test_model_df)
test_model_df$predicted_prob_yes <- predict(rf_model, newdata = test_model_df, type = "prob")[, "Yes"]

test_scored <- test_model_df %>%
  left_join(test_df %>% select(seqn, race_eth_label, income_tier, education), by = "seqn", suffix = c("", "_orig"))


# 9. OVERALL PERFORMANCE

cat("\n===== OVERALL CONFUSION MATRIX =====\n")
print(confusionMatrix(test_scored$predicted_label, test_scored$disability_label, positive = "Yes"))

overall_auc <- roc(test_scored$disability_label, test_scored$predicted_prob_yes)
cat(sprintf("\nOverall AUC: %.3f\n", auc(overall_auc)))


# 10. SUBGROUP-STRATIFIED ERROR -- THIS IS YOUR AXIS 1/AXIS 2 EVIDENCE

subgroup_accuracy <- function(df, group_var) {
  df %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      n = n(),
      accuracy = mean(predicted_label == disability_label, na.rm = TRUE),
      sensitivity = sum(predicted_label == "Yes" & disability_label == "Yes") /
        pmax(sum(disability_label == "Yes"), 1),
      specificity = sum(predicted_label == "No" & disability_label == "No") /
        pmax(sum(disability_label == "No"), 1),
      false_negative_rate = 1 - sensitivity,
      false_positive_rate = 1 - specificity,
      .groups = "drop"
    ) %>%
    arrange(accuracy)
}

cat("\n===== SUBGROUP ACCURACY: RACE/ETHNICITY =====\n")
race_results <- subgroup_accuracy(test_scored, "race_eth_label")
print(race_results)

cat("\n===== SUBGROUP ACCURACY: INCOME TIER =====\n")
income_results <- subgroup_accuracy(test_scored, "income_tier")
print(income_results)

cat("\n===== SUBGROUP ACCURACY: EDUCATION =====\n")
education_results <- subgroup_accuracy(test_scored, "education_orig")
print(education_results)


# 11. SAVE OUTPUTS FOR STEP 2 (decision-rule overlay + justice cross-tab)

write_csv(test_scored, "data/step1_test_predictions_scored_norace.csv")
write_csv(race_results, "data/step1_subgroup_accuracy_race_norace.csv")
write_csv(income_results, "data/step1_subgroup_accuracy_income_norace.csv")
write_csv(education_results, "data/step1_subgroup_accuracy_education_norace.csv")

cat("\n\nStep 1 complete. Files saved:\n")
cat("  - step1_test_predictions_scored_norace.csv   (row-level predictions + truth + subgroups)\n")
cat("  - step1_subgroup_accuracy_race_norace.csv\n")
cat("  - step1_subgroup_accuracy_income_norace.csv\n")
cat("  - step1_subgroup_accuracy_education_norace.csv\n")