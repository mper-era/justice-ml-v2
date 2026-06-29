
# STEP 4C: Missing-Education Investigation

# Purpose: across every prior run, the "Missing" education category behaved
# strangely - e.g. in Step 1's first run, sensitivity for this group was
# just 0.087 (FNR 91.3%), far worse than any other education category. This
# script investigates WHY, rather than just imputing past it as before.
#
# Key question: are people who skip the education question on NHANES
# systematically different in ways that matter (age, survey mode, proxy-
# reported interview, language barrier) - i.e. is the missingness itself
# informative - or is this just a small, noisy subgroup?
#
# PREREQUISITE: requires `pooled_df` in memory (auto-reloads from CSV).


library(tidyverse)

if (!exists("pooled_df")) {
  cat("pooled_df not found - loading from saved CSV\n")
  pooled_df <- read_csv("data/step3c_pooled_analytic_df.csv", show_col_types = FALSE)
}

cat(sprintf("Pooled data loaded: %d rows\n", nrow(pooled_df)))


# 1. HOW BIG IS THIS GROUP, AND HOW DOES IT COMPARE DEMOGRAPHICALLY?

pooled_df <- pooled_df %>%
  mutate(education_missing_flag = ifelse(education == "Missing", "Missing", "Answered"))

cat("\n===== SIZE OF MISSING-EDUCATION GROUP =====\n")
print(table(pooled_df$education_missing_flag))
cat(sprintf("\nMissing-education group: %.1f%% of total sample\n",
            100 * mean(pooled_df$education_missing_flag == "Missing")))


# 2. DEMOGRAPHIC PROFILE: how does the missing-education group differ?

cat("\n===== AGE: missing-education vs. answered =====\n")
print(pooled_df %>% group_by(education_missing_flag) %>%
        summarise(mean_age = mean(age, na.rm = TRUE), median_age = median(age, na.rm = TRUE),
                  n = n(), .groups = "drop"))

cat("\n===== RACE/ETHNICITY DISTRIBUTION: missing-education vs. answered =====\n")
print(pooled_df %>% group_by(education_missing_flag, race_eth_label) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(education_missing_flag) %>%
        mutate(pct = n / sum(n) * 100) %>%
        arrange(education_missing_flag, desc(pct)))

cat("\n===== POVERTY RATIO: missing-education vs. answered =====\n")
print(pooled_df %>% group_by(education_missing_flag) %>%
        summarise(mean_poverty_ratio = mean(poverty_ratio, na.rm = TRUE),
                  pct_missing_poverty = mean(is.na(poverty_ratio)) * 100,
                  n = n(), .groups = "drop"))

cat("\n===== MARITAL STATUS: missing-education vs. answered =====\n")
print(pooled_df %>% group_by(education_missing_flag, marital_status) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(education_missing_flag) %>%
        mutate(pct = n / sum(n) * 100) %>%
        filter(education_missing_flag == "Missing") %>%
        arrange(desc(pct)))


# 3. DOES THE MISSING-EDUCATION GROUP ALSO HAVE OTHER MISSING DATA?

# If education-missing respondents ALSO tend to be missing other fields
# (insurance, healthcare access, work status), that's a sign of a more
# general pattern - e.g. a harder-to-interview population (language
# barriers, proxy interviews, lower survey engagement) - rather than
# something specific to the education question.

cat("\n===== CO-OCCURRING MISSINGNESS: do education-missing respondents also\n")
cat("      have more missing data in OTHER fields? =====\n")

other_missing_check <- pooled_df %>%
  group_by(education_missing_flag) %>%
  summarise(
    n = n(),
    pct_insurance_missing = mean(has_insurance == "Missing", na.rm = TRUE) * 100,
    pct_healthcare_place_missing = mean(has_usual_healthcare_place == "Missing", na.rm = TRUE) * 100,
    pct_work_status_missing = mean(work_status == "Missing", na.rm = TRUE) * 100,
    pct_special_equipment_missing = mean(uses_special_equipment == "Missing", na.rm = TRUE) * 100,
    .groups = "drop"
  )
print(other_missing_check)

cat("\nInterpretation: if the 'Missing' education row shows MUCH higher %s\n")
cat("missing in other fields too, this confirms a general low-response-\n")
cat("engagement pattern rather than something specific to the education\n")
cat("question alone - which matters because it would mean the model is\n")
cat("trying to predict disability for a group with systematically LESS\n")
cat("usable signal across the board, not just one missing field.\n")


# 4. TRUE DISABILITY RATE: is this group different in the outcome itself?

cat("\n===== TRUE DISABILITY RATE: missing-education vs. answered =====\n")
print(pooled_df %>% group_by(education_missing_flag) %>%
        summarise(n = n(), pct_disability = mean(disability_label == "Yes", na.rm = TRUE) * 100,
                  .groups = "drop"))


# 5. CYCLE DISTRIBUTION: is missing-education concentrated in one cycle?

cat("\n===== CYCLE DISTRIBUTION: is this a cycle-specific pattern? =====\n")
print(pooled_df %>% group_by(cycle, education_missing_flag) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(cycle) %>%
        mutate(pct = n / sum(n) * 100) %>%
        filter(education_missing_flag == "Missing"))


# SAVE

write_csv(other_missing_check, "data/step4c_co_occurring_missingness.csv")

cat("\n\nStep 4c complete.\n")