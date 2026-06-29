
# STEP 3C: Multi-Cycle Pooling (2013-2014, 2015-2016, 2017-2018)

# NOTE: 2011-2012 (cycle G) is EXCLUDED -- DLQ_G does not exist (confirmed via
# Step 3b header discovery). Only H, I, J have the disability questionnaire.
#
# This script rebuilds Step 1's full pipeline (pull -> label -> features ->
# clean -> merge), run once per cycle, then stacks all three into one pooled
# analytic_df with a `cycle` column retained for any later cycle-effect checks.
#
# Harmonization notes baked in below, from the Step 3b header report:
#   - RIDRETH3, DMDEDUC2, INDFMPIR, DLQ010-080, PFQ054, HIQ011, core MCQ
#     columns: stable text/coding across H/I/J -- same recode logic as Step 1.
#   - HUQ010 (general health): same odd punctuation-in-label text across all
#     three cycles -- same "Refused"/"Don't know" -> "Missing" fix as Step 1.
#   - HUQ051 (healthcare visits): bucket boundaries are NOT identical across
#     cycles (I has an extra category vs J's 10). Harmonized below to the
#     COARSEST common bucket set so no cycle has a category the others lack.
#   - OCQ380 (work status): minor category-count differences across cycles --
#     harmonized to a common set; rare cycle-specific levels collapsed to
#     "Other" rather than dropped, to avoid silently losing respondents.


library(nhanesA)
library(tidyverse)
library(janitor)

cycles_to_pool <- c(H = "2013-2014", I = "2015-2016", J = "2017-2018")


# FUNCTION: build one cycle's analytic_df, using the SAME logic as Step 1,
# parameterized by cycle suffix


build_cycle_data <- function(suffix) {
  
  cat(sprintf("\n--- Building cycle %s ---\n", suffix))
  
  demo <- nhanes(paste0("DEMO_", suffix))
  dlq  <- nhanes(paste0("DLQ_",  suffix))
  pfq  <- nhanes(paste0("PFQ_",  suffix))
  mcq  <- nhanes(paste0("MCQ_",  suffix))
  hiq  <- nhanes(paste0("HIQ_",  suffix))
  huq  <- nhanes(paste0("HUQ_",  suffix))
  ocq  <- nhanes(paste0("OCQ_",  suffix))
  
  cat(sprintf("  Rows pulled -- demo:%d dlq:%d pfq:%d mcq:%d hiq:%d huq:%d ocq:%d\n",
              nrow(demo), nrow(dlq), nrow(pfq), nrow(mcq), nrow(hiq), nrow(huq), nrow(ocq)))
  
  # ---- LABEL: same 6-domain DLQ construction as Step 1 ----
  dlq_label <- dlq %>%
    clean_names() %>%
    select(seqn, dlq010, dlq020, dlq040, dlq050, dlq060, dlq080) %>%
    mutate(across(c(dlq010, dlq020, dlq040, dlq050, dlq060, dlq080),
                  ~ as.character(.x))) %>%
    mutate(across(c(dlq010, dlq020, dlq040, dlq050, dlq060, dlq080),
                  ~ ifelse(.x %in% c("Refused", "Don't know"), NA, .x))) %>%
    mutate(across(c(dlq010, dlq020, dlq040, dlq050, dlq060, dlq080),
                  ~ ifelse(.x == "Yes", 1, ifelse(.x == "No", 0, NA)))) %>%
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
  
  # ---- DEMOGRAPHICS ----
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
    ) %>%
    mutate(
      race_eth_label = as.character(race_eth),
      # DMDHHSIZ comes back as FACTOR in some cycles (e.g. "7 or more people
      # in the Household") rather than numeric -- harmonize by extracting the
      # leading number, collapsing the "7 or more" text to 7
      household_size = case_when(
        str_detect(as.character(household_size), "^[0-9]+$") ~ as.numeric(as.character(household_size)),
        str_detect(as.character(household_size), "7 or more") ~ 7,
        TRUE ~ NA_real_
      )
    )
  
  # ---- PFQ: special equipment only (same exclusion logic as Step 1) ----
  pfq_feat <- pfq %>%
    clean_names() %>%
    select(seqn, pfq054) %>%
    rename(uses_special_equipment = pfq054) %>%
    mutate(uses_special_equipment = as.character(uses_special_equipment),
           uses_special_equipment = ifelse(uses_special_equipment %in% c("Refused", "Don't know"),
                                           NA, uses_special_equipment))
  
  # ---- MCQ: same 8 chronic condition columns, confirmed stable across H/I/J ----
  mcq_feat <- mcq %>%
    clean_names() %>%
    select(seqn, mcq010, mcq160a, mcq160b, mcq160c, mcq160f, mcq220, mcq160l, mcq160m) %>%
    mutate(across(-seqn, as.character)) %>%
    mutate(across(-seqn, ~ ifelse(.x %in% c("Refused", "Don't know"), NA, .x))) %>%
    mutate(across(-seqn, ~ ifelse(.x == "Yes", 1, ifelse(.x == "No", 0, NA)))) %>%
    rowwise() %>%
    mutate(chronic_condition_count = sum(c(mcq010, mcq160a, mcq160b, mcq160c,
                                           mcq160f, mcq220, mcq160l, mcq160m) == 1, na.rm = TRUE)) %>%
    ungroup() %>%
    select(seqn, chronic_condition_count)
  
  # ---- HIQ: insurance status ----
  hiq_feat <- hiq %>%
    clean_names() %>%
    select(seqn, hiq011) %>%
    rename(has_insurance = hiq011) %>%
    mutate(has_insurance = as.character(has_insurance),
           has_insurance = ifelse(has_insurance %in% c("Refused", "Don't know"), NA, has_insurance))
  
  # ---- HUQ: general health (factor) + healthcare visits (HARMONIZED buckets) ----
  # HUQ051 bucket harmonization: collapse to the coarsest common categories
  # across H/I/J so no cycle has a finer split the others lack.
  harmonize_visits <- function(x) {
    x <- as.character(x)
    case_when(
      x %in% c("None") ~ "None",
      x %in% c("1") ~ "1",
      x %in% c("2 to 3") ~ "2 to 3",
      x %in% c("4 to 5") ~ "4 to 5",
      x %in% c("6 to 7") ~ "6 to 7",
      x %in% c("8 to 9") ~ "8 to 9",
      x %in% c("10 to 12") ~ "10 to 12",
      x %in% c("13 to 15") ~ "13 to 15",
      x %in% c("16 or more") ~ "16 or more",
      x %in% c("Don't know") ~ "Missing",
      TRUE ~ "Missing"   # catches any cycle-specific extra bucket not listed above
    )
  }
  
  huq_feat <- huq %>%
    clean_names() %>%
    select(seqn, huq010, huq020, huq030, huq051) %>%
    rename(
      general_health_rating      = huq010,
      health_worse_than_last_yr  = huq020,
      has_usual_healthcare_place = huq030,
      n_healthcare_visits_12mo   = huq051
    ) %>%
    mutate(
      general_health_rating = as.character(general_health_rating),
      general_health_rating = factor(ifelse(
        general_health_rating %in% c("Refused", "Don't know"), "Missing", general_health_rating
      )),
      health_worse_than_last_yr = as.character(health_worse_than_last_yr),
      health_worse_than_last_yr = ifelse(health_worse_than_last_yr %in% c("Refused", "Don't know"),
                                         NA, health_worse_than_last_yr),
      has_usual_healthcare_place = as.character(has_usual_healthcare_place),
      has_usual_healthcare_place = ifelse(has_usual_healthcare_place %in% c("Refused", "Don't know"),
                                          NA, has_usual_healthcare_place),
      n_healthcare_visits_12mo = factor(
        harmonize_visits(n_healthcare_visits_12mo),
        levels = c("None", "1", "2 to 3", "4 to 5", "6 to 7", "8 to 9",
                   "10 to 12", "13 to 15", "16 or more", "Missing"),
        ordered = TRUE
      )
    )
  
  # ---- OCQ: work status (harmonized to a common category set) ----
  # Rare cycle-specific categories not in the common set are collapsed to
  # "Other" rather than dropped, so respondents aren't silently lost.
  common_work_status <- c("Working at a job or business,", "Not working at a job or business?",
                          "Looking for work, or", "With a job or business but not at work,",
                          "Going to school", "Unable to work for health reasons", "Disabled",
                          "Retired", "Other", "Taking care of house or family", "On layoff")
  
  ocq_feat <- ocq %>%
    clean_names() %>%
    select(seqn, ocq380) %>%
    rename(work_status = ocq380) %>%
    mutate(
      work_status = as.character(work_status),
      work_status = ifelse(work_status %in% c("Refused", "Don't know"), NA, work_status),
      work_status = ifelse(!is.na(work_status) & !(work_status %in% common_work_status),
                           "Other", work_status)
    )
  
  # ---- MERGE ----
  cycle_df <- demo_feat %>%
    left_join(pfq_feat, by = "seqn") %>%
    left_join(mcq_feat, by = "seqn") %>%
    left_join(hiq_feat, by = "seqn") %>%
    left_join(huq_feat, by = "seqn") %>%
    left_join(ocq_feat, by = "seqn") %>%
    left_join(dlq_label, by = "seqn") %>%
    mutate(cycle = suffix) %>%
    filter(age >= 16) %>%
    filter(!is.na(disability_label))
  
  cat(sprintf("  Final analytic rows for cycle %s: %d\n", suffix, nrow(cycle_df)))
  
  cycle_df
}


# BUILD ALL THREE CYCLES AND STACK


all_cycles_list <- map(names(cycles_to_pool), build_cycle_data)
names(all_cycles_list) <- names(cycles_to_pool)

pooled_df <- bind_rows(all_cycles_list)

cat(sprintf("\n\n===== POOLING COMPLETE =====\n"))
cat(sprintf("Total pooled rows: %d (vs. %d in single-cycle Step 1)\n",
            nrow(pooled_df), nrow(all_cycles_list[["J"]])))
cat("\nRows per cycle:\n")
print(table(pooled_df$cycle))

cat("\nLabel balance, pooled:\n")
print(prop.table(table(pooled_df$disability_label)))

cat("\nLabel balance, by cycle (check for cycle drift):\n")
print(pooled_df %>% group_by(cycle) %>% summarise(pct_disability = mean(disability_label, na.rm = TRUE)))


# FINISH REMAINING CLEANUP (sex/education/marital_status factors, numeric
# imputation, income_tier) -- SAME logic as Step 1, applied once to the
# pooled data


pooled_df <- pooled_df %>%
  mutate(
    sex            = factor(sex, labels = c("Male", "Female")),
    education      = factor(ifelse(is.na(education) | education %in% c("Refused", "Don't know"),
                                   "Missing", as.character(education))),
    marital_status = factor(ifelse(is.na(marital_status) | marital_status %in% c("Refused", "Don't know"),
                                   "Missing", as.character(marital_status))),
    work_status    = factor(ifelse(is.na(work_status), "Missing", work_status)),
    has_insurance  = factor(ifelse(is.na(has_insurance), "Missing", has_insurance)),
    has_usual_healthcare_place = factor(ifelse(is.na(has_usual_healthcare_place), "Missing", has_usual_healthcare_place)),
    uses_special_equipment = factor(ifelse(is.na(uses_special_equipment), "Missing", uses_special_equipment)),
    health_worse_than_last_yr = as.numeric(factor(health_worse_than_last_yr)),  # ordinal-ish numeric, same as Step 1 treated it
    disability_label = factor(disability_label, labels = c("No", "Yes"))
  )

numeric_feats <- c("age", "poverty_ratio", "household_size", "chronic_condition_count",
                   "health_worse_than_last_yr")

for (col in numeric_feats) {
  na_flag_col <- paste0(col, "_was_missing")
  pooled_df[[na_flag_col]] <- is.na(pooled_df[[col]])
  med <- median(pooled_df[[col]], na.rm = TRUE)
  pooled_df[[col]][is.na(pooled_df[[col]])] <- med
}

pooled_df <- pooled_df %>%
  mutate(income_tier = case_when(
    poverty_ratio < 1.0 ~ "Below poverty line",
    poverty_ratio < 2.0 ~ "Near poverty (1-2x)",
    poverty_ratio < 4.0 ~ "Middle (2-4x)",
    TRUE ~ "Above 4x poverty line"
  ))

cat(sprintf("\n\nFinal pooled analytic_df ready: %d rows, %d columns\n",
            nrow(pooled_df), ncol(pooled_df)))
cat("Subgroup sizes (race), pooled -- compare to single-cycle Step 1 sizes:\n")
print(table(pooled_df$race_eth_label))


# SAVE

write_csv(pooled_df, "data/step3c_pooled_analytic_df.csv")
cat("\nSaved: step3c_pooled_analytic_df.csv\n")
cat("\nThis is now your `analytic_df` equivalent -- rename/assign it and feed it\n")
cat("into Step 1's Section 6 onward (train/test split, model fit, subgroup\n")
cat("scoring) exactly as before. The `cycle` column is retained in case you\n")
cat("want to check for cycle-specific effects later.\n")