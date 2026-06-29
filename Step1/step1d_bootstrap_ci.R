
# OVERNIGHT JOB: Bootstrapped Confidence Intervals on Subgroup Error Rates

# Purpose: your subgroup tables report point estimates (e.g. "65.6% FNR for
# Non-Hispanic Asian respondents, n=222") with no uncertainty range. This
# script resamples the test set thousands of times to put a real 95% CI
# around each subgroup's sensitivity/specificity/FNR, so you can say whether
# an observed gap is distinguishable from sampling noise -- not just report
# a number from a single train/test split.
#
# METHOD: rather than just bootstrapping the existing test set predictions
# (which would only capture sampling variability in WHO ended up in the test
# set, not in the model itself), this does the more rigorous version:
# repeatedly re-splits train/test, refits the Random Forest each time, and
# records subgroup metrics from each fresh fit. This is why it's an overnight
# job, not a 2-minute one -- it's effectively running Step 1's model fit
# hundreds of times over.
#
# Run AFTER sourcing the full step1 script (analytic_df must exist).


library(tidyverse)
library(randomForest)
library(caret)

N_BOOTSTRAP <- 5000   # 5000 full model refits -- this is the overnight-sized number
# drop to 50 first to sanity-check the script runs end-to-end
# before committing to the full overnight run

feature_cols <- c("sex", "age", "race_eth_label", "education", "marital_status",
                  "poverty_ratio", "household_size", "uses_special_equipment",
                  "chronic_condition_count", "has_insurance", "general_health_rating",
                  "health_worse_than_last_yr", "has_usual_healthcare_place",
                  "n_healthcare_visits_12mo", "work_status")

bootstrap_subgroup_metrics <- function(seed_val) {
  set.seed(seed_val)
  
  analytic_df_boot <- analytic_df %>% mutate(strata_var = paste(disability_label, race_eth_label))
  train_idx <- createDataPartition(analytic_df_boot$strata_var, p = 0.75, list = FALSE)
  train_b <- analytic_df_boot[train_idx, ]
  test_b  <- analytic_df_boot[-train_idx, ]
  
  train_model_b <- train_b %>% select(disability_label, all_of(feature_cols)) %>%
    mutate(race_eth_label = factor(race_eth_label), work_status = factor(work_status))
  
  test_model_b <- test_b %>% select(disability_label, all_of(feature_cols), seqn) %>%
    mutate(
      race_eth_label = factor(race_eth_label, levels = levels(train_model_b$race_eth_label)),
      work_status = factor(work_status, levels = levels(train_model_b$work_status)),
      general_health_rating = factor(general_health_rating, levels = levels(train_model_b$general_health_rating)),
      n_healthcare_visits_12mo = factor(n_healthcare_visits_12mo,
                                        levels = levels(train_model_b$n_healthcare_visits_12mo), ordered = TRUE)
    )
  
  rf_b <- randomForest(disability_label ~ ., data = train_model_b, ntree = 500, na.action = na.omit)
  
  test_model_b$predicted_label <- predict(rf_b, newdata = test_model_b)
  
  test_scored_b <- test_model_b %>%
    left_join(test_b %>% select(seqn, race_eth_label, income_tier), by = "seqn", suffix = c("", "_orig"))
  
  race_metrics <- test_scored_b %>%
    group_by(race_eth_label_orig) %>%
    summarise(
      sensitivity = sum(predicted_label == "Yes" & disability_label == "Yes") / pmax(sum(disability_label == "Yes"), 1),
      specificity = sum(predicted_label == "No" & disability_label == "No") / pmax(sum(disability_label == "No"), 1),
      .groups = "drop"
    ) %>%
    mutate(group_type = "race", boot_id = seed_val) %>%
    rename(group = race_eth_label_orig)
  
  income_metrics <- test_scored_b %>%
    group_by(income_tier) %>%
    summarise(
      sensitivity = sum(predicted_label == "Yes" & disability_label == "Yes") / pmax(sum(disability_label == "Yes"), 1),
      specificity = sum(predicted_label == "No" & disability_label == "No") / pmax(sum(disability_label == "No"), 1),
      .groups = "drop"
    ) %>%
    mutate(group_type = "income", boot_id = seed_val) %>%
    rename(group = income_tier)
  
  bind_rows(race_metrics, income_metrics)
}

cat(sprintf("Starting %d bootstrap iterations at %s\n", N_BOOTSTRAP, Sys.time()))
cat("This will take a while -- each iteration refits a full Random Forest.\n")
cat("Progress prints every 25 iterations.\n\n")

results_list <- vector("list", N_BOOTSTRAP)

for (i in 1:N_BOOTSTRAP) {
  results_list[[i]] <- bootstrap_subgroup_metrics(seed_val = 1000 + i)
  if (i %% 25 == 0) {
    cat(sprintf("  Completed %d / %d at %s\n", i, N_BOOTSTRAP, Sys.time()))
    # Save intermediate progress in case the run gets interrupted overnight
    saveRDS(bind_rows(results_list[1:i]), "bootstrap_progress_checkpoint.rds")
  }
}

all_boot_results <- bind_rows(results_list)
write_csv(all_boot_results, "bootstrap_all_iterations_raw.csv")

cat(sprintf("\nAll %d iterations complete at %s\n", N_BOOTSTRAP, Sys.time()))


# SUMMARIZE: 95% CIs for each subgroup

ci_summary <- all_boot_results %>%
  group_by(group_type, group) %>%
  summarise(
    n_iterations = n(),
    mean_sensitivity = mean(sensitivity, na.rm = TRUE),
    ci_low_sensitivity = quantile(sensitivity, 0.025, na.rm = TRUE),
    ci_high_sensitivity = quantile(sensitivity, 0.975, na.rm = TRUE),
    mean_fnr = 1 - mean_sensitivity,
    ci_low_fnr = 1 - ci_high_sensitivity,
    ci_high_fnr = 1 - ci_low_sensitivity,
    mean_specificity = mean(specificity, na.rm = TRUE),
    ci_low_specificity = quantile(specificity, 0.025, na.rm = TRUE),
    ci_high_specificity = quantile(specificity, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(group_type, mean_fnr)

write_csv(ci_summary, "bootstrap_ci_summary.csv")

cat("\n===== BOOTSTRAPPED 95% CONFIDENCE INTERVALS =====\n")
print(ci_summary, n = 50)

cat("\n\nDone. Saved files:\n")
cat("  - bootstrap_all_iterations_raw.csv   (every iteration's raw metrics)\n")
cat("  - bootstrap_ci_summary.csv           (final 95% CIs per subgroup -- USE THIS)\n")
cat("\nHow to read this: if two subgroups' FNR confidence intervals do NOT overlap,\n")
cat("the gap between them is statistically distinguishable from sampling noise --\n")
cat("a much stronger claim than a single point estimate.\n")

system('osascript -e \'display notification "Bootstrap run complete" with title "R Script Finished"\'')