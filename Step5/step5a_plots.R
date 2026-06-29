
# STEP 5: ALL FIGURES - ggplot2

# Every plot for the paper lives in this one file, in clearly labeled
# sections. Shared palette/theme objects are defined once at the top so
# changing the house style updates every plot at once.
#
# Each section is self-contained: loads its own data (from memory if present,
# else from the saved CSV), builds the plot, prints it, and saves PNG + PDF.
# Re-running the whole file is cheap - no modeling, just plotting from
# already-computed results.

library(tidyverse)
library(scales)


# 0. SHARED STYLE - change once, applies everywhere


# Core palette
COLOR_RULE_A      <- "#4A6FA5"   # outreach / missed-care harm (cool blue)
COLOR_RULE_B      <- "#C0735A"   # review / wrongly-flagged harm (warm terracotta)
COLOR_NEUTRAL     <- "#7A7A7A"   # neutral/reference bars
COLOR_HIGHLIGHT   <- "#9B4F4F"   # for calling out the worst-performing group
COLOR_TEXT_DARK   <- "#2B2B2B"
COLOR_TEXT_MUTED  <- "#666666"
COLOR_CAPTION     <- "#888888"

# Shared theme, applied on top of theme_minimal() in every plot
theme_paper <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2, margin = ggplot2::margin(b = 4)),
      plot.subtitle = element_text(face = "bold", size = base_size - 2.5, color = COLOR_TEXT_MUTED, margin = ggplot2::margin(b = 14)),
      plot.caption = element_text(face = "bold", size = base_size - 5, color = COLOR_CAPTION, hjust = 0, margin = ggplot2::margin(t = 12)),
      legend.text = element_text(face = "bold", size = base_size - 3),
      legend.title = element_text(face = "bold", size = base_size - 3),
      axis.text = element_text(face = "bold", size = base_size - 2.5),
      axis.title.x = element_text(face = "bold", size = base_size - 2, margin = ggplot2::margin(t = 8)),
      axis.title.y = element_text(face = "bold", size = base_size - 2, margin = ggplot2::margin(r = 8)),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(face = "bold", size = base_size - 2)
    )
}

# Output directory for all figures - separate from scripts/data
PLOTS_DIR <- "plots"
if (!dir.exists(PLOTS_DIR)) {
  dir.create(PLOTS_DIR)
  cat(sprintf("Created directory: %s/\n", PLOTS_DIR))
}

# Standard save helper - saves PNG only per-plot, into plots/. All plots also
# get collected into `all_plots_list` below, so a single combined PDF can be
# built at the end of the script instead of one PDF per figure.
all_plots_list <- list()
all_plots_dims  <- list()

save_fig <- function(plot_obj, filename, width = 8, height = 6) {
  png_path <- file.path(PLOTS_DIR, paste0(filename, ".png"))
  ggsave(png_path, plot_obj, width = width, height = height, dpi = 300, bg = "white")
  cat(sprintf("Saved: %s\n", png_path))
  
  # Register this plot + its dimensions for the combined master PDF
  all_plots_list[[length(all_plots_list) + 1]] <<- plot_obj
  all_plots_dims[[length(all_plots_dims) + 1]] <<- c(width, height)
}

# Consistent income tier ordering used across multiple plots
INCOME_LEVELS <- c("Below poverty line", "Near poverty (1-2x)", "Middle (2-4x)", "Above 4x poverty line")



# PLOT 1: Harm Inversion - Income Tier

if (!exists("comparison_income")) {
  comparison_income <- read_csv("data/step3e_comparison_income_pooled.csv", show_col_types = FALSE)
}

income_long <- comparison_income %>%
  select(income_tier, rule_a_harm_rate, rule_b_harm_rate) %>%
  pivot_longer(cols = c(rule_a_harm_rate, rule_b_harm_rate), names_to = "rule", values_to = "harm_rate") %>%
  mutate(
    rule = recode(rule, rule_a_harm_rate = "Rule A: Outreach\n(missed-care harm)",
                  rule_b_harm_rate = "Rule B: Review\n(wrongly-flagged harm)"),
    income_tier = factor(income_tier, levels = INCOME_LEVELS)
  )

p1_harm_inversion_income <- ggplot(income_long, aes(x = income_tier, y = harm_rate, fill = rule)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, color = "white", linewidth = 0.3) +
  geom_text(aes(label = percent(harm_rate, accuracy = 1)),
            position = position_dodge(width = 0.75), vjust = -0.5, size = 3.4, color = COLOR_TEXT_DARK) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Rule A: Outreach\n(missed-care harm)" = COLOR_RULE_A,
                               "Rule B: Review\n(wrongly-flagged harm)" = COLOR_RULE_B), name = NULL) +
  labs(
    title = "Who the proxy harms depends on what it's used for",
    subtitle = "Wealthier disabled respondents are missed by outreach; poorer non-disabled\nrespondents are wrongly flagged for review - same model, opposite populations harmed",
    x = NULL, y = "Harm rate within base population",
    caption = "Base population: true disability cases (Rule A) / true non-disability cases (Rule B).\nNHANES 2013-2018 pooled, n = 4,733 held-out test respondents."
  ) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 9.5))

print(p1_harm_inversion_income)
save_fig(p1_harm_inversion_income, "step5_plot1_harm_inversion_income")



# PLOT 2: Harm Inversion - Race/Ethnicity

if (!exists("comparison_race")) {
  comparison_race <- read_csv("data/step3e_comparison_race_pooled.csv", show_col_types = FALSE)
}

race_long <- comparison_race %>%
  select(race_eth_label_orig, rule_a_harm_rate, rule_b_harm_rate) %>%
  mutate(race_eth_label_orig = fct_reorder(race_eth_label_orig, rule_a_harm_rate, .desc = TRUE)) %>%
  pivot_longer(cols = c(rule_a_harm_rate, rule_b_harm_rate), names_to = "rule", values_to = "harm_rate") %>%
  mutate(
    rule = recode(rule, rule_a_harm_rate = "Rule A: Outreach\n(missed-care harm)",
                  rule_b_harm_rate = "Rule B: Review\n(wrongly-flagged harm)")
  )

p2_harm_inversion_race <- ggplot(race_long, aes(x = race_eth_label_orig, y = harm_rate, fill = rule)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, color = "white", linewidth = 0.3) +
  geom_text(aes(label = percent(harm_rate, accuracy = 1)),
            position = position_dodge(width = 0.75), vjust = -0.5, size = 3.2, color = COLOR_TEXT_DARK) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Rule A: Outreach\n(missed-care harm)" = COLOR_RULE_A,
                               "Rule B: Review\n(wrongly-flagged harm)" = COLOR_RULE_B), name = NULL) +
  labs(
    title = "The same inversion holds across race/ethnicity",
    subtitle = "Non-Hispanic Asian respondents are most missed by outreach but least\nlikely to be wrongly flagged for review - White respondents show the reverse",
    x = NULL, y = "Harm rate within base population",
    caption = "NHANES 2013-2018 pooled, n = 4,733 held-out test respondents."
  ) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 9))

print(p2_harm_inversion_race)
save_fig(p2_harm_inversion_race, "step5_plot2_harm_inversion_race", width = 9, height = 6.5)



# PLOT 3: Bootstrap 95% CIs - Income Tier (the "is it real or noise" figure)

if (!exists("ci_summary_pooled")) {
  ci_summary_pooled <- read_csv("data/step3d_bootstrap_ci_summary_pooled.csv", show_col_types = FALSE)
}

income_ci <- ci_summary_pooled %>%
  filter(group_type == "income") %>%
  mutate(group = factor(group, levels = INCOME_LEVELS))

p3_bootstrap_ci_income <- ggplot(income_ci, aes(x = group, y = mean_fnr)) +
  geom_col(fill = COLOR_RULE_A, width = 0.55, alpha = 0.85) +
  geom_errorbar(aes(ymin = ci_low_fnr, ymax = ci_high_fnr), width = 0.18, linewidth = 0.7, color = COLOR_TEXT_DARK) +
  geom_text(aes(y = ci_high_fnr, label = percent(mean_fnr, accuracy = 1)), 
            vjust = -0.8, size = 3.6, color = COLOR_TEXT_DARK) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "The income gap is statistically real, not sampling noise",
    subtitle = "False-negative rate by income tier, with 95% bootstrap confidence intervals\n(2,000 iterations) - the poorest and wealthiest tiers' intervals do not overlap",
    x = NULL, y = "False negative rate",
    caption = "Bootstrapped 95% CIs, 2,000 resampled model refits on pooled NHANES 2013-2018 data."
  ) +
  theme_paper()

print(p3_bootstrap_ci_income)
save_fig(p3_bootstrap_ci_income, "step5_plot3_bootstrap_ci_income")



# PLOT 4: Bootstrap 95% CIs - Race/Ethnicity

race_ci <- ci_summary_pooled %>%
  filter(group_type == "race") %>%
  mutate(group = fct_reorder(group, mean_fnr, .desc = TRUE))

p4_bootstrap_ci_race <- ggplot(race_ci, aes(x = group, y = mean_fnr)) +
  geom_col(fill = COLOR_RULE_A, width = 0.55, alpha = 0.85) +
  geom_errorbar(aes(ymin = ci_low_fnr, ymax = ci_high_fnr), width = 0.18, linewidth = 0.7, color = COLOR_TEXT_DARK) +
  geom_text(aes(y = ci_high_fnr, label = percent(mean_fnr, accuracy = 1)), 
            vjust = -0.8, size = 3.4, color = COLOR_TEXT_DARK) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "Non-Hispanic Asian respondents: the clearest race-based gap",
    subtitle = "False-negative rate by race/ethnicity, with 95% bootstrap confidence intervals -\nAsian and White respondents' intervals no longer overlap after pooling cycles",
    x = NULL, y = "False negative rate",
    caption = "Bootstrapped 95% CIs, 2,000 resampled model refits on pooled NHANES 2013-2018 data."
  ) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 9))

print(p4_bootstrap_ci_race)
save_fig(p4_bootstrap_ci_race, "step5_plot4_bootstrap_ci_race", width = 9, height = 6.5)



# PLOT 5: Per-Domain Breakdown - where the model actually has signal

if (!exists("all_domain_income")) {
  all_domain_income <- read_csv("data/step4b_per_domain_income.csv", show_col_types = FALSE)
}

domain_labels <- c(
  hearing_difficulty = "Hearing",
  vision_difficulty = "Vision",
  concentration_difficulty = "Concentration",
  mobility_difficulty = "Mobility",
  selfcare_difficulty = "Self-care",
  errands_difficulty = "Errands"
)

domain_income_plot_df <- all_domain_income %>%
  mutate(
    domain_label = recode(domain, !!!domain_labels),
    income_tier = factor(income_tier, levels = INCOME_LEVELS)
  )

p5_per_domain_income <- ggplot(domain_income_plot_df, aes(x = income_tier, y = false_negative_rate)) +
  geom_col(fill = COLOR_RULE_B, width = 0.6, alpha = 0.85) +
  facet_wrap(~ domain_label, ncol = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "The income gradient is concentrated in mobility, not other domains",
    subtitle = "Hearing and vision difficulty are barely detectable by this proxy for ANY income\ngroup; mobility is the one domain with real signal - and the clearest income gradient",
    x = NULL, y = "False negative rate",
    caption = "Per-domain models fit separately on each of the 6 DLQ disability domains, pooled NHANES 2013-2018."
  ) +
  theme_paper(base_size = 11) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 7.5))

print(p5_per_domain_income)
save_fig(p5_per_domain_income, "step5_plot5_per_domain_income", width = 9, height = 7)



# PLOT 6: Calibration - predicted vs observed probability, overall

if (!exists("overall_calibration")) {
  overall_calibration <- read_csv("data/step4a_calibration_overall.csv", show_col_types = FALSE)
}

p6_calibration <- ggplot(overall_calibration, aes(x = mean_predicted_prob, y = observed_rate)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = COLOR_NEUTRAL, linewidth = 0.6) +
  geom_point(size = 3, color = COLOR_RULE_A) +
  geom_line(color = COLOR_RULE_A, linewidth = 0.6, alpha = 0.6) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Predicted probabilities track observed rates well overall",
    subtitle = "10-bin calibration curve, all respondents pooled - points close to the dashed\ndiagonal indicate predicted probabilities are reasonably trustworthy on average",
    x = "Mean predicted probability (per bin)", y = "Observed disability rate (per bin)",
    caption = "Mean absolute calibration gap (overall): 0.024. Pooled NHANES 2013-2018 held-out test set."
  ) +
  coord_equal() +
  theme_paper()

print(p6_calibration)
save_fig(p6_calibration, "step5_plot6_calibration_overall", width = 7, height = 7)



# PLOT 7: Calibration error by subgroup - the "reliability ≠ sensitivity" figure

race_cal_summary <- read_csv("data/step4a_calibration_summary_race.csv", show_col_types = FALSE)

p7_calibration_by_race <- ggplot(race_cal_summary %>% mutate(race_eth_label_orig = fct_reorder(race_eth_label_orig, mean_abs_calibration_error)),
                                 aes(x = race_eth_label_orig, y = mean_abs_calibration_error)) +
  geom_col(fill = COLOR_HIGHLIGHT, width = 0.6, alpha = 0.85) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Calibration error and sensitivity gaps don't track each other",
    subtitle = "Non-Hispanic Asian respondents have the worst SENSITIVITY but reasonably good\nCALIBRATION; \"Other Race/Multiracial\" shows the opposite pattern",
    x = NULL, y = "Mean absolute calibration error",
    caption = "Lower = predicted probabilities track observed outcomes more closely for that group.\nPooled NHANES 2013-2018."
  ) +
  theme_paper()

print(p7_calibration_by_race)
save_fig(p7_calibration_by_race, "step5_plot7_calibration_by_race")



# PLOT 8: Cross-cycle generalization - random split vs. temporal split

# Manually constructed comparison table (pulling key numbers from step3d and
# step4d's printed output) since these live in two different script runs'
# environments and weren't saved in a single joined CSV.

cross_cycle_comparison <- tribble(
  ~income_tier,            ~split_type,        ~false_negative_rate,
  "Below poverty line",    "Random split",      0.397,
  "Near poverty (1-2x)",   "Random split",      0.426,
  "Middle (2-4x)",         "Random split",      0.522,
  "Above 4x poverty line", "Random split",      0.649,
  "Below poverty line",    "Temporal split",    0.436,
  "Near poverty (1-2x)",   "Temporal split",    0.440,
  "Middle (2-4x)",         "Temporal split",    0.511,
  "Above 4x poverty line", "Temporal split",    0.625
) %>%
  mutate(income_tier = factor(income_tier, levels = INCOME_LEVELS))

p8_cross_cycle <- ggplot(cross_cycle_comparison, aes(x = income_tier, y = false_negative_rate,
                                                     color = split_type, group = split_type)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.75)) +
  scale_color_manual(values = c("Random split" = COLOR_NEUTRAL, "Temporal split" = COLOR_HIGHLIGHT), name = NULL) +
  labs(
    title = "The income gradient survives the strictest possible test",
    subtitle = "Training on 2013-2016 and testing ONLY on fully unseen 2017-2018 respondents\nproduces nearly identical results to a random pooled split",
    x = NULL, y = "False negative rate",
    caption = "Random split: Step 3d (train+test mixed across cycles). Temporal split: Step 4d\n(train on cycles H+I only, test exclusively on cycle J)."
  ) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 9.5))

print(p8_cross_cycle)
save_fig(p8_cross_cycle, "step5_plot8_cross_cycle_generalization")



# MASTER PDF - all 8 figures combined into one document

# A single PDF device stays open for the whole document; pages can't change
# size mid-document in the base pdf() device, so we size the document to fit
# the widest/tallest plot in the set, and each individual plot still renders
# at full quality within that page (ggplot scales to fill the page cleanly).

library(qpdf)

master_pdf_path <- file.path(PLOTS_DIR, "step5_all_figures_master.pdf")

temp_pdfs <- c()
for (i in seq_along(all_plots_list)) {
  dims <- all_plots_dims[[i]]
  temp_path <- file.path(PLOTS_DIR, sprintf("_temp_page_%d.pdf", i))
  ggsave(temp_path, all_plots_list[[i]], width = dims[1], height = dims[2], device = cairo_pdf)
  temp_pdfs <- c(temp_pdfs, temp_path)
}

pdf_combine(input = temp_pdfs, output = master_pdf_path)
unlink(temp_pdfs)

cat(sprintf("\nSaved combined master PDF: %s (%d figures)\n", master_pdf_path, length(all_plots_list)))


# DONE

cat("\n\nAll 8 figures generated:\n")
cat(sprintf("  - Individual PNGs in %s/\n", PLOTS_DIR))
cat(sprintf("  - One combined PDF: %s\n", master_pdf_path))