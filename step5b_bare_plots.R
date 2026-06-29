
# STEP 5B: Bare Plot Versions (no title, no subtitle)

# Generates a second PNG of each of the 8 figures from step5_plots.R, with
# BOTH the title and subtitle fully removed -- just the plot itself, ready
# to drop into the paper with a caption handled by the manuscript text
# instead of on-figure text. This is the standard convention for academic
# paper figures (captions live in the document, not baked into the image).
#
# NOTE: labs(title = NULL, subtitle = NULL) does NOT actually remove text in
# ggplot2 -- NULL is treated as "leave unchanged," not "clear this." The
# reliable way to suppress rendering is via theme(plot.title = element_blank(),
# plot.subtitle = element_blank()), which is what this script uses.
#
# METHOD: reuses the plot objects already stored in `all_plots_list` from
# step5_plots.R rather than rebuilding from scratch.
#
# PREREQUISITE: run step5_plots.R FIRST, in the SAME R session (do not
# restart R in between) -- this script depends on `all_plots_list` and
# `all_plots_dims` already existing in memory from that run.


library(tidyverse)

stopifnot("all_plots_list not found -- run step5_plots.R first in this session" =
            exists("all_plots_list"))
stopifnot("all_plots_dims not found -- run step5_plots.R first in this session" =
            exists("all_plots_dims"))
stopifnot("PLOTS_DIR not found -- run step5_plots.R first in this session" =
            exists("PLOTS_DIR"))

cat(sprintf("Found %d plots from step5_plots.R session\n", length(all_plots_list)))


# Generate bare (no title, no subtitle) versions


for (i in seq_along(all_plots_list)) {
  
  original_plot <- all_plots_list[[i]]
  dims <- all_plots_dims[[i]]
  
  bare_plot <- original_plot +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank()
    )
  
  filename <- file.path(PLOTS_DIR, sprintf("step5_plot%d_bare.png", i))
  ggsave(filename, bare_plot, width = dims[1], height = dims[2], dpi = 300, bg = "white")
  
  cat(sprintf("Saved: %s\n", filename))
}

cat("\n\nAll 8 bare (title-free, subtitle-free) versions generated in plots/\n")
cat("alongside the original subtitled versions (filenames: step5_plotN_bare.png).\n")
cat("Caption text should be handled in the manuscript document, not the image.\n")