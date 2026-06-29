
# STEP 3B: NHANES Multi-Cycle Header Discovery

# Purpose: before writing the actual pooling/harmonization script, we need
# to know exactly how variable names, factor levels, and table availability
# differ across cycles. Rather than discovering mismatches one error at a
# time (like Step 1), this script pulls every relevant table from every
# target cycle and dumps the column names + first-few factor levels for
# each, so the harmonization script can be written once, correctly, using
# REAL information instead of assumptions.
#
# Cycles covered: 2011-2012 (G), 2013-2014 (H), 2015-2016 (I), 2017-2018 (J)
# (2019-2020 is excluded - that cycle was disrupted by COVID and NHANES
# combined it with a partial 2017-2020 file with different methodology;
# better to leave it out unless you specifically want to investigate that
# disruption as its own angle later.)
#
# OUTPUT: a single text file, "nhanes_cycle_header_report.txt", containing
# every table's column names and (for likely-categorical columns) a preview
# of factor levels, organized by cycle and table. Send this back and the
# actual harmonization script will be built directly from it.


library(nhanesA)
library(tidyverse)

cycles <- c(G = "2011-2012", H = "2013-2014", I = "2015-2016", J = "2017-2018")
tables_needed <- c("DEMO", "DLQ", "PFQ", "MCQ", "HIQ", "HUQ", "OCQ")

output_lines <- c()
add_line <- function(...) {
  output_lines <<- c(output_lines, sprintf(...))
}

add_line("============================================================")
add_line("NHANES MULTI-CYCLE HEADER REPORT")
add_line("Generated: %s", as.character(Sys.time()))
add_line("============================================================\n")

for (suffix in names(cycles)) {
  cycle_label <- cycles[suffix]
  add_line("\n##############################")
  add_line("CYCLE: %s (suffix _%s)", cycle_label, suffix)
  add_line("##############################")

  for (tbl_prefix in tables_needed) {
    tbl_name <- paste0(tbl_prefix, "_", suffix)

    add_line("\n-- %s --", tbl_name)

    result <- tryCatch({
      dat <- nhanes(tbl_name)
      list(success = TRUE, data = dat)
    }, error = function(e) {
      list(success = FALSE, error = as.character(e))
    })

    if (!result$success) {
      add_line("  FAILED TO PULL: %s", result$error)
      add_line("  (Table may not exist for this cycle, or may have a different name)")
      next
    }

    dat <- result$data
    add_line("  Rows: %d | Columns: %d", nrow(dat), ncol(dat))
    add_line("  Column names: %s", paste(names(dat), collapse = ", "))

    # For each column, report its class and (if factor/character) a preview
    # of unique values - this is what catches the "text labels instead of
    # numeric codes" problem we kept hitting in Step 1, before it becomes a
    # debugging session instead of a known fact going into script-writing.
    add_line("\n  Column details:")
    for (col in names(dat)) {
      col_class <- class(dat[[col]])[1]
      if (col_class %in% c("factor", "character")) {
        uniq_vals <- unique(as.character(dat[[col]]))
        preview <- paste(head(uniq_vals, 8), collapse = " | ")
        if (length(uniq_vals) > 8) preview <- paste0(preview, " | ... (", length(uniq_vals), " total)")
        add_line("    %-20s [%s] values: %s", col, col_class, preview)
      } else {
        rng <- tryCatch(range(dat[[col]], na.rm = TRUE), error = function(e) c(NA, NA))
        add_line("    %-20s [%s] range: %s to %s", col, col_class, rng[1], rng[2])
      }
    }
  }
}

# Write to file
writeLines(output_lines, "nhanes_cycle_header_report.txt")

cat(sprintf("\n\nDone. Report written to: nhanes_cycle_header_report.txt\n"))
cat(sprintf("Total lines: %d\n", length(output_lines)))
