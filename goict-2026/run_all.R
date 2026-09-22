# ======================================================================
# GOICT-2026 — run the whole pipeline unattended
#   Rscript run_all.R
# ======================================================================

options(goict.ci = TRUE, warn = 1)
dir.create("GOICT_2026_output", showWarnings = FALSE)

# keep the previous status so validate.R can compare against it
s <- "GOICT_2026_output/GOICT_app_status.rds"
if (file.exists(s)) file.copy(s, "prev_status.rds", overwrite = TRUE)

steps <- c("GOICT_01_patches.R",        # download + measure
           "GOICT_01b_window.R",        # window and sample
           "GOICT_02a_results.R",       # denominators, results
           "GOICT_04_decomposition.R",  # baseline comparison
           "GOICT_06_fixes_600dpi.R",   # shortfall correction
           "GOICT_07_publish.R")        # provisional + status

for (f in steps) {
  message("\n>>>>>>>>>> ", f)
  source(f, echo = FALSE)
}
message("\nPipeline finished.")
