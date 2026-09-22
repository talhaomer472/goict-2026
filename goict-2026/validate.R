# ======================================================================
# GOICT-2026 — checks that must pass before anything is published.
# Exits with an error (and the workflow stops) if any fail, leaving the
# live app on the last good data.
# ======================================================================

OUT <- "GOICT_2026_output"
S   <- readRDS(file.path(OUT, "GOICT_app_status.rds"))
P   <- if (file.exists("prev_status.rds")) readRDS("prev_status.rds") else NULL

fail <- character(0)
chk  <- function(ok, msg) {
  cat(if (ok) "  PASS  " else "  FAIL  ", msg, "\n", sep = "")
  if (!ok) fail <<- c(fail, msg)
}

cat("\n=== VALIDATION ===\n")

for (f in c("GOICT_results.rds", "GOICT_decomposition.rds",
            "GOICT_headline.rds", "GOICT_app_status.rds"))
  chk(file.exists(file.path(OUT, f)), paste("file present:", f))

chk(S$identity_gap < 1,   "decomposition identity holds")
chk(S$sum_gap < 1,        "country totals equal the global total")
chk(S$n_economies >= 50,  sprintf("at least 50 economies (%d)", S$n_economies))
chk(is.finite(S$reported_usd) && S$reported_usd > 0,
    "reported total is positive")
chk(is.finite(S$provisional_extra_usd), "provisional figure is finite")
chk(S$brent_max_jump_pct < 20,
    sprintf("no Brent daily move above 20%% in 90 days (max %.1f%%)",
            S$brent_max_jump_pct))

if (!is.null(P)) {
  same_window <- identical(as.Date(S$window_end), as.Date(P$window_end))
  if (same_window) {
    ch <- abs(S$reported_usd / P$reported_usd - 1)
    chk(ch <= 0.25,
        sprintf("headline moved %.1f%% with the same window (limit 25%%)",
                100 * ch))
  } else {
    cat("  INFO  window moved from ", format(P$window_end), " to ",
        format(S$window_end), "\n", sep = "")
  }
}

if (length(fail)) {
  cat("\nVALIDATION FAILED. Nothing will be published.\n")
  quit(status = 1)
}
cat("\nAll checks passed.\n")
