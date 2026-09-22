# ======================================================================
# GOICT-2026 — SCRIPT 06 (corrected)
# FIXES AND 600 DPI
#
# Run after GOICT_04_decomposition.R. Needs GOICT in memory.
#
#   0  Structure diagnostic
#   1  Corrects the supply-shortfall bug in script 04
#   2  Flags baseline artifacts
#   3  Verifies the Brent tail used by the projection
#   4  Re-saves every figure at 600 dpi
#
# Exposure, realised and avoided from script 04 were CORRECT.
# Only the barrel counts were wrong.
# ======================================================================

library(tidyverse)
library(lubridate)

OUTDIR <- "GOICT_2026_output"
DPI    <- 600

stopifnot(exists("GOICT"))

R <- readRDS(file.path(OUTDIR, "GOICT_results.rds"))
H <- readRDS(file.path(OUTDIR, "GOICT_headline.rds"))
D <- readRDS(file.path(OUTDIR, "GOICT_decomposition.rds"))

N_DAYS <- R$n_days

rule <- function(t) {
  cat("\n", strrep("=", 70), "\n", t, "\n", strrep("=", 70), "\n", sep = "")
}


# ======================================================================
# 0. DIAGNOSTIC
#
# Last run failed because a join did not deliver its column. Print the
# structures so any further mismatch is visible rather than silent.
# ======================================================================

rule("0. STRUCTURE DIAGNOSTIC")

cat("\nD$decomp rows:", nrow(D$decomp), "\n")
cat("D$decomp columns:\n  ")
cat(paste(names(D$decomp), collapse = ", "), "\n")

cat("\nH$daily rows:", nrow(H$daily), "\n")
cat("H$daily columns:\n  ")
cat(paste(names(H$daily), collapse = ", "), "\n")

cat("\nR$results has 'days' column:", "days" %in% names(R$results), "\n")
cat("N_DAYS from results file:", N_DAYS, "\n")


# ======================================================================
# 1. CORRECTED SHORTFALL
#
# Bug: barrels_q0 was computed after q0_kbd had been reassigned inside
# summarise(), so it held one day's volume rather than the window's.
#
# Correct: (Q0 - Q_actual) barrels/day x 1000 x days in window.
#
# The day count is derived three ways, in order of preference, so a
# failed join cannot silently produce a missing column again.
# ======================================================================

days_lookup <-
  if ("days" %in% names(R$results)) {
    R$results |> select(iso2, n_days = days)
  } else {
    H$daily |>
      group_by(iso2) |>
      summarise(n_days = n_distinct(date), .groups = "drop")
  }

decomp <-
  D$decomp |>
  select(-any_of(c("shortfall_barrels", "shortfall_mb",
                   "barrels_q0", "barrels_actual",
                   "shortfall_bbl_per_capita",
                   "n_days", "days_in_window"))) |>
  left_join(days_lookup, by = "iso2") |>
  mutate(
    n_days = coalesce(n_days, N_DAYS)   # fallback, never NA
  )

stopifnot(!any(is.na(decomp$n_days)))

decomp <-
  decomp |>
  mutate(
    barrels_q0        = q0_kbd     * 1000 * n_days,
    barrels_actual    = q_mean_kbd * 1000 * n_days,
    shortfall_barrels = barrels_q0 - barrels_actual,
    shortfall_mb      = shortfall_barrels / 1e6,
    shortfall_bbl_per_capita = shortfall_barrels / pop
  )

GLOBAL_SHORTFALL <- sum(decomp$shortfall_barrels, na.rm = TRUE)

rule("1. CORRECTED SUPPLY SHORTFALL")

cat("\nGlobal crude not received vs pre-conflict baseline:\n")
cat("  ", round(GLOBAL_SHORTFALL / 1e6, 1), " million barrels\n", sep = "")
cat("  ", round(GLOBAL_SHORTFALL / 1000 / N_DAYS / 1000, 2),
    " mb/d average over the window\n", sep = "")

cat("\nIndependent check:\n")
cat("  baseline ", round(sum(decomp$q0_kbd) / 1000, 2), " mb/d\n", sep = "")
cat("  actual   ", round(sum(decomp$q_mean_kbd) / 1000, 2), " mb/d\n", sep = "")
cat("  gap x ", N_DAYS, " days = ",
    round((sum(decomp$q0_kbd) - sum(decomp$q_mean_kbd)) *
          1000 * N_DAYS / 1e6, 1), " million barrels\n", sep = "")

cat("\nLargest shortfalls:\n")
print(
  decomp |>
    filter(shortfall_mb > 0) |>
    arrange(desc(shortfall_mb)) |>
    transmute(
      country,
      shortfall_mb = round(shortfall_mb, 1),
      vol_chg_pct  = round(volume_change_pct, 1),
      bbl_per_head = round(shortfall_bbl_per_capita, 2),
      avoided_bn   = round(avoided_bn, 2)
    ) |>
    slice_head(n = 20),
  n = Inf, width = Inf
)


# ======================================================================
# 2. BASELINE ARTIFACT SCREEN
#
# A country whose baseline rests on partial December-February reporting
# shows an implausible volume change. Those are data problems, not
# behaviour, and must not be reported as adjustment.
# ======================================================================

rule("2. BASELINE ARTIFACT SCREEN")

base_detail <-
  GOICT$imports_actual |>
  filter(month %in% as.Date(c("2025-12-01", "2026-01-01",
                              "2026-02-01"))) |>
  group_by(iso2) |>
  summarise(
    n_base_months = sum(is.finite(imports_actual_kbd)),
    base_min = suppressWarnings(min(imports_actual_kbd, na.rm = TRUE)),
    base_max = suppressWarnings(max(imports_actual_kbd, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(
    base_spread_pct =
      if_else(is.finite(base_max) & base_max > 0,
              100 * (base_max - base_min) / base_max,
              NA_real_)
  )

decomp <-
  decomp |>
  select(-any_of(c("n_base_months", "base_min", "base_max",
                   "base_spread_pct", "artifact_flag",
                   "decomp_sample"))) |>
  left_join(base_detail, by = "iso2") |>
  mutate(
    artifact_flag =
      coalesce(n_base_months, 0L) < 3 |
      abs(volume_change_pct) > 50 |
      coalesce(base_spread_pct, 0) > 60,

    decomp_sample = !artifact_flag & q0_kbd >= 50
  )

cat("\nFLAGGED — do not report these as adjustment behaviour:\n")
print(
  decomp |>
    filter(artifact_flag) |>
    arrange(desc(abs(volume_change_pct))) |>
    transmute(
      country,
      q0_kbd      = round(q0_kbd),
      q_actual    = round(q_mean_kbd),
      vol_chg_pct = round(volume_change_pct, 1),
      n_base_months,
      base_spread = round(base_spread_pct, 0),
      exposure_bn = round(exposure_bn, 2),
      avoided_bn  = round(avoided_bn, 2)
    ),
  n = Inf, width = Inf
)

clean <- decomp |> filter(decomp_sample)

cat("\nClean decomposition sample:", nrow(clean),
    "of", nrow(decomp), "countries\n")

cat("\n--- DECOMPOSITION, CLEAN SAMPLE (report as headline) ---\n")
cat("Exposure at pre-conflict volumes: $",
    round(sum(clean$exposure_usd) / 1e9, 1), " bn\n", sep = "")
cat("Realised additional expenditure:  $",
    round(sum(clean$realised_usd) / 1e9, 1), " bn\n", sep = "")
cat("Avoided by importing less:        $",
    round(sum(clean$avoided_usd) / 1e9, 1), " bn (",
    round(100 * sum(clean$avoided_usd) / sum(clean$exposure_usd), 1),
    "%)\n", sep = "")
cat("Crude not received:               ",
    round(sum(clean$shortfall_barrels) / 1e6, 0),
    " million barrels\n", sep = "")

cat("\n--- FULL SAMPLE (report as robustness) ---\n")
cat("Exposure $", round(sum(decomp$exposure_usd) / 1e9, 1),
    " bn, realised $", round(sum(decomp$realised_usd) / 1e9, 1),
    " bn, avoided $", round(sum(decomp$avoided_usd) / 1e9, 1),
    " bn\n", sep = "")
cat("Crude not received ",
    round(sum(decomp$shortfall_barrels) / 1e6, 0),
    " million barrels\n", sep = "")

cat("\nKey countries (for the manuscript):\n")
print(
  decomp |>
    filter(country %in% c("China", "Japan", "South Korea", "India",
                          "United States", "Italy", "Germany",
                          "United Kingdom", "Singapore")) |>
    transmute(
      country,
      vol_chg_pct  = round(volume_change_pct, 1),
      exposure_bn  = round(exposure_bn, 2),
      realised_bn  = round(realised_bn, 2),
      avoided_bn   = round(avoided_bn, 2),
      shortfall_mb = round(shortfall_mb, 1),
      artifact_flag
    ),
  n = Inf, width = Inf
)


# ======================================================================
# 3. BRENT TAIL CHECK
#
# The projection reported a latest Brent of $130.80 on 2026-09-15,
# inconsistent with market levels reported in mid-September. Inspect
# the raw tail before using that projection.
# ======================================================================

rule("3. BRENT TAIL — VERIFY AGAINST EIA BEFORE PUBLISHING")

cat("\nLast 20 Brent observations:\n")
print(GOICT$brent |> slice_tail(n = 20), n = Inf)

brent_post <- GOICT$brent |> filter(date > R$window_end)

cat("\nSince window end (", nrow(brent_post), " observations):\n", sep = "")
cat("  min  $", round(min(brent_post$brent_usd_bbl), 2), "\n", sep = "")
cat("  max  $", round(max(brent_post$brent_usd_bbl), 2), "\n", sep = "")
cat("  mean $", round(mean(brent_post$brent_usd_bbl), 2), "\n", sep = "")

jumps <-
  GOICT$brent |>
  arrange(date) |>
  mutate(pct_chg = 100 * (brent_usd_bbl / lag(brent_usd_bbl) - 1)) |>
  filter(abs(pct_chg) > 10)

cat("\nDaily moves above 10 percent (possible parse errors):\n")
if (nrow(jumps) > 0) {
  print(jumps |> transmute(date, brent_usd_bbl,
                           pct_chg = round(pct_chg, 1)), n = Inf)
  cat("\nCheck each against the EIA series directly. One bad cell in\n")
  cat("the workbook will distort the projection.\n")
} else {
  cat("None. Series looks clean.\n")
}


# ======================================================================
# 4. SAVE
# ======================================================================

D$decomp <- decomp

D$global$shortfall       <- GLOBAL_SHORTFALL
D$global$exposure_clean  <- sum(clean$exposure_usd)
D$global$realised_clean  <- sum(clean$realised_usd)
D$global$avoided_clean   <- sum(clean$avoided_usd)
D$global$shortfall_clean <- sum(clean$shortfall_barrels)

saveRDS(D, file.path(OUTDIR, "GOICT_decomposition.rds"))
write_csv(decomp, file.path(OUTDIR, "GOICT_decomposition.csv"))


# ======================================================================
# 5. RE-SAVE FIGURES AT 600 DPI
#
# Patches dpi at source time; the scripts themselves are untouched.
# ======================================================================

if (!isTRUE(getOption("goict.ci"))) {

rule("5. RE-SAVING FIGURES AT 600 DPI")

resave_at_600 <- function(script) {

  if (!file.exists(script)) {
    cat("Not found, skipping:", script, "\n")
    return(invisible(NULL))
  }

  txt <- readLines(script, warn = FALSE)
  txt <- gsub("dpi\\s*=\\s*300", paste0("dpi = ", DPI), txt)

  tmp <- tempfile(fileext = ".R")
  writeLines(txt, tmp)

  cat("\nRe-running", script, "at", DPI, "dpi\n")

  ok <- tryCatch({
    source(tmp, local = new.env(parent = globalenv()), echo = FALSE)
    TRUE
  }, error = function(e) {
    cat("  FAILED:", conditionMessage(e), "\n")
    FALSE
  })

  invisible(ok)
}

resave_at_600("GOICT_05_figures.R")
resave_at_600("GOICT_03_maps.R")
resave_at_600("GOICT_01b_window.R")

cat("\nNOTE: 600 dpi files are roughly four times larger. Check the\n")
cat("journal's file-size limit, and consider PDF for line figures.\n")


}

cat("\n", strrep("=", 70), "\n", sep = "")
cat("Done. Send me sections 1, 2 and 3 of this output.\n")
cat(strrep("=", 70), "\n\n", sep = "")
