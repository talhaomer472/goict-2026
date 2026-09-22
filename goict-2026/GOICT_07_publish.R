# ======================================================================
# GOICT-2026 — SCRIPT 07
# PUBLISH: compute the provisional extension and write the status file
# the app reads. Run after script 06. Needs GOICT in memory.
#
# The provisional figure uses CALENDAR days with the last price carried
# forward, exactly like the headline. (Script 04's version summed
# trading days only and understated it by roughly 30%.)
# ======================================================================

library(dplyr)

OUT <- "GOICT_2026_output"
stopifnot(exists("GOICT"))

R <- readRDS(file.path(OUT, "GOICT_results.rds"))
H <- readRDS(file.path(OUT, "GOICT_headline.rds"))

P0 <- R$benchmark
W0 <- R$window_start
W1 <- R$window_end

# ---- calendar-day price path after the window ------------------------
last_price_date <- max(GOICT$brent$date)

post <-
  data.frame(date = seq(W1 + 1, last_price_date, by = "day")) |>
  left_join(GOICT$brent, by = "date") |>
  arrange(date)

# carry the last observed price forward (weekends, holidays)
if (is.na(post$brent_usd_bbl[1])) {
  post$brent_usd_bbl[1] <-
    tail(GOICT$brent$brent_usd_bbl[GOICT$brent$date <= W1], 1)
}
for (i in seq_len(nrow(post))[-1]) {
  if (is.na(post$brent_usd_bbl[i]))
    post$brent_usd_bbl[i] <- post$brent_usd_bbl[i - 1]
}

price_integral_post <- sum(post$brent_usd_bbl - P0)

# ---- latest reported volume per economy ------------------------------
q_latest <-
  GOICT$imports_actual |>
  filter(is.finite(imports_actual_kbd)) |>
  group_by(iso2) |>
  slice_max(month, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(iso2, q_latest = imports_actual_kbd)

prov <-
  R$results |>
  select(iso2, mean_imports_kbd) |>
  left_join(q_latest, by = "iso2") |>
  mutate(q_use = coalesce(q_latest, mean_imports_kbd),
         extra = q_use * 1000 * price_integral_post)

PROV_EXTRA <- sum(prov$extra, na.rm = TRUE)

# ---- largest daily Brent move in the last 90 days (parse-error check)
recent <- GOICT$brent |>
  filter(date >= last_price_date - 90) |>
  arrange(date) |>
  mutate(pct = 100 * abs(brent_usd_bbl / lag(brent_usd_bbl) - 1))

status <- list(
  vintage               = R$vintage,
  created_utc           = format(Sys.time(), tz = "UTC", usetz = TRUE),
  window_start          = W0,
  window_end            = W1,
  n_days                = R$n_days,
  benchmark             = P0,
  reported_usd          = R$global_additional_usd,
  total_bill_usd        = R$global_total_usd,
  provisional_extra_usd = PROV_EXTRA,
  provisional_to        = last_price_date,
  n_economies           = nrow(R$results),
  n_positive            = sum(R$results$additional_usd > 0),
  identity_gap          = GOICT$checks$identity_gap,
  sum_gap               = GOICT$checks$sum_gap,
  brent_max_jump_pct    = max(recent$pct, na.rm = TRUE)
)

saveRDS(status, file.path(OUT, "GOICT_app_status.rds"))

cat("\n=== PUBLISH ===\n")
cat("Window:      ", format(W0), "to", format(W1), "\n")
cat("Reported:    $", round(status$reported_usd / 1e9, 1), " bn\n", sep = "")
cat("Provisional: +$", round(PROV_EXTRA / 1e9, 1), " bn to ",
    format(last_price_date), " (calendar days)\n", sep = "")
cat("Combined:    $", round((status$reported_usd + PROV_EXTRA) / 1e9, 1),
    " bn\n", sep = "")
