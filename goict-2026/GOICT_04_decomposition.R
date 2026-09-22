# ======================================================================
# GOICT-2026 — SCRIPT 04
# DECOMPOSITION, SUPPLY SHORTFALL, SCENARIOS, CONSEQUENCES
#
# Run after GOICT_02a_results.R. Needs GOICT in memory.
#
# The analytical core of the paper. All arithmetic, no estimation.
#
#   1  Pre-conflict baseline volumes
#   2  Price-volume decomposition:
#        exposure at pre-conflict volumes vs realised expenditure
#   3  Supply shortfall in barrels
#   4  Who absorbed through price, who through supply
#   5  Scenarios bounded by OBSERVED volume behaviour
#   6  Projection from 1 July to the present
#   7  Consequences: reserves, import cover
#   8  Sentences for the manuscript
# ======================================================================

library(tidyverse)
library(lubridate)

OUTDIR <- "GOICT_2026_output"

stopifnot(exists("GOICT"))

R       <- readRDS(file.path(OUTDIR, "GOICT_results.rds"))
H       <- readRDS(file.path(OUTDIR, "GOICT_headline.rds"))
results <- R$results

P0           <- R$benchmark
WINDOW_START <- R$window_start
WINDOW_END   <- R$window_end
N_DAYS       <- R$n_days

rule <- function(t) {
  cat("\n", strrep("=", 70), "\n", t, "\n", strrep("=", 70), "\n", sep = "")
}


# ======================================================================
# 1. PRE-CONFLICT BASELINE VOLUMES
#
# Mean of the three reported months before the conflict began.
# Actual data only, no imputation.
# ======================================================================

BASE_MONTHS <- as.Date(c("2025-12-01", "2026-01-01", "2026-02-01"))

baseline <-
  GOICT$imports_actual |>
  filter(month %in% BASE_MONTHS, is.finite(imports_actual_kbd)) |>
  group_by(iso2, country, iso3) |>
  summarise(
    q0_kbd    = mean(imports_actual_kbd),
    n_base    = dplyr::n(),
    .groups   = "drop"
  ) |>
  filter(n_base >= 2)

cat("\nBaseline built for", nrow(baseline), "countries",
    "(needs at least 2 of Dec-Feb reported).\n")


# ======================================================================
# 2. PRICE-VOLUME DECOMPOSITION
#
#   Exposure  = sum_t  Q0  * (P_t - P0)   what the shock would have cost
#                                          at pre-conflict volumes
#   Realised  = sum_t  Q_t * (P_t - P0)   what was actually paid extra
#   Avoided   = Exposure - Realised        reduction achieved by
#                                          importing less
#
# Avoided is negative for countries that increased imports.
# ======================================================================

price_gap <-
  H$daily |>
  distinct(date, brent_usd_bbl) |>
  mutate(gap = brent_usd_bbl - P0)

PRICE_INTEGRAL <- sum(price_gap$gap)

cat("Price integral over the window:",
    round(PRICE_INTEGRAL, 1), "USD-days per barrel\n")

decomp <-
  H$daily |>
  inner_join(baseline |> select(iso2, q0_kbd), by = "iso2") |>
  mutate(gap = brent_usd_bbl - P0) |>
  group_by(iso2, country, iso3) |>
  summarise(
    q0_kbd        = first(q0_kbd),
    q_mean_kbd    = mean(imports_kbd_used, na.rm = TRUE),
    realised_usd  = sum(imports_kbd_used * 1000 * gap, na.rm = TRUE),
    exposure_usd  = sum(q0_kbd          * 1000 * gap, na.rm = TRUE),
    barrels_q0    = sum(q0_kbd * 1000),
    barrels_actual= sum(imports_kbd_used * 1000, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    avoided_usd        = exposure_usd - realised_usd,
    pct_avoided        = 100 * avoided_usd / exposure_usd,
    volume_change_pct  = 100 * (q_mean_kbd - q0_kbd) / q0_kbd,

    # Barrels not received relative to the pre-conflict baseline
    shortfall_barrels  = barrels_q0 - barrels_actual,
    shortfall_mb       = shortfall_barrels / 1e6,

    exposure_bn = exposure_usd / 1e9,
    realised_bn = realised_usd / 1e9,
    avoided_bn  = avoided_usd  / 1e9
  ) |>
  left_join(
    results |> select(iso2, pop, gdp_usd, exports, reserves, income),
    by = "iso2"
  ) |>
  mutate(
    exposure_per_capita = exposure_usd / pop,
    realised_per_capita = realised_usd / pop
  ) |>
  arrange(desc(exposure_usd))

GLOBAL_EXPOSURE <- sum(decomp$exposure_usd)
GLOBAL_REALISED <- sum(decomp$realised_usd)
GLOBAL_AVOIDED  <- GLOBAL_EXPOSURE - GLOBAL_REALISED
GLOBAL_SHORTFALL<- sum(decomp$shortfall_barrels)

rule("2. PRICE-VOLUME DECOMPOSITION, GLOBAL")

cat("\nCountries with a usable baseline:", nrow(decomp), "\n")
cat("\nExposure at pre-conflict volumes: $",
    round(GLOBAL_EXPOSURE / 1e9, 1), " bn\n", sep = "")
cat("Realised additional expenditure:  $",
    round(GLOBAL_REALISED / 1e9, 1), " bn\n", sep = "")
cat("Avoided by importing less:        $",
    round(GLOBAL_AVOIDED / 1e9, 1), " bn (",
    round(100 * GLOBAL_AVOIDED / GLOBAL_EXPOSURE, 1), "%)\n", sep = "")
cat("\nCrude not received vs baseline:   ",
    round(GLOBAL_SHORTFALL / 1e6, 1), " million barrels\n", sep = "")
cat("Equivalent to                     ",
    round(GLOBAL_SHORTFALL / 1e6 / N_DAYS / 1000, 2), " mb/d\n", sep = "")

cat("\nINTERPRETATION: the world did not simply pay the full price\n")
cat("increase. It paid part and went without the rest. Both halves\n")
cat("are costs, and only the first appears in an expenditure measure.\n")


rule("2b. DECOMPOSITION BY COUNTRY (top 25 by exposure)")

print(
  decomp |>
    transmute(
      country,
      q0_kbd      = round(q0_kbd),
      q_actual    = round(q_mean_kbd),
      vol_chg_pct = round(volume_change_pct, 1),
      exposure_bn = round(exposure_bn, 2),
      realised_bn = round(realised_bn, 2),
      avoided_bn  = round(avoided_bn, 2),
      pct_avoided = round(pct_avoided, 1)
    ) |>
    slice_head(n = 25),
  n = Inf, width = Inf
)


# ======================================================================
# 3. SUPPLY SHORTFALL
# ======================================================================

rule("3. LARGEST SUPPLY SHORTFALLS (barrels not received)")

print(
  decomp |>
    filter(shortfall_mb > 0) |>
    arrange(desc(shortfall_mb)) |>
    transmute(
      country,
      shortfall_mb = round(shortfall_mb, 1),
      vol_chg_pct  = round(volume_change_pct, 1),
      shortfall_per_capita_bbl = round(shortfall_barrels / pop, 2),
      avoided_bn   = round(avoided_bn, 2)
    ) |>
    slice_head(n = 20),
  n = Inf, width = Inf
)

cat("\nCountries that INCREASED imports during the conflict:\n")
print(
  decomp |>
    filter(volume_change_pct > 0) |>
    arrange(desc(volume_change_pct)) |>
    transmute(
      country,
      vol_chg_pct = round(volume_change_pct, 1),
      exposure_bn = round(exposure_bn, 2),
      realised_bn = round(realised_bn, 2),
      extra_paid_bn = round(-avoided_bn, 2)
    ),
  n = Inf, width = Inf
)


# ======================================================================
# 4. ABSORPTION MODE
#
# A simple two-way split, no estimation. Did a country absorb the
# shock through expenditure, through reduced supply, or both?
# ======================================================================

rule("4. HOW THE SHOCK WAS ABSORBED")

absorption <-
  decomp |>
  mutate(
    mode = case_when(
      volume_change_pct >   2 ~ "Bought more",
      volume_change_pct >  -5 ~ "Paid the price",
      volume_change_pct > -20 ~ "Partly adjusted",
      TRUE                    ~ "Went without"
    )
  )

print(
  absorption |>
    group_by(mode) |>
    summarise(
      n              = dplyr::n(),
      exposure_bn    = round(sum(exposure_usd) / 1e9, 1),
      realised_bn    = round(sum(realised_usd) / 1e9, 1),
      avoided_bn     = round(sum(avoided_usd)  / 1e9, 1),
      shortfall_mb   = round(sum(shortfall_barrels) / 1e6, 1),
      .groups = "drop"
    ),
  width = Inf
)

cat("\nCountries in each group:\n")
for (m in unique(absorption$mode)) {
  cat("\n", m, ":\n  ", sep = "")
  cat(paste(absorption$country[absorption$mode == m], collapse = ", "),
      "\n")
}


# ======================================================================
# 5. SCENARIOS BOUNDED BY OBSERVED BEHAVIOUR
#
# No elasticities, no model. Two bounds, both from your own data:
#   HIGH  volumes at the pre-conflict baseline Q0
#   LOW   volumes at the conflict-period mean actually observed
# ======================================================================

rule("5. FORWARD SCENARIOS, BOUNDED BY OBSERVED VOLUMES")

SCEN_PRICES <- c(90, 105, 120, 150)
SCEN_DAYS   <- c(90, 180, 365)

global_q0 <- sum(decomp$q0_kbd)
global_qa <- sum(decomp$q_mean_kbd)

cat("\nGlobal crude imports, pre-conflict baseline:",
    round(global_q0 / 1000, 2), "mb/d\n")
cat("Global crude imports, conflict-period mean: ",
    round(global_qa / 1000, 2), "mb/d\n")
cat("\nHIGH bound holds volumes at the baseline.\n")
cat("LOW bound holds them at the observed conflict-period level.\n")
cat("Neither is a forecast.\n\n")

scen_global <-
  expand_grid(price = SCEN_PRICES, days = SCEN_DAYS) |>
  mutate(
    high_bn = global_q0 * 1000 * (price - P0) * days / 1e9,
    low_bn  = global_qa * 1000 * (price - P0) * days / 1e9
  ) |>
  arrange(days, price)

print(
  scen_global |>
    transmute(
      `Brent ($/bbl)` = price,
      `Horizon (days)` = days,
      `Low ($bn)`  = round(low_bn, 0),
      `High ($bn)` = round(high_bn, 0)
    ),
  n = Inf, width = Inf
)

# Country-level scenario at a single focal case
FOCAL_PRICE <- 120
FOCAL_DAYS  <- 180

cat("\nCountry detail at $", FOCAL_PRICE, " for ", FOCAL_DAYS,
    " days:\n\n", sep = "")

scen_country <-
  decomp |>
  mutate(
    high_bn = q0_kbd     * 1000 * (FOCAL_PRICE - P0) * FOCAL_DAYS / 1e9,
    low_bn  = q_mean_kbd * 1000 * (FOCAL_PRICE - P0) * FOCAL_DAYS / 1e9,
    high_per_capita = high_bn * 1e9 / pop,
    low_per_capita  = low_bn  * 1e9 / pop,
    high_pct_exports = 100 * high_bn * 1e9 / exports
  ) |>
  arrange(desc(high_bn))

print(
  scen_country |>
    transmute(
      country,
      low_bn  = round(low_bn, 1),
      high_bn = round(high_bn, 1),
      low_per_capita  = round(low_per_capita, 0),
      high_per_capita = round(high_per_capita, 0),
      high_pct_exports = round(high_pct_exports, 2)
    ) |>
    slice_head(n = 20),
  n = Inf, width = Inf
)


# ======================================================================
# 6. PROJECTION FROM 1 JULY TO THE PRESENT
#
# The headline window closes 30 June because that is where reported
# volumes end. Prices did not stop. This shows what has accrued since,
# using June volumes. Label it clearly as a projection.
# ======================================================================

rule("6. PROJECTION SINCE THE HEADLINE WINDOW")

post <-
  GOICT$brent |>
  filter(date > WINDOW_END) |>
  mutate(gap = brent_usd_bbl - P0)

if (nrow(post) > 0) {

  post_integral <- sum(post$gap)
  post_days     <- as.integer(max(post$date) - WINDOW_END)

  # Latest reported monthly volume per country
  q_latest <-
    GOICT$imports_actual |>
    group_by(iso2) |>
    slice_max(month, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(iso2, q_latest_kbd = imports_actual_kbd)

  post_est <-
    decomp |>
    left_join(q_latest, by = "iso2") |>
    mutate(
      q_use = coalesce(q_latest_kbd, q_mean_kbd),
      post_usd = q_use * 1000 * post_integral
    )

  cat("\nPeriod: ", as.character(WINDOW_END + 1), " to ",
      as.character(max(post$date)), " (", post_days, " days)\n", sep = "")
  cat("Mean Brent:        $", round(mean(post$brent_usd_bbl), 2),
      "\n", sep = "")
  cat("Latest Brent:      $",
      round(post$brent_usd_bbl[which.max(post$date)], 2), "\n", sep = "")
  cat("\nPROJECTED additional expenditure since 30 June: $",
      round(sum(post_est$post_usd, na.rm = TRUE) / 1e9, 1),
      " bn\n", sep = "")
  cat("Reported headline (Feb-Jun):                    $",
      round(GLOBAL_REALISED / 1e9, 1), " bn\n", sep = "")
  cat("Combined, reported plus projected:              $",
      round((GLOBAL_REALISED +
             sum(post_est$post_usd, na.rm = TRUE)) / 1e9, 1),
      " bn\n", sep = "")
  cat("\nThe projection uses each country's most recent reported\n")
  cat("monthly volume. It is NOT part of the headline figure.\n")

} else {
  cat("\nNo Brent observations after the window end.\n")
}


# ======================================================================
# 7. CONSEQUENCES
#
# What the drain means, using only stock and flow ratios.
# ======================================================================

rule("7. CONSEQUENCES: FOREIGN-EXCHANGE DRAIN")

cat("\nThe additional bill is foreign exchange leaving the country.\n")
cat("Against reserves it shows the balance-of-payments pressure.\n\n")

print(
  results |>
    filter(!is.na(reserves), additional_usd > 0) |>
    mutate(
      pct_reserves = 100 * additional_usd / reserves,
      annualised_pct_reserves = pct_reserves * 365 / N_DAYS
    ) |>
    arrange(desc(pct_reserves)) |>
    transmute(
      country,
      pct_reserves = round(pct_reserves, 2),
      annualised   = round(annualised_pct_reserves, 2),
      pct_exports  = round(pct_exports, 2),
      income
    ) |>
    slice_head(n = 20),
  n = Inf, width = Inf
)

cat("\nNOTE FOR THE DISCUSSION: where this lands domestically differs.\n")
cat("Countries with administered fuel prices or large subsidy regimes\n")
cat("absorb it into the budget; others pass it to consumers. Pass-through\n")
cat("is not measured here and should not be claimed.\n")


# ======================================================================
# 8. SENTENCES
# ======================================================================

rule("8. SENTENCES FOR THE MANUSCRIPT")

jp <- decomp |> filter(country == "Japan")
it <- decomp |> filter(country == "Italy")
cn <- decomp |> filter(country == "China")

cat("\n--- DECOMPOSITION ---\n")
cat("At pre-conflict import volumes, the price increase between ",
    format(WINDOW_START, "%d %B"), " and ", format(WINDOW_END, "%d %B %Y"),
    "\nwould have cost the sample $", round(GLOBAL_EXPOSURE / 1e9, 1),
    " billion. Realised additional expenditure was\n$",
    round(GLOBAL_REALISED / 1e9, 1), " billion. The difference of $",
    round(GLOBAL_AVOIDED / 1e9, 1),
    " billion was not saved but foregone:\nimporters took delivery of ",
    round(GLOBAL_SHORTFALL / 1e6, 0),
    " million fewer barrels than the pre-conflict\nbaseline implies.\n",
    sep = "")

if (nrow(jp) == 1 && nrow(it) == 1) {
  cat("\n--- HETEROGENEITY ---\n")
  cat("Adjustment varied sharply. Japan's crude imports fell ",
      abs(round(jp$volume_change_pct, 0)),
      "% below baseline,\navoiding $", round(jp$avoided_bn, 1),
      " billion in expenditure while foregoing ",
      round(jp$shortfall_mb, 0),
      " million barrels.\nItaly's imports rose ",
      round(it$volume_change_pct, 0),
      "%, so it paid $", round(-it$avoided_bn, 1),
      " billion more than its\npre-conflict exposure implied.\n", sep = "")
}

if (nrow(cn) == 1) {
  cat("\n--- CHINA ---\n")
  cat("China, the largest importer, reduced volumes ",
      abs(round(cn$volume_change_pct, 0)),
      "% and avoided\n$", round(cn$avoided_bn, 1),
      " billion, the largest adjustment in the sample.\n", sep = "")
}

cat("\n--- CONTRIBUTION ---\n")
cat("Existing assessments of the 2026 shock model prospective output\n")
cat("effects. This note measures the realised expenditure and the\n")
cat("realised supply shortfall, country by country, from reported\n")
cat("volumes and observed prices, and separates the two.\n")


# ======================================================================
# SAVE
# ======================================================================

saveRDS(
  list(
    decomp   = decomp,
    baseline = baseline,
    absorption = absorption,
    scen_global  = scen_global,
    scen_country = scen_country,
    global = list(
      exposure  = GLOBAL_EXPOSURE,
      realised  = GLOBAL_REALISED,
      avoided   = GLOBAL_AVOIDED,
      shortfall = GLOBAL_SHORTFALL
    )
  ),
  file.path(OUTDIR, "GOICT_decomposition.rds")
)

write_csv(decomp,       file.path(OUTDIR, "GOICT_decomposition.csv"))
write_csv(scen_global,  file.path(OUTDIR, "GOICT_scenarios_global.csv"))
write_csv(scen_country, file.path(OUTDIR, "GOICT_scenarios_country.csv"))

cat("\n", strrep("=", 70), "\n", sep = "")
cat("Saved GOICT_decomposition.rds/.csv and scenario tables\n")
cat(strrep("=", 70), "\n\n", sep = "")
