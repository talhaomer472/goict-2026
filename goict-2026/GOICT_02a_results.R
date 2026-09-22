# ======================================================================
# GOICT-2026 — SCRIPT 02a
# ALL RESULTS, PRINTED TO THE CONSOLE
#
# Run after GOICT_01_patches.R and GOICT_01b_window.R.
# Needs the GOICT object in memory and GOICT_headline.rds on disk.
#
# Prints, in order:
#   1  Headline
#   2  Imputation
#   3  Exclusions
#   4  Re-export screen (entrepot economies)
#   5  Denominator coverage
#   6  Top 25 by total additional expenditure
#   7  Top 25 per person, domestic importers only
#   8  Entrepot economies shown separately
#   9  Top 25 by share of GDP
#  10  Top 25 by share of exports
#  11  Burden by income group
#  12  Exporter mirror
#  13  Sentences for the manuscript
#
# Writes GOICT_results.rds and three CSVs. No figures, no app.
# ======================================================================

library(tidyverse)
library(lubridate)

if (!requireNamespace("WDI", quietly = TRUE)) {
  stop("Run install.packages('WDI') first, then source this again.")
}
library(WDI)

OUTDIR <- "GOICT_2026_output"
if (!dir.exists(OUTDIR)) dir.create(OUTDIR)

stopifnot(exists("GOICT"))

H <- readRDS(file.path(OUTDIR, "GOICT_headline.rds"))

P0           <- H$benchmark
WINDOW_START <- H$window_start
WINDOW_END   <- H$window_end
N_DAYS       <- as.integer(WINDOW_END - WINDOW_START) + 1

rule <- function(title) {
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat(title, "\n")
  cat(strrep("=", 70), "\n")
}


# ======================================================================
# BUILD: windowed country summary
# ======================================================================

country_window <-
  H$daily |>
  group_by(iso2, country, iso3) |>
  arrange(date, .by_group = TRUE) |>
  summarise(
    mean_imports_kbd   = mean(imports_kbd_used, na.rm = TRUE),
    latest_imports_kbd = dplyr::last(imports_kbd_used),
    additional_usd     = sum(additional_cost_usd, na.rm = TRUE),
    total_bill_usd     = sum(total_import_bill_usd, na.rm = TRUE),
    days               = dplyr::n(),
    days_imputed       = sum(quantity_status == "Estimated"),
    .groups = "drop"
  ) |>
  mutate(
    additional_billion = additional_usd / 1e9,
    total_bill_billion = total_bill_usd / 1e9,
    pct_above_benchmark = 100 * additional_usd / total_bill_usd
  )


# ======================================================================
# BUILD: crude export flow, for the re-export screen and the mirror
# ======================================================================

crude_exports <-
  GOICT$jodi_raw |>
  filter(
    ENERGY_PRODUCT == "CRUDEOIL",
    FLOW_BREAKDOWN == "TOTEXPSB",
    UNIT_MEASURE   == "KBD"
  ) |>
  mutate(
    iso2    = REF_AREA,
    month   = suppressWarnings(
      as.Date(paste0(substr(TIME_PERIOD, 1, 7), "-01"))
    ),
    exp_kbd = suppressWarnings(readr::parse_number(OBS_VALUE))
  ) |>
  filter(
    is.finite(exp_kbd),
    month >= floor_date(WINDOW_START, "month"),
    month <= floor_date(WINDOW_END, "month")
  ) |>
  group_by(iso2) |>
  summarise(mean_exports_kbd = mean(exp_kbd), .groups = "drop")


# Price integral over the window: sum of (P_t - P0) across days.
price_integral <-
  H$daily |>
  distinct(date, brent_usd_bbl) |>
  summarise(v = sum(brent_usd_bbl - P0, na.rm = TRUE)) |>
  pull(v)


# ======================================================================
# BUILD: World Bank denominators
# ======================================================================

wdi_raw <-
  WDI(
    indicator = c(
      pop      = "SP.POP.TOTL",
      gdp_usd  = "NY.GDP.MKTP.CD",
      exports  = "NE.EXP.GNFS.CD",
      reserves = "FI.RES.TOTL.CD"
    ),
    start = 2021, end = 2024, extra = TRUE
  )

wdi <-
  wdi_raw |>
  filter(!is.na(iso3c), region != "Aggregates") |>
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  summarise(
    income   = dplyr::last(stats::na.omit(income)),
    pop      = dplyr::last(stats::na.omit(pop)),
    gdp_usd  = dplyr::last(stats::na.omit(gdp_usd)),
    exports  = dplyr::last(stats::na.omit(exports)),
    reserves = dplyr::last(stats::na.omit(reserves)),
    .groups  = "drop"
  )


# ======================================================================
# BUILD: final results table
# ======================================================================

REEXPORT_THRESHOLD <- 0.20

results <-
  country_window |>
  left_join(crude_exports, by = "iso2") |>
  left_join(wdi, by = c("iso3" = "iso3c")) |>
  mutate(
    mean_exports_kbd = replace_na(mean_exports_kbd, 0),
    reexport_ratio   = mean_exports_kbd / mean_imports_kbd,
    entrepot         = reexport_ratio > REEXPORT_THRESHOLD,

    net_imports_kbd  = pmax(mean_imports_kbd - mean_exports_kbd, 0),
    net_additional_usd = net_imports_kbd * 1000 * price_integral,

    per_capita_usd     = additional_usd / pop,
    net_per_capita_usd = net_additional_usd / pop,
    pct_gdp            = 100 * additional_usd / gdp_usd,
    pct_exports        = 100 * additional_usd / exports,
    pct_reserves       = 100 * additional_usd / reserves,
    annualised_pct_gdp = pct_gdp * 365 / N_DAYS
  ) |>
  arrange(desc(additional_usd)) |>
  mutate(rank_absolute = row_number())

GLOBAL_ADD   <- sum(results$additional_usd, na.rm = TRUE)
GLOBAL_TOTAL <- sum(results$total_bill_usd, na.rm = TRUE)


# ======================================================================
# 1. HEADLINE
# ======================================================================

rule("1. HEADLINE")

cat("\nData vintage:        ", GOICT$vintage, "\n")
cat("Window:              ", as.character(WINDOW_START), "to",
    as.character(WINDOW_END), "\n")
cat("Days:                ", N_DAYS, "\n")
cat("Brent benchmark:     $", round(P0, 2), " per barrel\n", sep = "")
cat("Countries:           ", nrow(results), "\n")
cat("Mean crude imports:  ",
    round(sum(results$mean_imports_kbd) / 1000, 2), "mb/d\n")
cat("\nAdditional expenditure:  $",
    format(round(GLOBAL_ADD / 1e9, 1), big.mark = ","), " billion\n", sep = "")
cat("Total estimated bill:    $",
    format(round(GLOBAL_TOTAL / 1e9, 1), big.mark = ","), " billion\n", sep = "")
cat("Share above benchmark:   ",
    round(100 * GLOBAL_ADD / GLOBAL_TOTAL, 1), "%\n", sep = "")
cat("Per day:                 $",
    round(GLOBAL_ADD / N_DAYS / 1e9, 2), " billion\n", sep = "")

pop_covered <- sum(results$pop, na.rm = TRUE)
cat("\nPopulation covered:      ",
    round(pop_covered / 1e9, 2), "billion\n")
cat("Additional per person:   $",
    round(GLOBAL_ADD / pop_covered, 1), "\n", sep = "")


# ======================================================================
# 2. IMPUTATION
# ======================================================================

rule("2. IMPUTATION INSIDE THE WINDOW")

imp <-
  H$daily |>
  group_by(country) |>
  summarise(
    imputed_usd = sum(additional_cost_usd[quantity_status == "Estimated"],
                      na.rm = TRUE),
    months_imputed = n_distinct(
      floor_date(date[quantity_status == "Estimated"], "month")
    ),
    .groups = "drop"
  ) |>
  filter(imputed_usd > 0) |>
  arrange(desc(imputed_usd)) |>
  mutate(pct_of_global = 100 * imputed_usd / GLOBAL_ADD)

cat("\nTotal imputed value: $",
    round(sum(imp$imputed_usd) / 1e9, 2), " billion (",
    round(100 * sum(imp$imputed_usd) / GLOBAL_ADD, 1),
    "% of headline)\n", sep = "")

cat("\n")
print(imp, n = Inf)


# ======================================================================
# 3. EXCLUSIONS
# ======================================================================

rule("3. COUNTRIES EXCLUDED FROM THE HEADLINE")

cat("\nLast reported month precedes the conflict period,\n")
cat("so their whole window would be invented.\n\n")

print(H$country_status |> filter(!headline_sample), n = Inf)

cat("\nNOTE FOR THE PAPER: these are all developing economies.\n")
cat("Coverage drops out where reporting is weakest, so the\n")
cat("headline is a lower bound and understates the low-income share.\n")


# ======================================================================
# 4. RE-EXPORT SCREEN
# ======================================================================

rule("4. RE-EXPORT SCREEN (entrepot economies)")

cat("\nCrude exports as a share of crude imports over the window.\n")
cat("Above ", REEXPORT_THRESHOLD * 100,
    "% the barrels are not domestic consumption,\n", sep = "")
cat("so a per-resident figure is not meaningful.\n\n")

print(
  results |>
    filter(entrepot) |>
    arrange(desc(reexport_ratio)) |>
    transmute(
      country,
      imports_kbd = round(mean_imports_kbd),
      exports_kbd = round(mean_exports_kbd),
      reexport_pct = round(100 * reexport_ratio, 1),
      additional_billion = round(additional_billion, 2)
    ),
  n = Inf
)


# ======================================================================
# 5. DENOMINATOR COVERAGE
# ======================================================================

rule("5. DENOMINATOR COVERAGE")

cat("\nCountries with population:", sum(!is.na(results$pop)),
    "of", nrow(results), "\n")
cat("Countries with GDP:       ", sum(!is.na(results$gdp_usd)),
    "of", nrow(results), "\n")
cat("Countries with exports:   ", sum(!is.na(results$exports)),
    "of", nrow(results), "\n")

missing_denom <- results |> filter(is.na(pop) | is.na(gdp_usd))

if (nrow(missing_denom) > 0) {
  cat("\nMissing a denominator:\n")
  print(
    missing_denom |>
      transmute(country, iso3,
                additional_billion = round(additional_billion, 2)),
    n = Inf
  )
}


# ======================================================================
# 6. TOP BY TOTAL
# ======================================================================

rule("6. TOP 25 BY TOTAL ADDITIONAL EXPENDITURE")

print(
  results |>
    transmute(
      rank = rank_absolute,
      country,
      imports_kbd  = round(mean_imports_kbd),
      additional_bn = round(additional_billion, 2),
      total_bill_bn = round(total_bill_billion, 1),
      pct_above     = round(pct_above_benchmark, 1),
      per_person    = round(per_capita_usd, 1),
      pct_gdp       = round(pct_gdp, 3),
      entrepot
    ) |>
    slice_head(n = 25),
  n = Inf, width = Inf
)


# ======================================================================
# 7. TOP PER PERSON, DOMESTIC IMPORTERS
# ======================================================================

rule("7. TOP 25 PER PERSON (entrepot economies excluded)")

cat("\nWORDING FOR THE PAPER: an additional $X per resident left\n")
cat("the country to pay for crude oil. NOT 'each person paid $X\n")
cat("more for fuel' - pass-through to pump prices is not measured.\n\n")

print(
  results |>
    filter(!entrepot, !is.na(per_capita_usd)) |>
    arrange(desc(per_capita_usd)) |>
    transmute(
      country,
      per_person    = round(per_capita_usd, 1),
      additional_bn = round(additional_billion, 2),
      pct_gdp       = round(pct_gdp, 3),
      income,
      rank_total    = rank_absolute
    ) |>
    slice_head(n = 25),
  n = Inf, width = Inf
)


# ======================================================================
# 8. ENTREPOT ECONOMIES, SHOWN SEPARATELY
# ======================================================================

rule("8. ENTREPOT ECONOMIES, GROSS AND NET PER PERSON")

print(
  results |>
    filter(entrepot, !is.na(per_capita_usd)) |>
    arrange(desc(per_capita_usd)) |>
    transmute(
      country,
      gross_per_person = round(per_capita_usd, 1),
      net_per_person   = round(net_per_capita_usd, 1),
      reexport_pct     = round(100 * reexport_ratio, 1)
    ),
  n = Inf, width = Inf
)


# ======================================================================
# 9. TOP BY SHARE OF GDP
# ======================================================================

rule("9. TOP 25 BY SHARE OF GDP")

print(
  results |>
    filter(!is.na(pct_gdp)) |>
    arrange(desc(pct_gdp)) |>
    transmute(
      country,
      pct_gdp          = round(pct_gdp, 3),
      annualised_pct   = round(annualised_pct_gdp, 3),
      per_person       = round(per_capita_usd, 1),
      additional_bn    = round(additional_billion, 2),
      income,
      entrepot
    ) |>
    slice_head(n = 25),
  n = Inf, width = Inf
)


# ======================================================================
# 10. TOP BY SHARE OF EXPORTS
# ======================================================================

rule("10. TOP 25 BY SHARE OF EXPORT EARNINGS")

cat("\nThis is the balance-of-payments view: how much of a country's\n")
cat("export earnings the additional crude bill absorbed.\n\n")

print(
  results |>
    filter(!is.na(pct_exports)) |>
    arrange(desc(pct_exports)) |>
    transmute(
      country,
      pct_exports  = round(pct_exports, 2),
      pct_reserves = round(pct_reserves, 2),
      per_person   = round(per_capita_usd, 1),
      income,
      entrepot
    ) |>
    slice_head(n = 25),
  n = Inf, width = Inf
)


# ======================================================================
# 11. INCOME GROUPS
# ======================================================================

rule("11. BURDEN BY INCOME GROUP")

income_summary <-
  results |>
  filter(!is.na(income), income != "Aggregates") |>
  group_by(income) |>
  summarise(
    n_countries    = dplyr::n(),
    additional_bn  = sum(additional_usd, na.rm = TRUE) / 1e9,
    population_mn  = sum(pop, na.rm = TRUE) / 1e6,
    gdp_tn         = sum(gdp_usd, na.rm = TRUE) / 1e12,
    .groups = "drop"
  ) |>
  mutate(
    pct_of_total  = 100 * additional_bn / sum(additional_bn),
    per_person    = additional_bn * 1e9 / (population_mn * 1e6),
    pct_group_gdp = 100 * additional_bn / (gdp_tn * 1000)
  ) |>
  arrange(desc(additional_bn))

cat("\n")
print(
  income_summary |>
    transmute(
      income,
      n_countries,
      additional_bn = round(additional_bn, 1),
      pct_of_total  = round(pct_of_total, 1),
      per_person    = round(per_person, 1),
      pct_group_gdp = round(pct_group_gdp, 3)
    ),
  n = Inf, width = Inf
)


# ======================================================================
# 12. EXPORTER MIRROR
# ======================================================================

rule("12. EXPORTER MIRROR")

cat("\nWhere the additional money went. Approximated as mean crude\n")
cat("exports over the window times the price integral, so it is\n")
cat("less precise than the importer calculation.\n")

exporter_mirror <-
  crude_exports |>
  filter(mean_exports_kbd > 0) |>
  mutate(
    country = suppressWarnings(
      countrycode::countrycode(iso2, "iso2c", "country.name")
    ),
    additional_revenue_usd = mean_exports_kbd * 1000 * price_integral,
    additional_revenue_bn  = additional_revenue_usd / 1e9
  ) |>
  filter(!is.na(country)) |>
  arrange(desc(additional_revenue_usd))

EXPORTER_TOTAL <- sum(exporter_mirror$additional_revenue_usd)

cat("\nImporter additional expenditure: $",
    round(GLOBAL_ADD / 1e9, 1), " bn\n", sep = "")
cat("Exporter additional revenue:     $",
    round(EXPORTER_TOTAL / 1e9, 1), " bn\n", sep = "")
cat("Ratio (exporter / importer):     ",
    round(EXPORTER_TOTAL / GLOBAL_ADD, 3), "\n", sep = "")
cat("\nThe two will not match. JODI export and import coverage\n")
cat("differ and intra-sample trade is not netted. Report the ratio.\n\n")

print(
  exporter_mirror |>
    transmute(
      country,
      exports_kbd = round(mean_exports_kbd),
      additional_revenue_bn = round(additional_revenue_bn, 2)
    ) |>
    slice_head(n = 20),
  n = Inf
)


# ======================================================================
# 13. SENTENCES FOR THE MANUSCRIPT
# ======================================================================

rule("13. SENTENCES FOR THE MANUSCRIPT")

top3_abs <- results |> slice_head(n = 3)

top3_pc <-
  results |>
  filter(!entrepot, !is.na(per_capita_usd)) |>
  arrange(desc(per_capita_usd)) |>
  slice_head(n = 3)

top3_gdp <-
  results |>
  filter(!entrepot, !is.na(pct_gdp)) |>
  arrange(desc(pct_gdp)) |>
  slice_head(n = 3)

cat("\n---\n")
cat("Between ", format(WINDOW_START, "%d %B"), " and ",
    format(WINDOW_END, "%d %B %Y"), ", the ", nrow(results),
    " economies that report\ncrude-oil imports to JODI spent an estimated $",
    round(GLOBAL_ADD / 1e9, 1),
    " billion more on crude oil\nthan they would have at the pre-conflict Brent benchmark of $",
    round(P0, 2), " per\nbarrel. That is ",
    round(100 * GLOBAL_ADD / GLOBAL_TOTAL, 1),
    "% of their total estimated crude bill of $",
    round(GLOBAL_TOTAL / 1e9, 1), " billion,\nor about $",
    round(GLOBAL_ADD / N_DAYS / 1e9, 2), " billion a day.\n", sep = "")

cat("\n---\n")
cat("The largest absolute amounts fell on ",
    paste(top3_abs$country, collapse = ", "),
    ",\nat $", paste(round(top3_abs$additional_billion, 1),
                     collapse = ", $"),
    " billion respectively.\n", sep = "")

cat("\n---\n")
cat("Measured per resident, the heaviest burdens fell on ",
    paste(top3_pc$country, collapse = ", "),
    ",\nat $", paste(round(top3_pc$per_capita_usd, 0),
                     collapse = ", $"),
    " per person. None was party to the conflict.\n", sep = "")

cat("\n---\n")
cat("Relative to GDP, the largest burdens fell on ",
    paste(top3_gdp$country, collapse = ", "), ",\nat ",
    paste(round(top3_gdp$annualised_pct_gdp, 2), collapse = "%, "),
    "% of GDP on an annualised basis.\n", sep = "")

cat("\n--- LIMITATION SENTENCE, DO NOT DROP ---\n")
cat("Figures cover economies reporting crude-oil imports to JODI and\n")
cat("therefore skew toward advanced and large emerging economies.\n")
cat("Seven reporters were excluded for lacking conflict-period data.\n")
cat("The totals are a lower bound on the global burden.\n")


# ======================================================================
# SAVE
# ======================================================================

saveRDS(
  list(
    vintage        = GOICT$vintage,
    window_start   = WINDOW_START,
    window_end     = WINDOW_END,
    n_days         = N_DAYS,
    benchmark      = P0,
    price_integral = price_integral,
    global_additional_usd = GLOBAL_ADD,
    global_total_usd      = GLOBAL_TOTAL,
    results        = results,
    income         = income_summary,
    exporter       = exporter_mirror,
    imputation     = imp
  ),
  file.path(OUTDIR, "GOICT_results.rds")
)

write_csv(results,         file.path(OUTDIR, "GOICT_results.csv"))
write_csv(income_summary,  file.path(OUTDIR, "GOICT_income_groups.csv"))
write_csv(exporter_mirror, file.path(OUTDIR, "GOICT_exporter_mirror.csv"))

cat("\n")
cat(strrep("=", 70), "\n")
cat("Saved: GOICT_results.rds, GOICT_results.csv,\n")
cat("       GOICT_income_groups.csv, GOICT_exporter_mirror.csv\n")
cat(strrep("=", 70), "\n\n")
