# ======================================================================
# GOICT-2026 — GLOBAL OIL IMPORT COST TRACKER
# COMPLETE PATCHED SCRIPT
#
# This is your original run_goict() with all five patches already
# applied. Source it directly. Do not paste anything by hand.
#
# Changes relative to your original:
#   1. Downloads are cached into a dated vintage folder, so re-runs
#      reproduce identical numbers.
#   2. Reporters are screened against valid ISO2 codes, so JODI
#      aggregates cannot enter the global total and the map sample
#      equals the headline sample.
#   3. Multiple records per country-month resolve deterministically
#      instead of by CSV row order.
#   4. Benchmark reports both mean and median.
#   5. iso3 is carried through the whole pipeline.
#
# The measurement approach is unchanged.
# ======================================================================

library(tidyverse)
library(readxl)
library(lubridate)
library(countrycode)
library(scales)


run_goict <- function(
    vintage = NULL
) {

  # ====================================================================
  # A. CONFIGURATION
  # ====================================================================

  CONFLICT_START <- as.Date("2026-02-28")
  BENCHMARK_DAYS <- 20

  SCENARIO_PRICES <- c(80, 100, 120, 150)
  SCENARIO_DAYS   <- c(30, 90, 180)

  jodi_2025_url <-
    "https://www.jodidata.org/_resources/files/downloads/oil-data/annual-csv/primary/2025.csv"

  jodi_2026_url <-
    "https://www.jodidata.org/_resources/files/downloads/oil-data/annual-csv/primary/primaryyear2026.csv"

  eia_brent_url <-
    "https://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls"

  OUTDIR <- "GOICT_2026_output"

  if (!dir.exists(OUTDIR)) dir.create(OUTDIR)


  # ---- vintage handling (PATCH 0) ------------------------------------

  CACHE_ROOT <- "GOICT_vintages"

  if (!dir.exists(CACHE_ROOT)) dir.create(CACHE_ROOT)

  VINTAGE_TAG <-
    if (is.null(vintage)) format(Sys.Date(), "%Y-%m-%d") else vintage

  CACHE_DIR <- file.path(CACHE_ROOT, VINTAGE_TAG)

  if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR)

  cat("\n============================================================\n")
  cat("             GOICT-2026 STARTING\n")
  cat("============================================================\n")
  cat("\nData vintage:", VINTAGE_TAG, "\n")
  cat("Vintage folder:", CACHE_DIR, "\n")


  # ====================================================================
  # B. HELPER FUNCTIONS
  # ====================================================================

  # ---- cached download (PATCH 1) -------------------------------------

  download_to_cache <- function(url, filename) {

    target <- file.path(CACHE_DIR, filename)

    if (file.exists(target) && file.info(target)$size > 0) {
      cat("\nUsing cached file:", target, "\n")
      return(target)
    }

    cat("\nDownloading:\n", url, "\n")

    ok <-
      tryCatch(
        {
          utils::download.file(
            url      = url,
            destfile = target,
            mode     = "wb",
            quiet    = FALSE,
            method   = "libcurl"
          )
          TRUE
        },
        error = function(e) {
          message("Download failed: ", conditionMessage(e))
          FALSE
        }
      )

    if (!ok || !file.exists(target) || file.info(target)$size == 0) {
      stop(paste("Could not obtain:", url))
    }

    target
  }


  # ---- safe date conversion for the EIA workbook ---------------------

  safe_date <- function(x) {

    if (inherits(x, "Date"))   return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x))
    if (is.numeric(x))         return(as.Date(x, origin = "1899-12-30"))

    x <- trimws(as.character(x))

    result <- rep(as.Date(NA), length(x))

    numeric_candidate <- suppressWarnings(as.numeric(x))

    numeric_index <-
      is.na(result) &
      is.finite(numeric_candidate) &
      numeric_candidate > 20000 &
      numeric_candidate < 80000

    if (any(numeric_index)) {
      result[numeric_index] <-
        as.Date(numeric_candidate[numeric_index],
                origin = "1899-12-30")
    }

    formats <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y",
                 "%Y/%m/%d", "%b %d, %Y", "%B %d, %Y")

    for (fmt in formats) {
      missing <- is.na(result)
      if (!any(missing)) break
      attempt <- suppressWarnings(as.Date(x[missing], format = fmt))
      result[missing] <- attempt
    }

    result
  }


  # ---- monthly import gap filling ------------------------------------
  #
  # Uses ONLY previous ACTUAL observations. Estimated observations are
  # never fed back recursively.

  fill_missing_imports <- function(df) {

    df <- df |> arrange(month)

    actual <- df$imports_actual_kbd
    used   <- actual
    status <- ifelse(is.finite(actual), "Actual", NA_character_)

    for (i in seq_along(used)) {

      if (!is.finite(used[i])) {

        previous_actual <-
          if (i == 1) {
            numeric(0)
          } else {
            p <- actual[seq_len(i - 1)]
            p[is.finite(p)]
          }

        if (length(previous_actual) > 0) {
          used[i]   <- mean(tail(previous_actual, 3), na.rm = TRUE)
          status[i] <- "Estimated"
        } else {
          used[i]   <- NA_real_
          status[i] <- "Unavailable"
        }
      }
    }

    df$imports_kbd_used <- used
    df$quantity_status  <- status

    df
  }


  # ====================================================================
  # C. DOWNLOAD JODI DATA
  # ====================================================================

  cat("\n============================================================\n")
  cat("1. JODI DATA\n")
  cat("============================================================\n")

  tmp_2025 <- download_to_cache(jodi_2025_url, "jodi_2025.csv")
  tmp_2026 <- download_to_cache(jodi_2026_url, "jodi_2026.csv")

  # Read every field as character so bind_rows cannot hit a type clash.
  jodi_2025 <-
    readr::read_csv(tmp_2025,
                    col_types = cols(.default = col_character()),
                    progress = FALSE, show_col_types = FALSE)

  jodi_2026 <-
    readr::read_csv(tmp_2026,
                    col_types = cols(.default = col_character()),
                    progress = FALSE, show_col_types = FALSE)

  cat("\n2025 JODI rows:", nrow(jodi_2025), "\n")
  cat("2026 JODI rows:", nrow(jodi_2026), "\n")

  jodi_raw <- bind_rows(jodi_2025, jodi_2026)

  cat("Combined JODI rows:", nrow(jodi_raw), "\n")


  # ====================================================================
  # D. FILTER TO CRUDE-OIL IMPORTS, THEN SCREEN ENTITIES
  # ====================================================================

  cat("\n============================================================\n")
  cat("2. CRUDE-OIL IMPORT DATA\n")
  cat("============================================================\n")

  jodi_imports <-
    jodi_raw |>

    filter(
      ENERGY_PRODUCT == "CRUDEOIL",
      FLOW_BREAKDOWN == "TOTIMPSB",
      UNIT_MEASURE   == "KBD"
    ) |>

    mutate(
      iso2  = REF_AREA,
      month = suppressWarnings(
        as.Date(paste0(substr(TIME_PERIOD, 1, 7), "-01"))
      ),
      imports_kbd = suppressWarnings(readr::parse_number(OBS_VALUE)),
      country = countrycode::countrycode(
        iso2, origin = "iso2c", destination = "country.name"
      )
    ) |>

    mutate(
      country = case_when(
        iso2 == "KR" ~ "South Korea",
        iso2 == "GB" ~ "United Kingdom",
        iso2 == "CZ" ~ "Czechia",
        iso2 == "TW" ~ "Taiwan",
        iso2 == "BN" ~ "Brunei",
        iso2 == "TR" ~ "Turkey",
        TRUE         ~ country
      ),
      country = if_else(is.na(country), iso2, country)
    ) |>

    filter(!is.na(month))

  cat("\nCrude-oil import observations:", nrow(jodi_imports), "\n")
  cat("Reporting entities before screen:",
      n_distinct(jodi_imports$iso2), "\n")


  # ---- entity screen (PATCH 2) ---------------------------------------

  valid_iso2 <-
    unique(stats::na.omit(countrycode::codelist$iso2c))

  entity_audit <-
    jodi_imports |>

    distinct(iso2, country) |>

    mutate(
      iso3 = suppressWarnings(
        countrycode::countrycode(iso2, "iso2c", "iso3c")
      ),
      is_valid_iso2 = iso2 %in% valid_iso2,
      retained      = is_valid_iso2 & !is.na(iso3)
    )

  cat("\n------------------------------------------------------------\n")
  cat("ENTITY SCREEN\n")
  cat("------------------------------------------------------------\n")
  cat("\nReporters found:", nrow(entity_audit), "\n")
  cat("Retained (valid ISO2 with ISO3 match):",
      sum(entity_audit$retained), "\n")
  cat("\nDROPPED ENTITIES — check these by hand before publishing:\n")

  print(entity_audit |> filter(!retained), n = Inf)

  jodi_imports <-
    jodi_imports |>

    semi_join(
      entity_audit |> filter(retained) |> select(iso2),
      by = "iso2"
    ) |>

    left_join(
      entity_audit |> select(iso2, iso3),
      by = "iso2"
    )

  stopifnot(
    all(jodi_imports$imports_kbd >= 0 |
        is.na(jodi_imports$imports_kbd))
  )

  cat("\nObservations after screen:", nrow(jodi_imports), "\n")


  # ====================================================================
  # E. COLLAPSE TO ONE COUNTRY-MONTH OBSERVATION  (PATCH 3)
  # ====================================================================

  imports_actual <-
    jodi_imports |>

    group_by(iso2, country, iso3, month) |>

    # Deterministic order, so the result does not depend on CSV row
    # order and re-downloads do not silently move the headline number.
    arrange(ASSESSMENT_CODE, imports_kbd, .by_group = TRUE) |>

    summarise(

      imports_actual_kbd = {
        z <- imports_kbd[is.finite(imports_kbd)]
        if (length(z) == 0) NA_real_ else dplyr::last(z)
      },

      assessment_code = dplyr::last(ASSESSMENT_CODE),

      n_source_records = dplyr::n(),

      n_distinct_values = dplyr::n_distinct(
        round(imports_kbd[is.finite(imports_kbd)], 6)
      ),

      value_range_kbd =
        if (sum(is.finite(imports_kbd)) > 1) {
          max(imports_kbd, na.rm = TRUE) - min(imports_kbd, na.rm = TRUE)
        } else {
          0
        },

      .groups = "drop"
    )

  duplicate_audit <-
    imports_actual |>
    filter(n_distinct_values > 1) |>
    arrange(desc(value_range_kbd))

  cat("\n------------------------------------------------------------\n")
  cat("DUPLICATE-RECORD AUDIT\n")
  cat("------------------------------------------------------------\n")
  cat("\nCountry-months with conflicting source values:",
      nrow(duplicate_audit), "\n")

  if (nrow(duplicate_audit) > 0) {
    cat("Largest disagreement:",
        round(max(duplicate_audit$value_range_kbd), 1), "kbd\n")
    print(duplicate_audit |> slice_head(n = 25))
  }


  # ====================================================================
  # F. DOWNLOAD BRENT FROM U.S. EIA
  # ====================================================================

  cat("\n============================================================\n")
  cat("3. BRENT FROM U.S. EIA\n")
  cat("============================================================\n")

  tmp_brent <- download_to_cache(eia_brent_url, "eia_brent.xls")

  brent_sheets <- readxl::excel_sheets(tmp_brent)

  cat("\nEIA workbook sheets:\n")
  print(brent_sheets)

  data_sheet <-
    if ("Data 1" %in% brent_sheets) "Data 1" else brent_sheets[2]

  cat("\nUsing EIA sheet:", data_sheet, "\n")

  brent_raw <- readxl::read_excel(tmp_brent, sheet = data_sheet, skip = 2)

  if (ncol(brent_raw) < 2) {
    stop("Could not identify the two Brent columns in the EIA workbook.")
  }

  brent <- brent_raw[, 1:2]
  names(brent) <- c("date_raw", "brent_raw")

  brent <-
    brent |>
    mutate(
      date           = safe_date(date_raw),
      brent_usd_bbl  = suppressWarnings(as.numeric(brent_raw))
    ) |>
    select(date, brent_usd_bbl) |>
    filter(!is.na(date), is.finite(brent_usd_bbl)) |>
    distinct(date, .keep_all = TRUE) |>
    arrange(date)

  cat("\nBrent observations:", nrow(brent), "\n")
  cat("First Brent date:", as.character(min(brent$date)), "\n")
  cat("Last Brent date:",  as.character(max(brent$date)),  "\n")

  cat("\nLatest Brent observations:\n")
  print(tail(brent, 10))


  # ====================================================================
  # G. BRENT BENCHMARK  (PATCH 4)
  # ====================================================================

  cat("\n============================================================\n")
  cat("4. PRE-CONFLICT BRENT BENCHMARK\n")
  cat("============================================================\n")

  benchmark_sample <-
    brent |>
    filter(date < CONFLICT_START) |>
    slice_tail(n = BENCHMARK_DAYS)

  if (nrow(benchmark_sample) < BENCHMARK_DAYS) {
    stop("Not enough pre-conflict Brent observations for the benchmark.")
  }

  BRENT_BENCHMARK_MEAN <-
    mean(benchmark_sample$brent_usd_bbl, na.rm = TRUE)

  BRENT_BENCHMARK_MEDIAN <-
    stats::median(benchmark_sample$brent_usd_bbl, na.rm = TRUE)

  BRENT_BENCHMARK <- BRENT_BENCHMARK_MEAN

  benchmark <-
    tibble(
      vintage                        = VINTAGE_TAG,
      conflict_start                 = CONFLICT_START,
      benchmark_days                 = BENCHMARK_DAYS,
      benchmark_start                = min(benchmark_sample$date),
      benchmark_end                  = max(benchmark_sample$date),
      brent_benchmark_mean_usd_bbl   = BRENT_BENCHMARK_MEAN,
      brent_benchmark_median_usd_bbl = BRENT_BENCHMARK_MEDIAN,
      brent_benchmark_usd_bbl        = BRENT_BENCHMARK
    )

  cat("\nBenchmark sample:\n")
  print(benchmark_sample, n = Inf)

  cat("\nBenchmark (mean)   = $",
      round(BRENT_BENCHMARK_MEAN, 2), "\n", sep = "")
  cat("Benchmark (median) = $",
      round(BRENT_BENCHMARK_MEDIAN, 2), "\n", sep = "")


  # ====================================================================
  # H. DAILY BRENT CALENDAR
  # ====================================================================

  LAST_PRICE_DATE <- max(brent$date, na.rm = TRUE)

  brent_daily <-
    tibble(
      date = seq.Date(
        from = min(benchmark_sample$date),
        to   = LAST_PRICE_DATE,
        by   = "day"
      )
    ) |>

    left_join(
      brent |> rename(brent_observed = brent_usd_bbl),
      by = "date"
    ) |>

    arrange(date) |>

    mutate(brent_usd_bbl = brent_observed) |>

    tidyr::fill(brent_usd_bbl, .direction = "down") |>

    mutate(
      price_status = if_else(!is.na(brent_observed),
                             "Observed", "Carried forward")
    ) |>

    filter(date >= CONFLICT_START)

  cat("\nDaily Brent calendar (head and tail):\n")
  print(head(brent_daily, 5))
  print(tail(brent_daily, 5))


  # ====================================================================
  # I. MONTHLY IMPORT PANEL
  # ====================================================================

  cat("\n============================================================\n")
  cat("5. MONTHLY IMPORT PANEL\n")
  cat("============================================================\n")

  country_key <- imports_actual |> distinct(iso2, country, iso3)

  all_months <-
    seq.Date(
      from = min(imports_actual$month, na.rm = TRUE),
      to   = floor_date(LAST_PRICE_DATE, unit = "month"),
      by   = "month"
    )

  imports_monthly <-
    tidyr::crossing(country_key, month = all_months) |>

    left_join(
      imports_actual |> select(iso2, month, imports_actual_kbd),
      by = c("iso2", "month")
    ) |>

    group_by(iso2, country, iso3) |>

    group_modify(~ fill_missing_imports(.x)) |>

    ungroup() |>

    arrange(country, month)

  cat("\nEconomies in monthly panel:",
      n_distinct(imports_monthly$iso2), "\n")

  cat("\nLatest monthly quantities:\n")
  print(
    imports_monthly |>
      group_by(iso2, country) |>
      slice_tail(n = 1) |>
      ungroup() |>
      arrange(desc(imports_kbd_used)),
    n = Inf
  )


  # ====================================================================
  # J. DAILY COUNTRY-LEVEL GOICT
  # ====================================================================

  cat("\n============================================================\n")
  cat("6. DAILY GOICT DATA\n")
  cat("============================================================\n")

  daily <-
    tidyr::crossing(country_key, brent_daily) |>

    mutate(month = floor_date(date, unit = "month")) |>

    left_join(
      imports_monthly |>
        select(iso2, month, imports_actual_kbd,
               imports_kbd_used, quantity_status),
      by = c("iso2", "month")
    ) |>

    filter(
      is.finite(imports_kbd_used),
      is.finite(brent_usd_bbl)
    ) |>

    mutate(

      total_import_bill_usd =
        imports_kbd_used * 1000 * brent_usd_bbl,

      benchmark_import_bill_usd =
        imports_kbd_used * 1000 * BRENT_BENCHMARK,

      # PRIMARY MEASURE: signed additional expenditure
      additional_cost_usd =
        imports_kbd_used * 1000 * (brent_usd_bbl - BRENT_BENCHMARK),

      # SECONDARY: premium only when Brent exceeds the benchmark
      positive_only_cost_usd =
        imports_kbd_used * 1000 *
        pmax(brent_usd_bbl - BRENT_BENCHMARK, 0)
    ) |>

    group_by(iso2, country, iso3) |>

    arrange(date, .by_group = TRUE) |>

    mutate(
      cumulative_cost_usd =
        cumsum(additional_cost_usd),
      cumulative_positive_only_cost_usd =
        cumsum(positive_only_cost_usd),
      cumulative_total_import_bill_usd =
        cumsum(total_import_bill_usd),
      cumulative_benchmark_import_bill_usd =
        cumsum(benchmark_import_bill_usd)
    ) |>

    ungroup()

  cat("\nDaily GOICT rows:", nrow(daily), "\n")

  # Identity check: QP must equal QP0 + Q(P - P0) to floating point.
  identity_gap <-
    max(
      abs(
        daily$total_import_bill_usd -
        (daily$benchmark_import_bill_usd + daily$additional_cost_usd)
      ),
      na.rm = TRUE
    )

  cat("Identity check, max absolute gap: ", identity_gap, "\n", sep = "")

  if (identity_gap > 1) {
    warning("Decomposition identity does not hold. Investigate before use.")
  }


  # ====================================================================
  # K. MONTHLY COUNTRY SUMMARY
  # ====================================================================

  monthly <-
    daily |>

    mutate(year_month = floor_date(date, unit = "month")) |>

    group_by(iso2, country, iso3, year_month) |>

    summarise(
      imports_kbd = mean(imports_kbd_used, na.rm = TRUE),
      additional_cost_usd =
        sum(additional_cost_usd, na.rm = TRUE),
      positive_only_cost_usd =
        sum(positive_only_cost_usd, na.rm = TRUE),
      total_import_bill_usd =
        sum(total_import_bill_usd, na.rm = TRUE),
      benchmark_import_bill_usd =
        sum(benchmark_import_bill_usd, na.rm = TRUE),
      number_of_days = dplyr::n(),
      .groups = "drop"
    ) |>

    arrange(country, year_month)


  # ====================================================================
  # L. COUNTRY SUMMARY
  # ====================================================================

  country_summary <-
    daily |>

    group_by(iso2, country, iso3) |>

    arrange(date, .by_group = TRUE) |>

    summarise(
      latest_date            = max(date, na.rm = TRUE),
      latest_imports_kbd     = dplyr::last(imports_kbd_used),
      latest_quantity_status = dplyr::last(quantity_status),
      latest_brent_usd_bbl   = dplyr::last(brent_usd_bbl),

      current_total_import_bill_usd =
        dplyr::last(total_import_bill_usd),
      current_additional_cost_usd =
        dplyr::last(additional_cost_usd),

      total_import_spending_usd =
        sum(total_import_bill_usd, na.rm = TRUE),
      benchmark_import_spending_usd =
        sum(benchmark_import_bill_usd, na.rm = TRUE),
      total_cost_usd =
        sum(additional_cost_usd, na.rm = TRUE),
      positive_only_cost_usd =
        sum(positive_only_cost_usd, na.rm = TRUE),

      average_daily_cost_usd =
        mean(additional_cost_usd, na.rm = TRUE),
      average_total_import_bill_usd =
        mean(total_import_bill_usd, na.rm = TRUE),

      number_of_days = dplyr::n(),

      .groups = "drop"
    ) |>

    arrange(desc(total_cost_usd)) |>

    mutate(
      rank = row_number(),
      total_cost_billion_usd =
        total_cost_usd / 1e9,
      total_import_spending_billion_usd =
        total_import_spending_usd / 1e9,
      average_daily_cost_million_usd =
        average_daily_cost_usd / 1e6,
      current_total_import_bill_million_usd =
        current_total_import_bill_usd / 1e6,
      current_additional_cost_million_usd =
        current_additional_cost_usd / 1e6
    )

  cat("\n============================================================\n")
  cat("COUNTRY RESULTS\n")
  cat("============================================================\n")

  print(
    country_summary |>
      select(rank, iso2, country, latest_imports_kbd,
             latest_quantity_status,
             total_import_spending_billion_usd,
             total_cost_billion_usd,
             average_daily_cost_million_usd),
    n = Inf
  )


  # ====================================================================
  # M. GLOBAL DAILY AND MONTHLY
  # ====================================================================

  global_daily <-
    daily |>

    group_by(date) |>

    summarise(
      countries          = n_distinct(iso2),
      global_imports_kbd = sum(imports_kbd_used, na.rm = TRUE),
      total_import_bill_usd =
        sum(total_import_bill_usd, na.rm = TRUE),
      benchmark_import_bill_usd =
        sum(benchmark_import_bill_usd, na.rm = TRUE),
      additional_cost_usd =
        sum(additional_cost_usd, na.rm = TRUE),
      positive_only_cost_usd =
        sum(positive_only_cost_usd, na.rm = TRUE),
      .groups = "drop"
    ) |>

    arrange(date) |>

    mutate(
      cumulative_cost_usd =
        cumsum(additional_cost_usd),
      cumulative_positive_only_cost_usd =
        cumsum(positive_only_cost_usd),
      cumulative_total_import_bill_usd =
        cumsum(total_import_bill_usd),
      cumulative_benchmark_import_bill_usd =
        cumsum(benchmark_import_bill_usd)
    )

  global_latest <-
    global_daily |>

    slice_tail(n = 1) |>

    mutate(
      global_imports_mbd =
        global_imports_kbd / 1000,
      total_import_bill_billion_day =
        total_import_bill_usd / 1e9,
      additional_cost_billion_day =
        additional_cost_usd / 1e9,
      cumulative_cost_billion_usd =
        cumulative_cost_usd / 1e9,
      cumulative_total_import_bill_billion_usd =
        cumulative_total_import_bill_usd / 1e9
    )

  cat("\n============================================================\n")
  cat("LATEST GLOBAL RESULT\n")
  cat("============================================================\n")
  print(global_latest)

  # Cross-check: country totals must sum to the global total.
  sum_gap <-
    abs(
      sum(country_summary$total_cost_usd, na.rm = TRUE) -
      dplyr::last(global_daily$cumulative_cost_usd)
    )

  cat("\nCountry-vs-global sum gap: ", sum_gap, "\n", sep = "")

  global_monthly <-
    daily |>

    mutate(year_month = floor_date(date, unit = "month")) |>

    group_by(year_month) |>

    summarise(
      total_import_bill_usd =
        sum(total_import_bill_usd, na.rm = TRUE),
      benchmark_import_bill_usd =
        sum(benchmark_import_bill_usd, na.rm = TRUE),
      additional_cost_usd =
        sum(additional_cost_usd, na.rm = TRUE),
      positive_only_cost_usd =
        sum(positive_only_cost_usd, na.rm = TRUE),
      .groups = "drop"
    )

  cat("\nGlobal monthly results:\n")
  print(global_monthly, n = Inf)


  # ====================================================================
  # N. FUTURE PRICE SCENARIOS
  # ====================================================================

  future_scenarios <-
    tidyr::crossing(

      country_summary |>
        select(rank, iso2, country, iso3,
               latest_imports_kbd, latest_quantity_status),

      scenario_price_usd_bbl = SCENARIO_PRICES,
      horizon_days           = SCENARIO_DAYS
    ) |>

    mutate(
      projected_total_import_bill_usd =
        latest_imports_kbd * 1000 *
        scenario_price_usd_bbl * horizon_days,

      projected_benchmark_import_bill_usd =
        latest_imports_kbd * 1000 *
        BRENT_BENCHMARK * horizon_days,

      projected_additional_cost_usd =
        latest_imports_kbd * 1000 *
        (scenario_price_usd_bbl - BRENT_BENCHMARK) * horizon_days,

      projected_total_import_bill_billion_usd =
        projected_total_import_bill_usd / 1e9,

      projected_additional_cost_billion_usd =
        projected_additional_cost_usd / 1e9
    ) |>

    arrange(scenario_price_usd_bbl, horizon_days, rank)

  global_scenarios <-
    future_scenarios |>

    group_by(scenario_price_usd_bbl, horizon_days) |>

    summarise(
      projected_total_import_bill_usd =
        sum(projected_total_import_bill_usd, na.rm = TRUE),
      projected_additional_cost_usd =
        sum(projected_additional_cost_usd, na.rm = TRUE),
      .groups = "drop"
    ) |>

    mutate(
      projected_total_import_bill_billion_usd =
        projected_total_import_bill_usd / 1e9,
      projected_additional_cost_billion_usd =
        projected_additional_cost_usd / 1e9
    )

  cat("\nGlobal future scenarios:\n")
  print(global_scenarios, n = Inf)


  # ====================================================================
  # O. SAVE TABLES
  # ====================================================================

  cat("\n============================================================\n")
  cat("7. SAVING OUTPUTS\n")
  cat("============================================================\n")

  outputs <-
    list(
      GOICT_Brent_daily            = brent,
      GOICT_benchmark_sample       = benchmark_sample,
      GOICT_entity_audit           = entity_audit,
      GOICT_duplicate_audit        = duplicate_audit,
      GOICT_imports_actual         = imports_actual,
      GOICT_imports_monthly        = imports_monthly,
      GOICT_daily                  = daily,
      GOICT_monthly                = monthly,
      GOICT_country_summary        = country_summary,
      GOICT_global_daily           = global_daily,
      GOICT_global_monthly         = global_monthly,
      GOICT_country_future_scen    = future_scenarios,
      GOICT_global_future_scen     = global_scenarios
    )

  iwalk(
    outputs,
    ~ readr::write_csv(
        .x,
        file.path(OUTDIR, paste0(.y, ".csv"))
      )
  )

  cat("\nResults saved in:", OUTDIR, "\n")


  # ====================================================================
  # P. FINAL SUMMARY
  # ====================================================================

  cat("\n============================================================\n")
  cat("                FINAL GOICT SUMMARY\n")
  cat("============================================================\n")

  cat("\nVintage:               ", VINTAGE_TAG, "\n")
  cat("Conflict-period start: ", as.character(CONFLICT_START), "\n")
  cat("Last Brent price date: ", as.character(LAST_PRICE_DATE), "\n")
  cat("Brent benchmark:       $",
      round(BRENT_BENCHMARK, 2), "/barrel\n", sep = "")
  cat("Economies represented: ", n_distinct(daily$iso2), "\n")
  cat("Identity gap:          ", identity_gap, "\n")
  cat("Sum gap:               ", sum_gap, "\n")

  cat("\n============================================================\n")
  cat("                GOICT-2026 COMPLETE\n")
  cat("============================================================\n\n")


  # ====================================================================
  # Q. RETURN  (PATCH 5)
  # ====================================================================

  list(
    vintage   = VINTAGE_TAG,
    cache_dir = CACHE_DIR,

    config = list(
      conflict_start    = CONFLICT_START,
      benchmark_days    = BENCHMARK_DAYS,
      benchmark_price   = BRENT_BENCHMARK,
      benchmark_mean    = BRENT_BENCHMARK_MEAN,
      benchmark_median  = BRENT_BENCHMARK_MEDIAN,
      last_price_date   = LAST_PRICE_DATE,
      scenario_prices   = SCENARIO_PRICES,
      scenario_days     = SCENARIO_DAYS
    ),

    checks = list(
      identity_gap = identity_gap,
      sum_gap      = sum_gap
    ),

    jodi_raw         = jodi_raw,
    jodi_imports     = jodi_imports,
    entity_audit     = entity_audit,
    duplicate_audit  = duplicate_audit,
    brent            = brent,
    benchmark        = benchmark,
    benchmark_sample = benchmark_sample,
    imports_actual   = imports_actual,
    imports_monthly  = imports_monthly,
    daily            = daily,
    monthly          = monthly,
    country_summary  = country_summary,
    global_daily     = global_daily,
    global_monthly   = global_monthly,
    global_latest    = global_latest,
    future_scenarios = future_scenarios,
    global_scenarios = global_scenarios
  )
}


# ======================================================================
# RUN
# ======================================================================

GOICT <- run_goict()


# ======================================================================
# STEP 0 CHECKS — run these and send me the output
# ======================================================================

cat("\n\n=== VOLUME CROSS-CHECK ===\n")
cat("Compare against national customs data:\n")
cat("  China ~11,600 kb/d, India ~5,000, United States ~6,500,\n")
cat("  South Korea ~2,800, Japan ~2,400\n\n")

GOICT$country_summary |>
  filter(country %in% c("China", "India", "United States",
                        "South Korea", "Japan", "Germany")) |>
  select(country, latest_imports_kbd, latest_quantity_status,
         total_cost_billion_usd) |>
  print()

cat("\n=== CHINA MONTHLY ACTUALS ===\n")

GOICT$imports_actual |>
  filter(iso2 == "CN") |>
  arrange(month) |>
  print(n = Inf)

cat("\n=== FLOW CODES (for the exporter mirror) ===\n")

GOICT$jodi_raw |>
  filter(ENERGY_PRODUCT == "CRUDEOIL") |>
  count(FLOW_BREAKDOWN, sort = TRUE) |>
  print(n = Inf)