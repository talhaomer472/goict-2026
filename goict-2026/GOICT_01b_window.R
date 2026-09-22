# ======================================================================
# GOICT-2026 — SCRIPT 01b
# WINDOW RESTRICTION AND VOLUME HETEROGENEITY
#
# Run after GOICT_01_full_patched.R, before script 02.
#
# Does three things:
#   1. Restricts the headline to the period covered by ACTUAL data,
#      so no imputed volume enters the published figure.
#   2. Flags countries whose conflict-period volumes are entirely
#      imputed, and excludes them from the headline sample.
#   3. Builds the volume-response figure, which is now a main display.
#
# Produces GOICT_headline, used by script 02 in place of GOICT$daily.
# ======================================================================

library(tidyverse)
library(lubridate)
library(scales)

stopifnot(exists("GOICT"))

OUTDIR <- "GOICT_2026_output"
if (!dir.exists(OUTDIR)) dir.create(OUTDIR)

CONFLICT_START <- GOICT$config$conflict_start
P0             <- GOICT$config$benchmark_price


# ======================================================================
# 1. DECIDE THE WINDOW FROM THE DATA
# ======================================================================

last_actual_by_country <-
  GOICT$imports_monthly |>
  filter(quantity_status == "Actual") |>
  group_by(iso2, country) |>
  summarise(last_actual_month = max(month), .groups = "drop")

coverage_by_month <-
  last_actual_by_country |>
  count(last_actual_month, name = "n_countries") |>
  arrange(desc(last_actual_month)) |>
  mutate(
    cumulative_countries = cumsum(n_countries),
    pct_of_panel = 100 * cumulative_countries /
                   nrow(last_actual_by_country)
  )

cat("\n=== REPORTING COVERAGE BY CUT-OFF MONTH ===\n")
print(coverage_by_month, n = Inf)

# The modal last-actual month, i.e. where most of the panel ends.
# Window end = the latest month that at least 70% of reporters have
# filed. Using the maximum would let one early reporter extend the
# window for everyone, filling the rest with projected volumes.
COVERAGE_RULE <- 0.70
WINDOW_END_MONTH <-
  coverage_by_month |>
  filter(pct_of_panel >= 100 * COVERAGE_RULE) |>
  summarise(m = max(last_actual_month)) |>
  pull(m)
WINDOW_END       <- ceiling_date(WINDOW_END_MONTH, "month") - days(1)

cat("\nHeadline window:",
    as.character(CONFLICT_START), "to",
    as.character(WINDOW_END), "\n")
cat("Days:", as.integer(WINDOW_END - CONFLICT_START) + 1, "\n")


# ======================================================================
# 2. EXCLUDE COUNTRIES WITH NO CONFLICT-PERIOD ACTUALS
#
# A country whose last reported month precedes the conflict has an
# entirely imputed window. It adds risk and no information.
# ======================================================================

STALE_CUTOFF <- as.Date("2026-01-01")

country_status <-
  last_actual_by_country |>
  mutate(
    months_stale =
      as.integer(
        interval(last_actual_month, WINDOW_END_MONTH) %/% months(1)
      ),
    headline_sample = last_actual_month >= STALE_CUTOFF
  ) |>
  arrange(last_actual_month)

cat("\n=== EXCLUDED FROM HEADLINE (entirely imputed window) ===\n")
print(country_status |> filter(!headline_sample), n = Inf)

cat("\nHeadline sample size:", sum(country_status$headline_sample),
    "of", nrow(country_status), "\n")


# ======================================================================
# 3. THE HEADLINE PANEL
# ======================================================================

GOICT_headline <-
  GOICT$daily |>

  filter(date >= CONFLICT_START, date <= WINDOW_END) |>

  semi_join(
    country_status |> filter(headline_sample) |> select(iso2),
    by = "iso2"
  ) |>

  group_by(iso2, country, iso3) |>
  arrange(date, .by_group = TRUE) |>
  mutate(
    cumulative_cost_usd =
      cumsum(additional_cost_usd),
    cumulative_total_import_bill_usd =
      cumsum(total_import_bill_usd)
  ) |>
  ungroup()


# How much of the headline still rests on imputed volumes?
imp_share <-
  GOICT_headline |>
  summarise(
    total     = sum(additional_cost_usd, na.rm = TRUE),
    imputed   = sum(additional_cost_usd[quantity_status == "Estimated"],
                    na.rm = TRUE),
    days_tot  = n_distinct(date),
    days_imp  = n_distinct(date[quantity_status == "Estimated"])
  ) |>
  mutate(pct_value_imputed = 100 * imputed / total)

cat("\n=== IMPUTATION INSIDE THE HEADLINE WINDOW ===\n")
print(imp_share)

# This should now be small. If it is not, a large importer is missing
# recent months (India is the known case) and needs a data splice.

cat("\nCountries still contributing imputed value in the window:\n")
print(
  GOICT_headline |>
    filter(quantity_status == "Estimated") |>
    group_by(country) |>
    summarise(
      imputed_usd = sum(additional_cost_usd, na.rm = TRUE),
      months = n_distinct(floor_date(date, "month")),
      .groups = "drop"
    ) |>
    arrange(desc(imputed_usd)),
  n = Inf
)


# ======================================================================
# 4. REVISED HEADLINE NUMBERS
# ======================================================================

headline_global <-
  GOICT_headline |>
  group_by(date) |>
  summarise(
    countries        = n_distinct(iso2),
    imports_kbd      = sum(imports_kbd_used, na.rm = TRUE),
    additional_usd   = sum(additional_cost_usd, na.rm = TRUE),
    total_bill_usd   = sum(total_import_bill_usd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(date) |>
  mutate(
    cumulative_additional_usd = cumsum(additional_usd),
    cumulative_total_bill_usd = cumsum(total_bill_usd)
  )

headline_country <-
  GOICT_headline |>
  group_by(iso2, country, iso3) |>
  summarise(
    additional_usd = sum(additional_cost_usd, na.rm = TRUE),
    total_bill_usd = sum(total_import_bill_usd, na.rm = TRUE),
    mean_imports_kbd = mean(imports_kbd_used, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(additional_usd)) |>
  mutate(
    rank = row_number(),
    additional_billion = additional_usd / 1e9
  )

cat("\n=== REVISED HEADLINE ===\n")
cat("Window:      ", as.character(CONFLICT_START), "to",
    as.character(WINDOW_END), "\n")
cat("Countries:   ", n_distinct(GOICT_headline$iso2), "\n")
cat("Cumulative additional expenditure: $",
    round(dplyr::last(headline_global$cumulative_additional_usd) / 1e9, 1),
    " bn\n", sep = "")
cat("Total import bill:                 $",
    round(dplyr::last(headline_global$cumulative_total_bill_usd) / 1e9, 1),
    " bn\n", sep = "")

cat("\nTop 20:\n")
print(
  headline_country |>
    select(rank, country, mean_imports_kbd, additional_billion) |>
    slice_head(n = 20),
  n = Inf
)


# ======================================================================
# 5. THE VOLUME-RESPONSE FIGURE
#
# Index each country's monthly crude imports to January 2026 = 100.
# This is the display that shows the shock was absorbed completely
# differently across importers.
# ======================================================================

FOCUS <- c("China", "Japan", "South Korea", "India",
           "United States", "Germany", "Italy", "Spain",
           "Netherlands", "Turkey")

vol_index <-
  GOICT$imports_actual |>

  filter(country %in% FOCUS,
         month >= as.Date("2025-10-01"),
         month <= WINDOW_END_MONTH) |>

  group_by(country) |>

  mutate(
    base = imports_actual_kbd[month == as.Date("2026-01-01")][1],
    index = 100 * imports_actual_kbd / base
  ) |>

  ungroup() |>

  filter(is.finite(index))

fig_volume <-
  ggplot(vol_index, aes(month, index, group = country)) +

  annotate("rect",
           xmin = CONFLICT_START, xmax = WINDOW_END,
           ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.45) +

  geom_hline(yintercept = 100, linetype = "dashed",
             colour = "grey50") +

  geom_line(linewidth = 0.9, colour = "grey70") +

  geom_line(
    data = ~ filter(.x, country %in% c("China", "Japan",
                                       "South Korea")),
    aes(colour = country), linewidth = 1.3
  ) +

  geom_point(
    data = ~ filter(.x, country %in% c("China", "Japan",
                                       "South Korea")),
    aes(colour = country), size = 2
  ) +

  scale_colour_manual(
    values = c("China" = "#c0392b",
               "Japan" = "#2471a3",
               "South Korea" = "#117a65"),
    name = NULL
  ) +

  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +

  labs(
    title = "The same price shock, very different volume responses",
    subtitle = paste0(
      "Monthly crude-oil imports, January 2026 = 100. ",
      "Shaded area marks the conflict period."
    ),
    x = NULL, y = "Index (Jan 2026 = 100)",
    caption = paste0(
      "Grey lines: other major importers. Source: JODI Oil. ",
      "Data vintage ", GOICT$vintage, "."
    )
  ) +

  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(colour = "grey40", hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(fig_volume)

ggsave(file.path(OUTDIR, "GOICT_fig_volume_response.png"),
       fig_volume, width = 9, height = 6, dpi = 300)


# ======================================================================
# 6. SAVE
# ======================================================================

saveRDS(
  list(
    window_start    = CONFLICT_START,
    window_end      = WINDOW_END,
    benchmark       = P0,
    vintage         = GOICT$vintage,
    daily           = GOICT_headline,
    global          = headline_global,
    country         = headline_country,
    country_status  = country_status,
    coverage        = coverage_by_month,
    imputation      = imp_share,
    volume_index    = vol_index
  ),
  file.path(OUTDIR, "GOICT_headline.rds")
)

write_csv(headline_country, file.path(OUTDIR, "GOICT_headline_country.csv"))
write_csv(country_status,   file.path(OUTDIR, "GOICT_country_status.csv"))
write_csv(vol_index,        file.path(OUTDIR, "GOICT_volume_index.csv"))
