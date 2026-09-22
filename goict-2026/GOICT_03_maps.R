# ======================================================================
# GOICT-2026 — SCRIPT 03
# WORLD MAPS WITH TOP-20 COUNTRY LABELS
#
# Run after GOICT_02a_results.R. Reads GOICT_results.rds.
#
# One-time install:
#   install.packages(c("sf", "rnaturalearth", "rnaturalearthdata",
#                      "ggrepel", "patchwork"))
#
# Produces in GOICT_2026_output:
#   GOICT_map_total.png       additional expenditure, $bn
#   GOICT_map_per_person.png  additional expenditure per resident
#   GOICT_map_exports.png     additional expenditure, % of exports
#   GOICT_map_europe.png      Europe close-up, where half the top 20 sits
#   GOICT_map_panel.png       the three world maps stacked
#
# Each map labels its own top 20, so the labels differ between maps.
# ======================================================================

library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)

OUTDIR <- "GOICT_2026_output"

R       <- readRDS(file.path(OUTDIR, "GOICT_results.rds"))
results <- R$results

# Taiwan is absent from World Bank data; patch so it is not grey.
results <-
  results |>
  mutate(
    pop            = if_else(iso2 == "TW", 23400000, pop),
    gdp_usd        = if_else(iso2 == "TW", 790e9,    gdp_usd),
    per_capita_usd = additional_usd / pop,
    pct_gdp        = 100 * additional_usd / gdp_usd
  )


# ======================================================================
# 1. WORLD POLYGONS
#
# ne_countries() stores "-99" in iso_a3 for several countries including
# France and Norway. Without the coalesce below, France would appear
# grey despite $3.73bn. Most common mistake in R choropleths.
# ======================================================================

world <-
  ne_countries(scale = "medium", returnclass = "sf") |>

  filter(admin != "Antarctica") |>

  mutate(
    iso3 = case_when(
      !is.na(iso_a3)    & iso_a3    != "-99" ~ iso_a3,
      !is.na(iso_a3_eh) & iso_a3_eh != "-99" ~ iso_a3_eh,
      TRUE                                   ~ adm0_a3
    )
  ) |>

  select(iso3, admin, geometry)


map_data <- world |> left_join(results, by = "iso3")


# ---- join diagnostic ------------------------------------------------

unmatched <-
  results |>
  filter(!iso3 %in% world$iso3) |>
  select(country, iso3, additional_billion)

cat("\n=== JOIN CHECK ===\n")
cat("Countries in results:", nrow(results), "\n")
cat("Matched to a polygon:", sum(results$iso3 %in% world$iso3), "\n")

if (nrow(unmatched) > 0) {
  cat("\nUNMATCHED — grey on the map:\n")
  print(unmatched, n = Inf)
} else {
  cat("All countries matched.\n")
}


# ======================================================================
# 2. LABEL ANCHOR POINTS
#
# A plain centroid fails for countries with distant territories:
# France's centroid lands in the Atlantic because the polygon includes
# French Guiana, and the same applies to the US with Alaska, Norway
# with Svalbard, and the Netherlands with its Caribbean islands.
#
# Fix: split into single polygons, keep the largest one per country,
# then take a point inside it.
# ======================================================================

anchors <-
  tryCatch(
    {
      suppressWarnings(
        world |>
          st_cast("POLYGON") |>
          mutate(.area = as.numeric(st_area(geometry))) |>
          group_by(iso3) |>
          slice_max(.area, n = 1, with_ties = FALSE) |>
          ungroup() |>
          st_point_on_surface()
      )
    },
    error = function(e) {
      message("Falling back to simple centroids: ",
              conditionMessage(e))
      suppressWarnings(st_centroid(world))
    }
  )

anchor_xy <-
  bind_cols(
    anchors |> st_drop_geometry() |> select(iso3),
    as_tibble(st_coordinates(anchors)) |> select(X, Y)
  )

cat("\nLabel anchors built for", nrow(anchor_xy), "countries.\n")


# ======================================================================
# 3. THEME AND HELPERS
# ======================================================================

theme_map <-
  theme_void(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle     = element_text(size = 10, colour = "grey30",
                                     hjust = 0, margin = margin(b = 8)),
    plot.caption      = element_text(size = 8, colour = "grey45",
                                     hjust = 0, margin = margin(t = 8)),
    legend.position   = "bottom",
    legend.key.height = unit(0.35, "cm"),
    legend.key.width  = unit(1.3, "cm"),
    legend.title      = element_text(size = 9),
    legend.text       = element_text(size = 8),
    plot.margin       = margin(10, 10, 10, 10)
  )

CAPTION <- paste0(
  "Labels show the 20 largest values for the mapped quantity. ",
  "Grey: does not report crude-oil imports to JODI, or excluded for ",
  "lacking conflict-period data.\nWindow ",
  format(R$window_start, "%d %b"), " to ",
  format(R$window_end, "%d %b %Y"),
  ". Sources: JODI Oil, U.S. EIA, World Bank. Vintage ", R$vintage, "."
)


# Build the label set for one metric.
label_set <- function(var, fmt, n = 20, region = "world") {

  d <-
    results |>
    filter(!is.na(.data[[var]]), .data[[var]] > 0) |>
    slice_max(.data[[var]], n = n) |>
    left_join(anchor_xy, by = "iso3") |>
    filter(!is.na(X)) |>
    mutate(lab = paste0(country, "\n", fmt(.data[[var]])))

  if (region == "europe") {
    d <- d |> filter(X > -25, X < 45, Y > 34, Y < 72)
  }

  d
}


make_map <- function(
    var, breaks, labels, fmt,
    title, subtitle, legend_title,
    palette = "YlOrRd",
    xlim = c(-170, 180), ylim = c(-58, 84),
    label_size = 2.5, n_labels = 20,
    region = "world"
) {

  d <-
    map_data |>
    mutate(
      band = cut(.data[[var]], breaks = breaks, labels = labels,
                 include.lowest = TRUE, right = FALSE)
    )

  labs_df <- label_set(var, fmt, n = n_labels, region = region)

  ggplot(d) +

    geom_sf(aes(fill = band), colour = "white", linewidth = 0.12) +

    geom_label_repel(
      data          = labs_df,
      aes(x = X, y = Y, label = lab),
      size          = label_size,
      lineheight    = 0.9,
      label.size    = 0.15,
      label.padding = unit(0.12, "lines"),
      label.r       = unit(0.1, "lines"),
      fill          = alpha("white", 0.82),
      colour        = "grey15",
      segment.colour = "grey30",
      segment.size  = 0.3,
      min.segment.length = 0,
      box.padding   = 0.35,
      point.padding = 0.1,
      force         = 6,
      max.overlaps  = Inf,
      seed          = 42
    ) +

    scale_fill_brewer(
      palette  = palette,
      na.value = "grey88",
      name     = legend_title,
      drop     = FALSE,
      guide    = guide_legend(
        nrow = 1, label.position = "bottom",
        title.position = "top", title.hjust = 0
      )
    ) +

    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +

    labs(title = title, subtitle = subtitle, caption = CAPTION) +

    theme_map
}


f_bn  <- function(x) paste0("$", formatC(x, format = "f", digits = 1), "bn")
f_usd <- function(x) paste0("$", formatC(x, format = "f", digits = 0))
f_pct <- function(x) paste0(formatC(x, format = "f", digits = 2), "%")


# ======================================================================
# 4. MAP A — TOTAL ADDITIONAL EXPENDITURE
# ======================================================================

map_total <-
  make_map(
    var    = "additional_billion",
    breaks = c(0, 0.5, 1, 2.5, 5, 10, 25, Inf),
    labels = c("<0.5", "0.5–1", "1–2.5", "2.5–5",
               "5–10", "10–25", ">25"),
    fmt    = f_bn,
    title  = "Additional crude-oil import expenditure",
    subtitle = paste0(
      "Billion USD above the pre-conflict Brent benchmark of $",
      round(R$benchmark, 2), " per barrel"
    ),
    legend_title = "Billion USD"
  )

ggsave(file.path(OUTDIR, "GOICT_map_total.png"),
       map_total, width = 13, height = 7.5, dpi = 300, bg = "white")


# ======================================================================
# 5. MAP B — PER RESIDENT
#
# Wording: money that left the country, not what an individual paid
# at the pump.
# ======================================================================

map_pc <-
  make_map(
    var    = "per_capita_usd",
    breaks = c(0, 10, 25, 50, 100, 150, 250, Inf),
    labels = c("<10", "10–25", "25–50", "50–100",
               "100–150", "150–250", ">250"),
    fmt    = f_usd,
    title  = "Additional crude-oil expenditure per resident",
    subtitle = paste0(
      "USD per resident that left the country to pay for crude oil. ",
      "Refining and trading hubs are noted in the text."
    ),
    legend_title = "USD per resident",
    palette = "PuBu"
  )

ggsave(file.path(OUTDIR, "GOICT_map_per_person.png"),
       map_pc, width = 13, height = 7.5, dpi = 300, bg = "white")


# ======================================================================
# 6. MAP C — SHARE OF EXPORT EARNINGS
# ======================================================================

map_exp <-
  make_map(
    var    = "pct_exports",
    breaks = c(0, 0.1, 0.25, 0.5, 0.75, 1, 2, Inf),
    labels = c("<0.1", "0.1–0.25", "0.25–0.5", "0.5–0.75",
               "0.75–1", "1–2", ">2"),
    fmt    = f_pct,
    title  = "Additional crude-oil expenditure as a share of export earnings",
    subtitle = "Percent of annual exports of goods and services",
    legend_title = "% of export earnings",
    palette = "YlGnBu"
  )

ggsave(file.path(OUTDIR, "GOICT_map_exports.png"),
       map_exp, width = 13, height = 7.5, dpi = 300, bg = "white")


# ======================================================================
# 7. EUROPE CLOSE-UP
#
# Twelve of the top 20 by resident are European and the labels crowd
# badly on a world map. This panel gives them room.
# ======================================================================

map_europe <-
  make_map(
    var    = "per_capita_usd",
    breaks = c(0, 10, 25, 50, 100, 150, 250, Inf),
    labels = c("<10", "10–25", "25–50", "50–100",
               "100–150", "150–250", ">250"),
    fmt    = f_usd,
    title  = "Europe: additional crude-oil expenditure per resident",
    subtitle = "USD per resident that left the country to pay for crude oil",
    legend_title = "USD per resident",
    palette = "PuBu",
    xlim = c(-12, 42), ylim = c(34, 71),
    label_size = 2.9, n_labels = 40, region = "europe"
  )

ggsave(file.path(OUTDIR, "GOICT_map_europe.png"),
       map_europe, width = 10, height = 9, dpi = 300, bg = "white")


# ======================================================================
# 8. THREE-PANEL FIGURE
# ======================================================================

if (requireNamespace("patchwork", quietly = TRUE)) {

  library(patchwork)

  strip <- function(p, tag) {
    p + labs(title = tag, subtitle = NULL, caption = NULL)
  }

  panel <-
    (strip(map_total, "A. Total, billion USD") /
     strip(map_pc,    "B. Per resident, USD") /
     strip(map_exp,   "C. Share of export earnings, %")) +

    plot_annotation(
      title = "The cost of the 2026 oil shock, by country",
      subtitle = paste0(
        "28 February to 30 June 2026. ", nrow(results),
        " reporting economies. $",
        round(R$global_additional_usd / 1e9, 1),
        " billion in additional crude-oil import expenditure."
      ),
      caption = CAPTION,
      theme = theme(
        plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, colour = "grey30"),
        plot.caption  = element_text(size = 8, colour = "grey45",
                                     hjust = 0)
      )
    )

  ggsave(file.path(OUTDIR, "GOICT_map_panel.png"),
         panel, width = 12, height = 19, dpi = 300, bg = "white")

  cat("\nSaved five maps to", OUTDIR, "\n")

} else {
  cat("\nInstall patchwork for the three-panel figure.\n")
  cat("Saved four maps to", OUTDIR, "\n")
}


# ---- what got labelled on each map ----------------------------------

cat("\n=== LABELS, MAP A (total) ===\n")
print(label_set("additional_billion", f_bn) |>
        select(country, additional_billion), n = Inf)

cat("\n=== LABELS, MAP B (per resident) ===\n")
print(label_set("per_capita_usd", f_usd) |>
        select(country, per_capita_usd), n = Inf)

cat("\n=== LABELS, MAP C (share of exports) ===\n")
print(label_set("pct_exports", f_pct) |>
        select(country, pct_exports), n = Inf)


print(map_exp)
