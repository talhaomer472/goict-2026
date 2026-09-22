# ======================================================================
# GOICT-2026 — SCRIPT 05
# ADDITIONAL FIGURES AND ROBUSTNESS TABLE
#
# Run after GOICT_04_decomposition.R. Needs GOICT in memory.
#
# Produces in GOICT_2026_output:
#   GOICT_fig_price_path.png    annotated Brent path, three phases
#   GOICT_fig_cumulative.png    global cumulative with phases
#   GOICT_fig_dumbbell.png      exposure vs realised, by country
#   GOICT_fig_rankflip.png      rank across three denominators
#   GOICT_fig_income_scatter.png burden vs income level
#   GOICT_fig_waterfall.png     global decomposition
#   plus a robustness table printed and saved
# ======================================================================

library(tidyverse)
library(lubridate)
library(scales)
library(ggrepel)

Sys.setlocale("LC_TIME", "C")

OUTDIR <- "GOICT_2026_output"

stopifnot(exists("GOICT"))

R <- readRDS(file.path(OUTDIR, "GOICT_results.rds"))
H <- readRDS(file.path(OUTDIR, "GOICT_headline.rds"))
D <- readRDS(file.path(OUTDIR, "GOICT_decomposition.rds"))

P0     <- R$benchmark
W0     <- R$window_start
W1     <- R$window_end
N_DAYS <- R$n_days

results <- R$results |> filter(iso2 != "BN")   # drop Brunei outlier
decomp  <- D$decomp  |> filter(iso2 != "BN")

SRC <- paste0(
  "Sources: JODI Oil, U.S. EIA, World Bank. Data vintage ",
  GOICT$vintage, "."
)

th <-
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, colour = "grey30"),
    plot.caption  = element_text(size = 8, colour = "grey45", hjust = 0),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )


# ======================================================================
# FIGURE A — ANNOTATED BRENT PATH
#
# Peak and trough are found from the data rather than hard-coded, so
# this stays correct on a later vintage. Add or edit event labels in
# the events tibble below once you have checked the dates yourself.
# ======================================================================

brent_win <-
  GOICT$brent |>
  filter(date >= W0 - 30)

peak   <- brent_win |> filter(date <= W1) |> slice_max(brent_usd_bbl, n = 1)
trough <- brent_win |> filter(date >= peak$date, date <= W1) |>
            slice_min(brent_usd_bbl, n = 1)
latest <- brent_win |> slice_max(date, n = 1)

cat("\n=== PRICE PATH LANDMARKS (verify before publishing) ===\n")
cat("Benchmark:     $", round(P0, 2), "\n", sep = "")
cat("Window peak:   $", round(peak$brent_usd_bbl, 2),
    " on ", as.character(peak$date), "\n", sep = "")
cat("Window trough: $", round(trough$brent_usd_bbl, 2),
    " on ", as.character(trough$date), "\n", sep = "")
cat("Latest:        $", round(latest$brent_usd_bbl, 2),
    " on ", as.character(latest$date), "\n", sep = "")

events <-
  tibble(
    date  = c(W0, peak$date, trough$date),
    label = c("Conflict begins",
              paste0("Peak $", round(peak$brent_usd_bbl, 0)),
              paste0("Trough $", round(trough$brent_usd_bbl, 0))),
    y     = c(P0 - 6, peak$brent_usd_bbl + 5, trough$brent_usd_bbl - 6)
  )

fig_price <-
  ggplot(brent_win, aes(date, brent_usd_bbl)) +

  annotate("rect", xmin = W0, xmax = W1, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.45) +

  geom_hline(yintercept = P0, linetype = "dashed", colour = "grey35") +

  geom_line(linewidth = 0.9, colour = "grey15") +

  geom_point(data = bind_rows(peak, trough), size = 2.4,
             colour = "#c0392b") +

  geom_text(data = events, aes(x = date, y = y, label = label),
            hjust = 0, size = 3.1, colour = "grey25",
            inherit.aes = FALSE) +

  annotate("text", x = W0 - 28, y = P0, label = paste0("Benchmark $", round(P0, 1)),
           hjust = 0, vjust = -0.6, size = 3.1, colour = "grey35") +

  scale_y_continuous(labels = dollar) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +

  labs(
    title = "Brent crude and the pre-conflict benchmark",
    subtitle = paste0(
      "Shaded area is the headline window, ",
      format(W0, "%d %b"), " to ", format(W1, "%d %b %Y"),
      ". The price returned close to the benchmark before rising again."
    ),
    x = NULL, y = "USD per barrel", caption = SRC
  ) +
  th +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(OUTDIR, "GOICT_fig_price_path.png"), fig_price,
       width = 10, height = 5.5, dpi = 300, bg = "white")


# ======================================================================
# FIGURE B — GLOBAL CUMULATIVE, WITH THE PLATEAU VISIBLE
# ======================================================================

fig_cum <-
  H$global |>
  ggplot(aes(date, cumulative_additional_usd / 1e9)) +

  geom_area(alpha = 0.15, fill = "#c0392b") +
  geom_line(linewidth = 1.1, colour = "#c0392b") +

  geom_vline(xintercept = trough$date, linetype = "dotted",
             colour = "grey40") +

  annotate("text", x = trough$date, y = 0,
           label = "  price back near benchmark",
           hjust = 0, vjust = -0.8, size = 3.1, colour = "grey35") +

  scale_y_continuous(labels = comma) +

  labs(
    title = "Cumulative additional crude-oil import expenditure",
    subtitle = paste0(
      nrow(results) + 1, " reporting economies. Flattening in June ",
      "reflects the price returning close to the benchmark."
    ),
    x = NULL, y = "Billion USD", caption = SRC
  ) + th

ggsave(file.path(OUTDIR, "GOICT_fig_cumulative.png"), fig_cum,
       width = 10, height = 5.5, dpi = 300, bg = "white")


# ======================================================================
# FIGURE C — DUMBBELL: EXPOSURE VS REALISED
#
# The decomposition made visible. A long rightward gap means the
# country avoided expenditure by importing less. A leftward gap means
# it imported more and paid above its pre-conflict exposure.
# ======================================================================

dumb <-
  decomp |>
  slice_max(exposure_usd, n = 20) |>
  mutate(country = fct_reorder(country, exposure_bn))

fig_dumb <-
  ggplot(dumb) +

  geom_segment(
    aes(x = realised_bn, xend = exposure_bn,
        y = country, yend = country,
        colour = avoided_bn > 0),
    linewidth = 1.1, alpha = 0.6
  ) +

  geom_point(aes(x = exposure_bn, y = country),
             size = 2.8, colour = "grey30") +

  geom_point(aes(x = realised_bn, y = country,
                 colour = avoided_bn > 0), size = 2.8) +

  scale_colour_manual(
    values = c("TRUE" = "#2471a3", "FALSE" = "#c0392b"),
    labels = c("TRUE" = "Imported less than baseline",
               "FALSE" = "Imported more than baseline"),
    name = NULL
  ) +

  labs(
    title = "Exposure at pre-conflict volumes versus realised expenditure",
    subtitle = paste0(
      "Grey point: cost at pre-conflict import volumes. ",
      "Coloured point: additional expenditure actually incurred."
    ),
    x = "Billion USD", y = NULL, caption = SRC
  ) + th

ggsave(file.path(OUTDIR, "GOICT_fig_dumbbell.png"), fig_dumb,
       width = 10, height = 7, dpi = 300, bg = "white")


# ======================================================================
# FIGURE D — RANK FLIP ACROSS DENOMINATORS
#
# The same shock, ranked three ways. Shows the answer depends on the
# denominator, which is the honest framing of the incidence result.
# ======================================================================

rank_base <-
  results |>
  filter(!is.na(per_capita_usd), !is.na(pct_exports), additional_usd > 0)

top_set <-
  rank_base |>
  slice_max(additional_usd, n = 12) |>
  pull(country)

also <-
  rank_base |>
  filter(!country %in% top_set) |>
  arrange(desc(pct_exports)) |>
  slice_head(n = 4) |>
  pull(country)

keep <- c(top_set, also)

rankflip <-
  rank_base |>
  mutate(
    r_total = rank(-additional_usd,  ties.method = "min"),
    r_pc    = rank(-per_capita_usd,  ties.method = "min"),
    r_exp   = rank(-pct_exports,     ties.method = "min")
  ) |>
  filter(country %in% keep) |>
  select(country, r_total, r_pc, r_exp) |>
  pivot_longer(-country, names_to = "metric", values_to = "rank") |>
  mutate(
    metric = factor(
      metric,
      levels = c("r_total", "r_pc", "r_exp"),
      labels = c("Total\n($bn)", "Per resident\n($)",
                 "Share of\nexports (%)")
    )
  )

fig_rank <-
  ggplot(rankflip, aes(metric, rank, group = country)) +

  geom_line(aes(colour = country), linewidth = 0.9, alpha = 0.75,
            show.legend = FALSE) +
  geom_point(aes(colour = country), size = 2.6, show.legend = FALSE) +

  geom_text_repel(
    data = rankflip |> filter(metric == "Total\n($bn)"),
    aes(label = country), hjust = 1, nudge_x = -0.18,
    size = 3, direction = "y", segment.size = 0.2, seed = 1
  ) +

  geom_text_repel(
    data = rankflip |> filter(metric == "Share of\nexports (%)"),
    aes(label = country), hjust = 0, nudge_x = 0.18,
    size = 3, direction = "y", segment.size = 0.2, seed = 1
  ) +

  scale_y_reverse(breaks = c(1, 5, 10, 20, 30, 40)) +
  scale_x_discrete(expand = expansion(mult = c(0.35, 0.35))) +

  labs(
    title = "The same shock, ranked three ways",
    subtitle = paste0(
      "Rank position by each measure. Lower is a heavier burden. ",
      "The ordering changes completely with the denominator."
    ),
    x = NULL, y = "Rank", caption = SRC
  ) + th

ggsave(file.path(OUTDIR, "GOICT_fig_rankflip.png"), fig_rank,
       width = 9, height = 7.5, dpi = 300, bg = "white")


# ======================================================================
# FIGURE E — BURDEN AGAINST INCOME LEVEL
#
# Tests your own distributional claim in public. No fitted line:
# this is a description, not an estimate.
# ======================================================================

scat <-
  results |>
  filter(!is.na(pct_exports), !is.na(gdp_usd), !is.na(pop),
         additional_usd > 0) |>
  mutate(gdp_pc = gdp_usd / pop)

lab_set <-
  scat |>
  filter(pct_exports > 0.7 | gdp_pc > 60000 | gdp_pc < 5000 |
         additional_usd > 9e9)

fig_scat <-
  ggplot(scat, aes(gdp_pc, pct_exports)) +

  geom_point(aes(size = additional_usd / 1e9, colour = income),
             alpha = 0.75) +

  geom_text_repel(data = lab_set, aes(label = country),
                  size = 3, seed = 7, max.overlaps = Inf,
                  box.padding = 0.4, segment.size = 0.25) +

  scale_x_log10(labels = dollar_format(scale = 1e-3, suffix = "k")) +

  scale_size_continuous(range = c(2, 10), name = "Additional ($bn)") +

  scale_colour_brewer(palette = "Dark2", name = NULL) +

  labs(
    title = "Burden relative to export earnings, against income level",
    subtitle = paste0(
      "Each point is one reporting economy. ",
      "Point size is the absolute additional expenditure."
    ),
    x = "GDP per capita (log scale)",
    y = "Additional crude expenditure, % of export earnings",
    caption = SRC
  ) + th

ggsave(file.path(OUTDIR, "GOICT_fig_income_scatter.png"), fig_scat,
       width = 10, height = 6.5, dpi = 300, bg = "white")


# ======================================================================
# FIGURE F — GLOBAL DECOMPOSITION WATERFALL
# ======================================================================

wf <-
  tibble(
    step = factor(
      c("Exposure at\npre-conflict volumes",
        "Avoided by\nimporting less",
        "Realised additional\nexpenditure"),
      levels = c("Exposure at\npre-conflict volumes",
                 "Avoided by\nimporting less",
                 "Realised additional\nexpenditure")
    ),
    value = c(D$global$exposure, -D$global$avoided,
              D$global$realised) / 1e9,
    kind  = c("total", "change", "total")
  ) |>
  mutate(
    ymin = c(0, D$global$realised / 1e9, 0),
    ymax = c(D$global$exposure / 1e9, D$global$exposure / 1e9,
             D$global$realised / 1e9)
  )

fig_wf <-
  ggplot(wf, aes(step)) +

  geom_rect(aes(xmin = as.numeric(step) - 0.35,
                xmax = as.numeric(step) + 0.35,
                ymin = ymin, ymax = ymax, fill = kind)) +

  geom_text(aes(y = (ymin + ymax) / 2,
                label = paste0("$", round(abs(value), 1), "bn")),
            size = 3.6, fontface = "bold", colour = "white") +

  scale_fill_manual(values = c("total" = "#34495e",
                               "change" = "#2471a3"),
                    guide = "none") +

  labs(
    title = "What the price shock would have cost, and what it did cost",
    subtitle = paste0(
      "The difference is not a saving. Importers took delivery of ",
      round(D$global$shortfall / 1e6, 0),
      " million fewer barrels."
    ),
    x = NULL, y = "Billion USD", caption = SRC
  ) + th

ggsave(file.path(OUTDIR, "GOICT_fig_waterfall.png"), fig_wf,
       width = 8, height = 5.5, dpi = 300, bg = "white")


# ======================================================================
# ROBUSTNESS TABLE
#
# The appendix table that answers "why 28 February" and "why 20 days"
# before a referee asks.
# ======================================================================

goict_alt <- function(start_date, bench_days = 20,
                      stat = c("mean", "median"),
                      positive_only = FALSE) {

  stat <- match.arg(stat)

  bs <-
    GOICT$brent |>
    filter(date < start_date) |>
    slice_tail(n = bench_days)

  if (nrow(bs) < bench_days) return(NULL)

  p0 <- if (stat == "mean") {
    mean(bs$brent_usd_bbl)
  } else {
    stats::median(bs$brent_usd_bbl)
  }

  d <- GOICT$daily |> filter(date >= start_date, date <= W1)

  gap <- d$brent_usd_bbl - p0
  if (positive_only) gap <- pmax(gap, 0)

  tibble(
    start_date  = start_date,
    bench_days  = bench_days,
    stat        = stat,
    positive_only = positive_only,
    benchmark   = p0,
    total_bn    = sum(d$imports_kbd_used * 1000 * gap, na.rm = TRUE) / 1e9
  )
}

robust <-
  bind_rows(

    # start-date grid
    map_dfr(
      seq.Date(as.Date("2026-01-15"), as.Date("2026-04-15"), by = "week"),
      ~ goict_alt(.x)
    ),

    # benchmark-window grid
    map_dfr(c(10, 30, 60, 90, 120), ~ goict_alt(W0, bench_days = .x)),

    # median benchmark
    goict_alt(W0, stat = "median"),

    # positive-only variant
    goict_alt(W0, positive_only = TRUE)
  )

cat("\n=== ROBUSTNESS ===\n")
print(
  robust |>
    transmute(
      start_date, bench_days, stat, positive_only,
      benchmark = round(benchmark, 2),
      total_bn  = round(total_bn, 1)
    ),
  n = Inf, width = Inf
)

cat("\nRange across all variants: $",
    round(min(robust$total_bn), 1), " bn to $",
    round(max(robust$total_bn), 1), " bn\n", sep = "")

baseline_row <- robust |> filter(start_date == W0, bench_days == 20,
                                 stat == "mean", !positive_only)

cat("Baseline specification:    $",
    round(baseline_row$total_bn, 1), " bn\n", sep = "")

write_csv(robust, file.path(OUTDIR, "GOICT_robustness.csv"))


# ======================================================================
# COVERAGE STATEMENT
# ======================================================================

cat("\n=== COVERAGE, FOR THE DATA SECTION ===\n")
cat("Crude imports represented: ",
    round(sum(results$mean_imports_kbd, na.rm = TRUE) / 1000, 2),
    " mb/d\n", sep = "")
cat("World seaborne + pipeline crude trade is roughly 43-48 mb/d,\n")
cat("so coverage is approximately ",
    round(100 * sum(results$mean_imports_kbd, na.rm = TRUE) / 1000 / 45),
    "%. Verify against the IEA Oil Market Report\n", sep = "")
cat("for the exact figure before publishing.\n")


cat("\n", strrep("=", 70), "\n", sep = "")
cat("Six figures and the robustness table saved to ", OUTDIR, "\n", sep = "")
cat(strrep("=", 70), "\n\n", sep = "")
