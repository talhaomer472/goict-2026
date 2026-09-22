# ======================================================================
# GOICT-2026 — GLOBAL OIL IMPORT COST TRACKER
# Single-page Shiny app
#
# Run from the folder containing GOICT_2026_output/
#   shiny::runApp("GOICT_app.R")
#
# No map tiles and no API key. The map is drawn with ggplot from
# country polygons, so it works offline and deploys cleanly.
# ======================================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

OUT <- "GOICT_2026_output"

R <- readRDS(file.path(OUT, "GOICT_results.rds"))
D <- readRDS(file.path(OUT, "GOICT_decomposition.rds"))

P0      <- R$benchmark
W0      <- R$window_start
W1      <- R$window_end
VINTAGE <- R$vintage

res <-
  R$results |>
  mutate(
    pop            = if_else(iso2 == "TW", 23400000, pop),
    gdp_usd        = if_else(iso2 == "TW", 790e9,    gdp_usd),
    per_capita_usd = additional_usd / pop,
    pct_gdp        = 100 * additional_usd / gdp_usd,
    bbl_per_head   = mean_imports_kbd * 1000 * 365 / pop
  ) |>
  left_join(
    D$decomp |> select(iso2, q0_kbd, q_mean_kbd, volume_change_pct),
    by = "iso2"
  ) |>
  arrange(desc(additional_usd))

# All reporting economies are kept. Some report zero or negligible
# crude-oil imports over the window and contribute nothing.
N_ALL      <- nrow(res)
N_POSITIVE <- sum(res$additional_usd > 0)

# Brunei is mapped but excluded from rankings: at 127 barrels per
# resident per year it refines for export, so its per-resident value
# is not domestic consumption.
rankable <- res |> filter(iso2 != "BN", additional_usd > 0)

REPORTED   <- sum(res$additional_usd)
TOTALBILL  <- R$global_total_usd

# Everything that changes with each data update is read from the status
# file written by the pipeline, so nothing here goes stale.
ST_FILE <- file.path(OUT, "GOICT_app_status.rds")
ST <- if (file.exists(ST_FILE)) readRDS(ST_FILE) else list(
  provisional_extra_usd = NA, provisional_to = W1, n_days = R$n_days)

PROV_TOTAL <- REPORTED + ST$provisional_extra_usd
PROV_TO    <- as.Date(ST$provisional_to)
N_DAYS     <- ST$n_days

# labels derived from the data
LAB_END      <- format(W1, "%d %b")
LAB_WINDOW   <- sprintf("%s to %s", format(W0, "%d %B"),
                        format(W1, "%d %B %Y"))
LAB_DURING   <- sprintf("During the war\n(%s-%s)",
                        format(W0 + 1, "%b"), format(W1, "%b"))

world <-
  ne_countries(scale = "medium", returnclass = "sf") |>
  filter(admin != "Antarctica") |>
  mutate(iso3 = case_when(
    !is.na(iso_a3)    & iso_a3    != "-99" ~ iso_a3,
    !is.na(iso_a3_eh) & iso_a3_eh != "-99" ~ iso_a3_eh,
    TRUE                                   ~ adm0_a3)) |>
  select(iso3, geometry) |>
  left_join(res |> select(iso3, country, additional_billion,
                          pct_exports, per_capita_usd, pct_gdp),
            by = "iso3")

METRICS <- c(
  "Per resident ($)"             = "per_capita_usd",
  "Total additional ($ bn)"      = "additional_billion",
  "Share of export earnings (%)" = "pct_exports",
  "Share of GDP (%)"             = "pct_gdp")

BREAKS <- list(
  pct_exports        = c(0, .1, .25, .5, .75, 1, 2, Inf),
  additional_billion = c(0, .5, 1, 2.5, 5, 10, 25, Inf),
  per_capita_usd     = c(0, 10, 25, 50, 100, 150, 250, Inf),
  pct_gdp            = c(0, .05, .1, .2, .3, .5, .75, Inf))

LABELS <- list(
  pct_exports        = c("<0.1","0.1-0.25","0.25-0.5","0.5-0.75",
                         "0.75-1","1-2",">2"),
  additional_billion = c("<0.5","0.5-1","1-2.5","2.5-5","5-10",
                         "10-25",">25"),
  per_capita_usd     = c("<10","10-25","25-50","50-100","100-150",
                         "150-250",">250"),
  pct_gdp            = c("<0.05","0.05-0.1","0.1-0.2","0.2-0.3",
                         "0.3-0.5","0.5-0.75",">0.75"))

th <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey85"),
    plot.title    = element_text(face = "bold", size = 14,
                                 colour = "grey10"),
    plot.subtitle = element_text(size = 11, colour = "grey25"),
    axis.title    = element_text(face = "bold", size = 12,
                                 colour = "grey15"),
    axis.text     = element_text(face = "bold", size = 11,
                                 colour = "grey15"),
    legend.title  = element_text(face = "bold", colour = "grey15"),
    legend.text   = element_text(face = "bold", colour = "grey15"),
    plot.margin   = margin(10, 18, 10, 10))


# ----------------------------------------------------------------------
ui <- page_sidebar(

  title = "GOICT-2026 — Global Oil Import Cost Tracker",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  sidebar = sidebar(
    width = 320,

    selectizeInput("country",
                   "Change the country to see the effect of the oil price",
                   choices = sort(res$country), selected = "India"),

    hr(),
    h6("Price scenario"),
    sliderInput("price", "Average Brent ($/bbl)",
                min = 60, max = 200, value = 120, step = 5),
    sliderInput("days", "Horizon (days)",
                min = 30, max = 365, value = 180, step = 30),
    uiOutput("scen_note"),

    hr(),
    selectInput("metric", "Show the cost measured as",
                choices = METRICS, selected = "per_capita_usd"),
    uiOutput("metric_help"),

    hr(),
    p(class = "small text-muted",
      sprintf("Pre-war benchmark $%.2f/bbl. Window %s to %s. Vintage %s.",
              P0, format(W0, "%d %b"), format(W1, "%d %b %Y"), VINTAGE)),
    downloadButton("dl", "Download data (CSV)",
                   class = "btn-sm btn-outline-secondary")
  ),

  # ---- introduction ----
  div(
    class = "mb-2",
    h4(class = "fw-bold mb-1",
       "How much more did countries pay for crude oil because of the
        2026 Middle East conflict?"),
    p(class = "text-muted mb-0",
      "Pick a country on the left to see what it paid, and move the
       sliders to see what a different oil price would cost it.")
  ),

  # ---- selected country: changeable on top, fixed below ----
  card(
    card_header(textOutput("cty_head", inline = TRUE)),
    card_body(
      fillable = FALSE,

      div(class = "alert alert-warning py-2 mb-2",
          strong("You can change this. "),
          "Move the two sliders on the left to set a different oil
           price and time period. Everything in this section updates
           as you move them."),

      uiOutput("scen_head"),
      uiOutput("cty_scen_boxes"),
      plotOutput("cty_scen", height = "420px", width = "100%"),

      hr(class = "my-4"),

      div(class = "alert alert-secondary py-2 mb-2",
          strong("This does not change. "),
          paste("Recorded figures for", LAB_WINDOW, "from
           reported import volumes and observed prices. The sliders do
           not affect them.")),

      uiOutput("cty_boxes"),
      layout_column_wrap(
        width = 1/2, fill = FALSE,
        plotOutput("cty_vol",  height = "320px", width = "100%"),
        plotOutput("cty_rank", height = "320px", width = "100%")
      )
    )
  ),

  # ---- worldwide totals ----
  layout_column_wrap(
    width = 1/4, fill = FALSE, heights_equal = "row",
    value_box(paste("Reported to", LAB_END), max_height = "130px",
              sprintf("$%.1f bn", REPORTED / 1e9),
              p(class = "small", "Published import volumes"),
              theme = "danger"),
    value_box(paste("Provisional to", format(PROV_TO, "%d %b")), max_height = "130px",
              sprintf("$%.1f bn", PROV_TOTAL / 1e9),
              p(class = "small", "Volumes projected after June"),
              theme = "secondary"),
    value_box("Above pre-war price", max_height = "130px",
              sprintf("%.1f%%", 100 * REPORTED / TOTALBILL),
              theme = "warning"),
    value_box("Countries", N_ALL, max_height = "130px",
              p(class = "small",
                sprintf("%d reporting imports", N_POSITIVE)),
              theme = "secondary")
  ),

  # ---- map and ranking ----
  card(card_header("World map"),
       card_body(fillable = FALSE,
                 plotOutput("map", height = "480px", width = "100%"))),

  card(card_header("Ten largest"),
       card_body(fillable = FALSE,
                 plotOutput("top10", height = "420px", width = "100%"))),

  # ---- footer ----
  card(
    card_body(
      class = "small text-muted",
      p(class = "mb-1",
        strong("Information tool. "),
        "Descriptive estimates for educational and informational
         purposes only. Not financial, investment or policy advice.
         The figures are an accounting quantity, not a causal
         estimate."),
      p(class = "mb-1",
        strong("Wording. "),
        "The per-resident figure is money that left the country to pay
         for crude oil, not what an individual paid at the pump.
         Singapore, the Netherlands, Belgium and Lithuania refine for
         other markets, so their per-resident values are not domestic
         consumption."),
      p(class = "mb-1",
        strong("Coverage. "),
        sprintf("All %d countries reporting crude-oil imports to JODI
                 are included; %d report zero or negligible crude
                 imports. Countries importing refined products rather
                 than crude do not appear, so totals are a lower bound.
                 Brunei is mapped but excluded from rankings. Sources:
                 JODI Oil, U.S. EIA, World Bank.",
                N_ALL, N_ALL - N_POSITIVE)),
      p(class = "mb-2",
        strong("Author: Talha Omer. "),
        "\u00A9 2026 Talha Omer."),
      div(
        class = "p-2 bg-light border rounded",
        strong("How to cite"),
        tags$pre(
          class = "mb-0 mt-1 small",
          style = "white-space:pre-wrap; background:transparent;",
          sprintf(paste0(
            "Omer, T. (2026). GOICT-2026: Global Crude-Oil Import Costs ",
            "During the 2026 Middle East Conflict. [Journal, volume, ",
            "pages, DOI to be added.]\n",
            "Tracker: GOICT-2026, data vintage %s. ",
            "[App URL to be added.]"), VINTAGE)))
    )
  )
)


# ----------------------------------------------------------------------
server <- function(input, output, session) {

  mlab <- reactive(names(METRICS)[METRICS == input$metric])

  HELP <- list(
    per_capita_usd = "The additional expenditure divided by population.
      Money that left the country per resident, not what an individual
      paid at the pump. Inflated for countries that refine for export.",
    additional_billion = "The additional expenditure in dollars. This
      ranking largely tracks the size of the economy and its import
      volume.",
    pct_exports = "The additional crude bill divided by annual exports
      of goods and services. Oil is paid for in dollars and exports are
      how a country earns them, so this measures the drain on the
      capacity to pay.",
    pct_gdp = "The additional expenditure as a share of annual GDP.
      The burden relative to what the economy produces.")

  output$metric_help <- renderUI(
    p(class = "small text-muted mt-1 mb-2", HELP[[input$metric]]))

  cr <- reactive(res |> filter(country == input$country))

  output$cty_head <- renderText(input$country)

  # ---- scenario -------------------------------------------------------

  scen <- reactive({
    r <- cr(); req(nrow(r) == 1)
    q_lo <- r$mean_imports_kbd
    q_hi <- if (is.na(r$q0_kbd)) r$mean_imports_kbd else r$q0_kbd
    gap  <- input$price - P0
    list(lo = q_lo * 1000 * gap * input$days,
         hi = q_hi * 1000 * gap * input$days,
         gap = gap, pop = r$pop, q_lo = q_lo, q_hi = q_hi)
  })

  output$scen_head <- renderUI({
    h5(class = "fw-bold mb-2",
       sprintf("If oil averages $%d a barrel for the next %d days",
               input$price, input$days))
  })

  output$scen_note <- renderUI({
    s <- scen()
    lo <- min(s$lo, s$hi); hi <- max(s$lo, s$hi)
    if (s$gap >= 0) {
      div(class = "alert alert-warning small p-2 mt-2",
          strong(sprintf("$%.1f - %.1f bn extra",
                         min(lo, hi)/1e9, max(lo, hi)/1e9)))
    } else {
      div(class = "alert alert-success small p-2 mt-2",
          strong(sprintf("$%.1f - %.1f bn saved",
                         min(abs(lo), abs(hi))/1e9,
                         max(abs(lo), abs(hi))/1e9)),
          br(),
          sprintf("Below the $%.2f pre-war price, so this is a saving.",
                  P0))
    }
  })

  output$cty_scen_boxes <- renderUI({
    s <- scen(); r <- cr(); req(nrow(r) == 1)
    lo <- min(s$lo, s$hi); hi <- max(s$lo, s$hi)
    saving <- s$gap < 0
    lab <- if (saving) "saved" else "extra"
    col <- if (saving) "success" else "danger"

    layout_column_wrap(
      width = 1/3,
      value_box(if (saving) "Saved" else "Extra cost", max_height = "130px",
                sprintf("$%.1f - %.1f bn",
                        min(abs(lo), abs(hi))/1e9,
                        max(abs(lo), abs(hi))/1e9),
                theme = col),
      value_box("Per resident", max_height = "130px",
                sprintf("$%.0f - %.0f",
                        min(abs(lo), abs(hi))/s$pop,
                        max(abs(lo), abs(hi))/s$pop),
                theme = if (saving) "success" else "warning"),
      value_box("Versus actual", max_height = "130px",
                sprintf("%.1fx", abs(hi) / r$additional_usd),
                p(class = "small",
                  sprintf("actual was $%.2f bn", r$additional_billion)),
                theme = "secondary")
    )
  })

  output$cty_scen <- renderPlot({
    s <- scen()
    g <- data.frame(price = seq(60, 200, by = 5)) |>
      mutate(hi = s$q_hi * 1000 * (price - P0) * input$days / 1e9,
             lo = s$q_lo * 1000 * (price - P0) * input$days / 1e9)

    ggplot(g, aes(price)) +
      geom_ribbon(aes(ymin = pmin(lo, hi), ymax = pmax(lo, hi)),
                  fill = "#2c7fb8", alpha = 0.25) +
      geom_line(aes(y = hi), colour = "#2c7fb8", linewidth = 0.9) +
      geom_hline(yintercept = 0, colour = "grey50") +
      geom_vline(xintercept = P0, colour = "#e67e22", linewidth = 1.3) +
      geom_vline(xintercept = input$price, colour = "#c0392b",
                 linewidth = 1.1) +
      annotate("label", x = P0, y = Inf,
               label = sprintf("Pre-war price $%.0f", P0),
               hjust = 0.5, vjust = 1.1, size = 5, fontface = "bold",
               colour = "#e67e22", fill = "white", label.size = 0.6) +
      annotate("label", x = input$price, y = -Inf,
               label = sprintf("Your price $%.0f", input$price),
               hjust = 0.5, vjust = -0.2, size = 5, fontface = "bold",
               colour = "#c0392b", fill = "white", label.size = 0.6) +
      labs(x = "Average Brent ($/bbl)", y = "Billion USD",
           title = sprintf("%s, %d-day scenario",
                           input$country, input$days),
           subtitle = "Above zero is extra cost, below zero a saving") +
      th
  })

  # ---- fixed ----------------------------------------------------------

  output$cty_boxes <- renderUI({
    r <- cr(); req(nrow(r) == 1)
    layout_column_wrap(
      width = 1/5,
      value_box("Additional", sprintf("$%.2f bn", r$additional_billion), max_height = "120px",
                theme = "danger"),
      value_box("Per resident", sprintf("$%.0f", r$per_capita_usd), max_height = "120px",
                theme = "warning"),
      value_box("% of exports", sprintf("%.2f%%", r$pct_exports), max_height = "120px",
                theme = "secondary"),
      value_box("% of GDP", sprintf("%.3f%%", r$pct_gdp), max_height = "120px",
                theme = "secondary"),
      value_box("Crude imports", max_height = "120px",
                sprintf("%.0f kb/d", r$mean_imports_kbd),
                theme = "secondary")
    )
  })

  output$cty_vol <- renderPlot({
    r <- cr(); req(nrow(r) == 1)
    if (is.na(r$q0_kbd) || is.na(r$q_mean_kbd) || r$q0_kbd <= 0) {
      return(ggplot() +
        annotate("text", x = 0, y = 0, size = 5, fontface = "bold",
                 colour = "grey25",
                 label = "No pre-war import baseline\navailable for this country.") +
        theme_void())
    }
    chg <- 100 * (r$q_mean_kbd - r$q0_kbd) / r$q0_kbd
    d <- data.frame(
      when = factor(c("Before the war\n(Dec-Feb)", LAB_DURING),
                    levels = c("Before the war\n(Dec-Feb)", LAB_DURING)),
      kbd  = c(r$q0_kbd, r$q_mean_kbd))
    ggplot(d, aes(when, kbd)) +
      geom_col(fill = c("#95a5a6", if (chg < 0) "#2471a3" else "#c0392b"),
               colour = "grey30", width = 0.55) +
      geom_text(aes(label = comma(round(kbd))), vjust = -0.5,
                size = 4.5, fontface = "bold", colour = "grey10") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                         labels = comma) +
      labs(x = NULL, y = "Thousand barrels per day",
           title = "Crude oil imports",
           subtitle = sprintf("%s %.0f%% during the war",
                              if (chg < 0) "Down" else "Up",
                              abs(chg))) + th
  })

  output$cty_rank <- renderPlot({
    r <- cr(); req(nrow(r) == 1)
    if (!r$country %in% rankable$country) {
      return(ggplot() +
        annotate("text", x = 0, y = 0, size = 5, fontface = "bold",
                 colour = "grey25",
                 label = paste("Not ranked.",
                               "No reported crude imports over the",
                               "window, or refines for export.",
                               sep = "\n")) +
        theme_void())
    }
    data.frame(
      measure = factor(c("Total","Per resident","% exports","% GDP"),
                       levels = c("Total","Per resident",
                                  "% exports","% GDP")),
      rank = c(rank(-rankable$additional_usd)[rankable$country == r$country],
               rank(-rankable$per_capita_usd)[rankable$country == r$country],
               rank(-rankable$pct_exports)[rankable$country == r$country],
               rank(-rankable$pct_gdp)[rankable$country == r$country])) |>
      ggplot(aes(measure, rank)) +
      geom_col(fill = "#f0ad4e", colour = "grey30", width = 0.6) +
      geom_text(aes(label = paste0("#", round(rank))),
                vjust = -0.4, size = 4.5, fontface = "bold",
                colour = "grey10") +
      scale_y_reverse(expand = expansion(mult = c(0.18, 0.05))) +
      labs(x = NULL, y = "Rank",
           title = sprintf("Rank out of %d countries",
                           nrow(rankable))) + th
  })

  # ---- map and ranking ------------------------------------------------

  output$map <- renderPlot({
    m <- input$metric
    d <- world |>
      mutate(band = cut(.data[[m]], breaks = BREAKS[[m]],
                        labels = LABELS[[m]], include.lowest = TRUE,
                        right = FALSE))
    ggplot(d) +
      geom_sf(aes(fill = band), colour = "white", linewidth = 0.1) +
      scale_fill_brewer(palette = "YlGnBu", na.value = "grey88",
                        name = NULL, drop = FALSE,
                        guide = guide_legend(nrow = 1)) +
      coord_sf(ylim = c(-58, 84), expand = FALSE) +
      labs(title = mlab()) +
      theme_void(base_size = 12) +
      theme(legend.position = "bottom",
            legend.key.height = unit(0.32, "cm"),
            legend.key.width  = unit(1.0, "cm"),
            legend.text = element_text(size = 9, face = "bold",
                                       colour = "grey15"),
            plot.title = element_text(face = "bold", size = 14,
                                      colour = "grey10"))
  })

  output$top10 <- renderPlot({
    rankable |>
      filter(!is.na(.data[[input$metric]])) |>
      slice_max(.data[[input$metric]], n = 10) |>
      mutate(country = reorder(country, .data[[input$metric]])) |>
      ggplot(aes(country, .data[[input$metric]])) +
      geom_col(fill = "#2c7fb8") +
      geom_text(aes(label = round(.data[[input$metric]], 1)),
                hjust = -0.15, size = 3.6, fontface = "bold",
                colour = "grey10") +
      coord_flip(clip = "off") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      labs(x = NULL, y = mlab(), title = mlab(),
           subtitle = "Brunei excluded: refines for export") + th
  })

  output$dl <- downloadHandler(
    filename = function() sprintf("GOICT_2026_%s.csv", VINTAGE),
    content  = function(f) utils::write.csv(res, f, row.names = FALSE))
}

shinyApp(ui, server)
