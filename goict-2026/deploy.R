# ======================================================================
# GOICT-2026 — deploy the app to shinyapps.io
# Credentials come from GitHub secrets, never from this file.
# ======================================================================

rsconnect::setAccountInfo(
  name   = Sys.getenv("SHINYAPPS_NAME"),
  token  = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET"))

rsconnect::deployApp(
  appDir        = ".",
  appFiles      = c("GOICT_app.R",
                    "GOICT_2026_output/GOICT_results.rds",
                    "GOICT_2026_output/GOICT_decomposition.rds",
                    "GOICT_2026_output/GOICT_headline.rds",
                    "GOICT_2026_output/GOICT_app_status.rds"),
  appPrimaryDoc = "GOICT_app.R",
  appName       = "goict-2026",
  forceUpdate   = TRUE,
  launch.browser = FALSE)
