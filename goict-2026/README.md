# GOICT-2026 — Global Oil Import Cost Tracker

Additional crude-oil import expenditure during the 2026 Middle East
conflict, measured from reported import volumes and observed prices.

Author: Talha Omer. © 2026 Talha Omer.
Educational and informational purposes only; not financial, investment
or policy advice.

## What runs automatically

Every Monday at 06:00 UTC a GitHub Action:

1. downloads the latest JODI crude-oil imports and EIA Brent prices,
2. runs the full pipeline (`run_all.R`),
3. runs `validate.R` — if any check fails, it stops here, nothing is
   published, and GitHub emails you the log,
4. commits the refreshed data files,
5. redeploys the app to shinyapps.io.

The app itself never downloads anything. It only reads the four files
in `GOICT_2026_output/`, so a failed download can never break it.

## Checks that must pass before publishing

- the decomposition identity holds, and country totals sum to the global
- at least 50 economies in the sample
- no Brent daily move above 20% in the last 90 days (catches parse errors)
- the headline has not moved more than 25% unless the window extended

## Files

| File | Role |
|---|---|
| `GOICT_01_patches.R` | download JODI and EIA, compute the daily measure |
| `GOICT_01b_window.R` | set the window: latest month filed by ≥70% of reporters |
| `GOICT_02a_results.R` | denominators and country results |
| `GOICT_04_decomposition.R` | baseline comparison |
| `GOICT_06_fixes_600dpi.R` | corrected shortfall; 600 dpi figures when run locally |
| `GOICT_07_publish.R` | provisional extension (calendar days) and status file |
| `run_all.R` / `validate.R` / `deploy.R` | automation |
| `GOICT_app.R` | the Shiny app |
| `GOICT_03_maps.R`, `GOICT_05_figures.R` | paper figures (local only) |
| `manuscript/` | LaTeX source and bibliography |

## One-time setup

In the repository: Settings → Secrets and variables → Actions →
New repository secret. Add three, from shinyapps.io → Account → Tokens:

- `SHINYAPPS_NAME`
- `SHINYAPPS_TOKEN`
- `SHINYAPPS_SECRET`

Then Actions → "GOICT-2026 weekly update" → Run workflow, to test it.

## Things to know

- The paper's frozen figures correspond to the vintage of 18 September
  2026. The live app will differ as JODI publishes and revises data.
- The JODI 2026 file URL is year-specific. In January 2027 add the 2027
  file to `GOICT_01_patches.R`.
- GitHub pauses scheduled workflows after 60 days without repository
  activity. The weekly data commit keeps it active.
