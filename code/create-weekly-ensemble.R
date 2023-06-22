# Weekly ensemble of progressively MAE weighted samples
# Steps:
# - Score MAE
# - Inverse weighting
# - Ensemble
#
# Rules as per forecast hub:
# Start only after 4 weeks data
# Weights should use all available data up to point of forecast

# Set up -----
library(here)
library(dplyr)
library(lubridate)
library(purrr)
source(here("code", "import-results.R"))
source(here("code", "score-samples.R"))

# local <- TRUE
# results <- import_projections(round = 2, local = local, n_model_min = 3)

# Load samples from all models, with observed data -----
create_weekly_ensembles <- function(results) {
  # Dates for ensemble creation
  latest_data <- max(subset(results, !is.na(obs))$target_end_date)
  ensemble_dates <- unique(results$target_end_date)
  forecast_dates <- ensemble_dates[5:length(ensemble_dates)] # start at 4 weeks in
  forecast_dates <- forecast_dates[forecast_dates <= latest_data + 7] # only forecast up to where we can evaluate with observed data

  # Divide results with weekly expanding history of observations
  results_window <- map(forecast_dates,
                        ~ results |>
                          mutate(obs_100k = ifelse(target_end_date < .x,
                                                   obs_100k, NA)))
  names(results_window) <- forecast_dates

  # Score samples, to look at the weights -----
  message("Scoring samples with increasing weeks of data")
  results_scored <- map(results_window,
                        ~ score_samples(results = .x, truncate_weeks = 0))

  # look at weights
  weights <- bind_rows(results_scored, .id = "forecast_date")

  # Ensemble weighted samples for each week of results -----
  message("Creating ensembles from progressively scored samples")
  quantiles <- c(0.01, 0.25, 0.5, 0.75, 0.99)
  ensembles <- map_dfr(results_scored,
                       ~ .x |>
                         group_by(location, target_variable, target_end_date) |>
                         summarise(value = cNORM::weighted.quantile.harrell.davis(
                           x = value_100k,
                           probs = quantiles,
                           weights = weight),
                           quantile = paste0("q", quantiles),
                           model = "Samples",
                           scenario_id = "Weighted",
                           .groups = "drop"
                         ),
                       .id = "forecast_date") |>
    ungroup()

  # only keep forecasts for specified horizons
  ensembles <- ensembles |>
    mutate(horizon = as.numeric(target_end_date - as.Date(forecast_date)) / 7)
  # ie conditioned on data up to n weeks ago

  weekly_ensembles <- list(ensembles = ensembles,
                           weights = weights)

  return(weekly_ensembles)
}

