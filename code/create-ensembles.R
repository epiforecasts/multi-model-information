library(here)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
# local <- TRUE
# results <- import_projections(round = 2, local = local, n_model_min = 3)

# Create ensembles from raw samples and from quantiles of each model ----------
create_simple_ensembles <- function(results,
                                    quantiles = c(0.01, 0.025,
                                                  seq(0.05, 0.95, by = 0.05),
                                                  0.975, 0.99)) {

  # Create simple ensemble of samples -----
  ensemble_samples <- results |>
    group_by(round, location, target_variable, target_end_date, scenario_id) |>
    reframe(
      n = n(),
      value = quantile(value_100k, quantiles),
      quantile = paste0("q", quantiles),
      model = "Trajectories"
    )

  # Create ensemble of quantile-summarised-samples -----
  # ----- take quantiles from models
  ensemble_quantile <- results |>
    group_by(round, location, target_variable, target_end_date, scenario_id,
             model) |>
    reframe(
      value = quantile(value_100k, quantiles),
      quantile = paste0("q", quantiles)
    ) |>
    # ----- take median of each quantile
    group_by(round, location, target_variable, target_end_date, scenario_id,
             quantile) |>
    summarise(
      n = n(),
      value = median(value),
      model = "Quantiles"
    )

  # Combine -----
  ensembles <- bind_rows(ensemble_quantile, ensemble_samples)

  return(ensembles)
}



# LOP ensemble ------------------------------------------------------------
create_lop_ensemble <- function(results,
                                quantiles = c(0.01, 0.025,
                                              seq(0.05, 0.95, by = 0.05),
                                              0.975, 0.99)) {
  # ----- take quantiles from models
  model_quantiles <- results |>
    group_by(round, location, target_variable, target_end_date, scenario_id,
             model) |>
    reframe(
      value = quantile(value_100k, quantiles),
      output_type_id = quantiles,
    ) |>
    rename(model_id = model) |>
    mutate(output_type = "quantile")

  # ----- create linear pool (quantiles; interpolate; take quantiles)
  lp_from_qs <- hubEnsembles::linear_pool(model_quantiles)
  lp_from_qs <- lp_from_qs |>
    rename(quantile = output_type_id,
           model = model_id) |>
    mutate(quantile = paste0("q", quantile),
           model = "Linear pool")

  return(lp_from_qs)
}

# Weekly ensemble of progressively MAE weighted samples -------------------
# Steps:
# - Score MAE
# - Inverse weighting
# - Ensemble
#
# Start only after 4 weeks data
# Weights use all available data up to point of forecast

# Set up
source(here("code", "import-results.R"))
source(here("code", "score-samples.R"))

# Load samples from all models, with observed data
create_weekly_ensembles <- function(results) {
  # Dates for ensemble creation
  latest_data <- max(subset(results, !is.na(obs))$target_end_date)
  ensemble_dates <- unique(results$target_end_date)
  # -----EDIT----
  # forecast_dates <- ensemble_dates[5:length(ensemble_dates)] # start at 4 weeks in
  #
  forecast_dates <- ensemble_dates
  #
  # forecast_dates <- forecast_dates[forecast_dates <= latest_data + 7] # only forecast up to where we can evaluate with observed data
  #---- end edit----

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
  quantiles <- c(0.01, 0.025,
                 seq(0.05, 0.95, by = 0.05),
                 0.975, 0.99)
  ensembles <- map_dfr(results_scored,
                       ~ .x |>
                         group_by(location, target_variable,
                                  target_end_date,
                                  n_weeks_scored) |>
                         reframe(
                           value = cNORM::weighted.quantile.harrell.davis(
                             x = value_100k,
                             probs = quantiles,
                             weights = weight),
                           quantile = paste0("q", quantiles),
                           model = "Trajectories",
                           scenario_id = "Weighted",
                         ),
                       .id = "forecast_date") |>
    ungroup()

  # Add an unweighted ensemble (one for each equivalent to all the weighted ensembles) to use as a baseline for evaluation
  ensembles_unweighted <- map_dfr(results_scored,
                       ~ .x |>
                         group_by(location, target_variable,
                                  target_end_date,
                                  n_weeks_scored) |>
                         reframe(
                           value = quantile(value_100k, quantiles),
                           quantile = paste0("q", quantiles),
                           model = "Trajectories unweighted",
                           scenario_id = "Unweighted",
                         ),
                       .id = "forecast_date") |>
    ungroup()

  ensembles <- bind_rows(ensembles, ensembles_unweighted) |>
    mutate(n_weeks_scored = ifelse(is.na(n_weeks_scored), 0, n_weeks_scored),
           horizon = as.numeric(target_end_date - as.Date(forecast_date)) / 7)

  weekly <- list(ensembles = ensembles,
                           weights = weights)

  return(weekly)
}
