# Weekly ensemble of MAE weighted samples
# Steps:
# - Score MAE
# - Inverse weighting
# - Ensemble
#
# Rules as per forecast hub:
# Start only after 4 weeks data
# Weights should use all available data up to point of forecast
library(here)
library(dplyr)
library(lubridate)
library(purrr)
library(ggplot2)
source(here("code", "import-results.R"))
source(here("code", "format-results.R"))
source(here("code", "create-ensembles.R"))
source(here("code", "score-samples.R"))
local <- TRUE

# Load samples from all models
results <- import_results(round = 2, local = local)

# Clean and include observed data where available
results <- format_results(results = results,
                          n_model_min = 3, local = local)

# Dates for ensemble creation
latest_data <- max(subset(results, !is.na(obs))$target_end_date)
ensemble_dates <- unique(results$target_end_date)
forecast_dates <- ensemble_dates[5:length(ensemble_dates)] # start at 4 weeks in
forecast_dates <- forecast_dates[forecast_dates <= latest_data + 7]

# Divide results into "real time": weekly 4 week forecasts with expanding history of projections and observations
#   Projections & data up to forecast date,
#     plus 4 week horizon forecast (with "real time" = 1 week) with no data
results_window <- map(forecast_dates,
                      ~ results |>
                        group_by(location, target_variable) |>
                        # keep projections up to forecasting date
                        #    + 4 weeks ahead
                        filter(target_end_date <= .x + weeks(16)) |> # 4
                        # remove observed data in forecast horizon
                        mutate(obs_100k = ifelse(target_end_date < .x,
                                                 obs_100k, NA)))
names(results_window) <- forecast_dates


# Score sample weights over time
weights_time <- map_dfr(results_window,
                        ~ score_samples(results = .x, truncate_weeks = 0),
                        .id = "forecast_date") |>
  ungroup()

# summarise weights
weights <- weights_time |>
  ungroup() |>
  filter(horizon == 1) |>
  mutate(forecast_date = as.Date(forecast_date)) |>
  select(forecast_date, location, target_variable, scenario_id,
         model, sample,
         mae, weight) |>
  mutate(sample_scenario = paste0(substr(model, 1, 3), sample, scenario_id),
         target = paste(location, target_variable),
         scenario_id = ordered(scenario_id))

# Create ensemble forecasts
ensemble_all <- map_dfr(results_window,
                        ~ create_ensembles(results = .x,
                                           truncate_weeks = 0),
                        .id = "forecast_date") |>
  mutate(model = ifelse(scenario_id == "Weighted", "Weighted", model)) |>
  # only keep forecasts, not "past" projections
  filter(target_end_date > forecast_date)

# see plotting code for weights + ensembles: plot-x-over-time.R
