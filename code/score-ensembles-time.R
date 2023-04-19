# Weekly ensemble of MAE weighted samples
# Steps:
# - Score MAE
# - Inverse weighting
# - Ensemble
#
# Rules as per forecast hub:
# Start only after 4 weeks data
# Forecast only 4 weeks ahead
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
                        # keep projections up to forecasting date + 4 weeks ahead
                        filter(target_end_date <= .x + weeks(4)) |>
                        # remove observed data in forecast horizon
                        mutate(obs_100k = ifelse(target_end_date < .x,
                                                 obs_100k, NA)))
names(results_window) <- forecast_dates


# Sample weights over time --------------------------------------------------
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
         mae, weight)

weights |>
  mutate(sample_scenario = paste(model, sample, scenario_id),
         target = paste(location, target_variable),
         scenario_id = ordered(scenario_id)) |>
  ggplot(aes(x = forecast_date, y = weight,
             group = sample_scenario,
             col = model, fill = model
             )) +
  # geom_line() +
  geom_col() +
  labs(x = NULL, y = "Weight of samples") +
  facet_wrap(~target, nrow = 5, scales = "free") +
  theme_bw() +
  theme(legend.position = "right")

ggsave("weights-time-sum.jpg", width = 6, height = 12)


# Ensembles ---------------------------------------------------------------
# Create ensemble forecasts
ensemble_all <- map_dfr(results_window,
                        ~ create_ensembles(results = .x,
                                           truncate_weeks = 0),
                        .id = "forecast_date") |>
  mutate(model = ifelse(scenario_id == "Weighted", "Weighted", model)) |>
  # only keep forecasts, not "past" projections
  filter(target_end_date > forecast_date)

# plot weighted ensemble
obs_data <- results |>
  distinct(location, target_variable, target_end_date, obs_100k)

# ensemble_weighted <- ensemble_all |>
#   filter(model == "Samples Weighted")

ensemble_all |>
  # ----- Data
  pivot_wider(names_from = quantile) |>
  mutate(median = q0.5) |>
  select(-q0.5) |>
  left_join(obs_data, by = c("location", "target_variable", "target_end_date")) |>
  mutate(forecast_date = as.Date(forecast_date),
         target = ordered(paste(location, target_variable)),
         model = ordered(model),
         scenario_id = ordered(scenario_id)) |>
  # ----- Plot
  ggplot(aes(x = target_end_date,
             #group = forecast_date,
             col = scenario_id,
             fill = scenario_id
             )) +
  # ----- Geoms
  # ensembles
  geom_ribbon(aes(ymin = q0.01, ymax = q0.99),
              col = NA, alpha = 0.1) +
  geom_ribbon(aes(ymin = q0.25, ymax = q0.75),
              col = NA, alpha = 0.4) +
  geom_line(aes(y = median), lwd = 1) +
  # observed data as points
  geom_point(aes(y = obs_100k),
             colour = "black", size = 0.6,
             show.legend = FALSE) +
  # ----- Structure
  # facets
  facet_grid(cols = vars(target), rows = vars(model),
             scales = "free") +
  # labels
  labs(x = NULL, y = "inc per 100k") +
  scale_x_date(breaks = "1 month", date_labels = "%b '%y") +
  # theme
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("forecast-over-time.jpg", height = 12, width = 4)


# ideas -------------------------------------------------------------------
# plot sample trajectories in the background
#
# explore weights over time:
#    at what point do weights "settle" into the true trajectory
# use only x weeks past data for weights:
#    if the idea is to eliminate trajectories over time
# central predictions: since scenarios intended for uncertainty
#    take the central 75% interval of scenario projections and treat it as the 99% interval for a forecast (eliminate >75%, relabel quantiles)
# [trajectories: scoring for characteristics ie peak timing]

