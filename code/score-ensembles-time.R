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
source(here("code", "import-results.R"))
source(here("code", "format-results.R"))
source(here("code", "create-ensembles.R"))
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

# Create ensemble forecasts
ensemble_all <- map_dfr(results_window,
                        ~ create_ensembles(results = .x, truncate_weeks = 0),
                        .id = "forecast_date")

# Process
ensemble_all <- ensemble_all |>
  filter(target_end_date > forecast_date) |>
  mutate(model = paste(model, scenario_id))

# Score

# scratchpad --------------------------------------------------------------
library(ggplot2)

obs_data <- results |>
  distinct(location, target_variable, target_end_date, obs_100k)

# plot weighted ensemble
ensemble_weighted <- ensemble_all |>
  filter(model == "Samples Weighted") |>
  pivot_wider(names_from = quantile) |>
  mutate(median = q0.5) |>
  select(-q0.5) |>
  left_join(obs_data, by = c("location", "target_variable", "target_end_date"))

ensemble_weighted |>
  mutate(forecast_date = as.Date(forecast_date)) |>
  mutate(forecast_date = ordered(forecast_date)) |>
  mutate(target = paste(location, target_variable)) |>
  ggplot(aes(x = target_end_date,
             fill = forecast_date, col = forecast_date
             )) +
  # ----- Geoms
  # ensembles
  # geom_ribbon(aes(ymin = q0.01, ymax = q0.99),
  #             col = NA, alpha = 0.1) +
  geom_ribbon(aes(ymin = q0.25, ymax = q0.75),
              col = NA, alpha = 0.4) +
  geom_line(aes(y = median), lwd = 1) +
  # observed data as points
  geom_point(aes(y = obs_100k),
             colour = "black", size = 0.6, show.legend = FALSE) +
  # ----- Structure
  # facets
  facet_wrap(~ target,
             ncol = 1,
             scales = "free") +
  # labels
  labs(x = NULL, y = "inc per 100k") +
  scale_x_date(breaks = "1 month", date_labels = "%b '%y") +
  # theme
  theme_bw() +
  theme(legend.position = "none",
        axis.line = element_line(linewidth = 0.25),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank())
ggsave("forecast-over-time.jpg", height = 15, width = 12)


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

