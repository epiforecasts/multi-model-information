# Plot weighted ensemble of samples over time
source(here("code", "score-ensembles-time.R"))

# set up data for plotting
obs_data <- results |>
  distinct(location, target_variable, target_end_date, obs_100k)

ensemble_plot <- ensemble_all |>
  pivot_wider(names_from = quantile) |>
  mutate(median = q0.5) |>
  select(-q0.5) |>
  full_join(obs_data, by = c("location", "target_variable", "target_end_date")) |>
  mutate(forecast_date = as.Date(forecast_date),
         target = factor(paste(location, target_variable)),
         scenario_id = factor(scenario_id),
         wk_horizon = as.numeric((target_end_date - forecast_date) / 7))

# Full grid as figure 1, quantiles-samples-weighted, by scenario -------------
ensemble_plot |>
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

# ggsave("output/forecast-over-time.jpg", height = 10, width = 10)

# Show consecutive 4 v 8 v 16 week forecasts ------------------------------
ensemble_consec <- ensemble_plot |>
  filter(scenario_id == "Weighted"
         & wk_horizon %in% c(4, 8, 16)
  )

## want to show full range of observations on plot
##   def a better way to get a repeated grid of target/wk_horizon/obs
# obs_consec <- obs_data |>
#   mutate(scenario_id = "Weighted",
#          wk_horizon = 4,
#          target = factor(paste(location, target_variable)))
# obs_consec8 <- obs_consec |> mutate(wk_horizon = 8)
# obs_consec16 <- obs_consec |> mutate(wk_horizon = 16)
# obs_consec <- bind_rows(obs_consec, obs_consec8, obs_consec16)
## this still doesn't work :/

# ----- Plot
ensemble_consec |>
  # bind_rows(ensemble_consec, obs_consec) |>
  mutate(wk_horizon = factor(wk_horizon)) |>
  ggplot(aes(x = target_end_date,
             group = wk_horizon,
             col = wk_horizon,
             fill = wk_horizon
  )) +
  # ----- Geoms
  # ensembles
  geom_ribbon(aes(ymin = q0.25, ymax = q0.75),
              col = NA, alpha = 0.4) +
  geom_line(aes(y = median), lwd = 1) +
  # observed data as points
  geom_point(aes(y = obs_100k),
             colour = "black", size = 0.6,
             show.legend = FALSE) +
  # show start date of weighted forecasting
  geom_vline(xintercept = min(results$target_end_date) + weeks(4),
             lty = 2) +
  # ----- Structure
  # facets
  facet_grid(rows = vars(target),
             scales = "free") +
  # labels
  labs(x = NULL, y = "inc per 100k",
       col = "Weeks horizon", fill = "Weeks horizon",
       caption =
         "Weighted ensemble forecasts at 4, 8, and 16 weeks ahead,
       showing median & 50% quantiles.

      Raw projections started from 30 July 2022. Weights are based
      on at least 4 weeks training data (to 20 August 2022, vertical
      dashed line), giving the first 4 week ahead forecast on
      the 24 September 2022 (earliest ribbon). Consecutive forecasts
      are created from updating weights with each week's observed data.") +
  # scales
  scale_x_date(limits = c(min(results$target_end_date), NA),
               breaks = "1 month", date_labels = "%b '%y") +
  scale_color_brewer(type = "qual", direction = -1,
                     palette = 1,
                     aesthetics = c("fill", "colour")) +
  # theme
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("output/forecast-4-8-16wk.jpg", height = 15, width = 10)

