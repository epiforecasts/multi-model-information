# plot weights of samples over time

source(here("code", "score-ensembles-time.R"))

# heatmap
weights |>
  mutate(target = paste(location, target_variable),
         sample_scenario = paste0(model, sample, scenario_id)) |>
  # order by highest total weight over time
  mutate(sample_scenario = forcats::fct_reorder(sample_scenario,
                                                weight, sum),
         #weight = cut_number(weight, 5)
  ) |>
  ggplot(aes(x = forecast_date, y = sample_scenario,
             fill = weight)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_wrap(~target, ncol = 5, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom")

# ggsave("output/weights-time-heat.jpg", width = 20, height = 12)
