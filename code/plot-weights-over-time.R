# plot weights of samples over time

source(here("code", "score-ensembles-time.R"))

# ----- Weights -----
# column fill
weights |>
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

# ggsave("output/weights-time-sum.jpg", width = 6, height = 12)

# heatmap
weights |>
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
