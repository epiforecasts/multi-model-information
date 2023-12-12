ens_check <- ensembles |>
  group_by(round, location, target_variable, scenario_id,
           quantile, target_end_date) |>
  pivot_wider(names_from = model,
              names_prefix = "value_") |>
  janitor::clean_names() |>
  mutate(diff_abs = value_linear_pool - value_quantiles,
         diff_rel = value_linear_pool / value_quantiles,
         diff_rel = ifelse(is.infinite(diff_rel), NA, diff_rel))

sum(ens_check$value_linear_pool == ens_check$value_quantiles,
    na.rm = TRUE)


data <- ens_check |>
  ungroup() |>
  filter(scenario_id == "A", location == "NL") |>
  select(value_lop, value_quantile)

irr::icc(data, model = "oneway",
         type = "agreement", unit = "single")

# Average differences across time by quantile
ens_check_summary <- ens_check |>
  group_by(target, quantile) |>
  summarise(diff_abs = mean(diff_abs, na.rm = TRUE))




ens_check |>
  ggplot(aes(x = target_end_date)) +
  # ----- Geoms
  # ensembles
  geom_tile(aes(y = scenario_id, fill = diff_abs)) +
  scale_fill_viridis_c() +
  scale_x_date(breaks = "1 year") +
  labs(fill = "Difference between
       LOP and trajectory ensemble",
       x = "Week", y = "Scenario") +
  # ----- Structure
  # facets
  facet_grid(rows = vars(target),
             cols = vars(quantile)) +
  # theme
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(face = "bold",
                                  size = 10),
        axis.text.x = element_blank())

