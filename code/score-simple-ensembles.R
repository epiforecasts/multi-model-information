# Scores between ensembles

# Format forecasts and observations for scoring
obs_results <- results |>
  group_by(target_end_date, target) |>
  distinct(obs_100k)

forecasts <- ensembles |>
  ungroup() |>
  select(-c(round, target_variable, location)) |>
  mutate(quantile = as.numeric(gsub(pattern = "q", replacement = "",
                                    x = quantile)),
         forecast_date = as.Date("2022-07-30")-7) |>
  left_join(obs_results, by = c("target_end_date", "target")) |>
  rename(prediction = value,
         true_value = obs_100k) |>
  # remove where no data (after March 2023)
  filter(target_end_date <= as.Date("2023-03-04") &
           !is.na(model))

# Score forecasts on log scale
scores <- forecasts |>
  mutate(
    scale = "log",
    true_value = log(true_value + 1e-05), #1/100000
    prediction = log(pmax(prediction, 0) + 1e-05)) |>
  score(metrics = c("interval_score")) |>
  summarise_scores(by = c("model", "scenario_id",
                          "target"),
                   na.rm = TRUE)

# Score all forecasts relative to each other (pairwise) -----
score_pairwise_raw <- pairwise_comparison(scores = scores,
                                          metric = "interval_score",
                                          baseline = "Trajectories",
                                          by = c("target"))

score_pairwise <- score_pairwise_raw |>
  filter(compare_against == "Trajectories" &
           model != "Trajectories") |>
  select(model, target,
         rel_wis = scaled_rel_skill)

scores_boxplot <- score_pairwise |>
  ggplot(aes(x = target, col = model)) +
  geom_point(aes(y = rel_wis)) +
  geom_linerange(aes(ymin = 1, ymax = rel_wis)) +
  geom_hline(yintercept = 1, lty = 2) +
  labs(x = NULL, y = "Relative WIS", col = "Ensemble",
       title = "SI Figure 1. Predictive performance of ensembles",
       caption = "Distribution of ensemble performance scores (relative WIS), of ensembles created by quantile-average or linear opinion pool.\n Performance is compared to an ensemble of quantiles across all trajectories (reference line at 1)") +
  coord_flip() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")

scores_boxplot
