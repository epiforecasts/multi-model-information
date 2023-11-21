library(here)
library(dplyr)
library(scoringutils)
source(here("R", "import-data.R"))

# Get forecasts & observations -----
# Observed data
obs <- import_observed() |>
  select(-c(model, location_name, population)) |>
  rename(true_value = value)
forecasts <- weekly$ensembles |>
  rename(prediction = value) |>
  mutate(quantile = as.numeric(gsub(pattern = "q", replacement = "",
                                    x = quantile))) |>
  left_join(obs, by = c("target_end_date", "location", "target_variable"))

# Score forecasts on natural and log scales -----
scores <- forecasts |>
  mutate(scale = "natural") |>
  # add version for the log transformations
  rbind(forecasts |>
          mutate(
            scale = "log",
            true_value = log(true_value + 1),
            prediction = log(pmax(prediction, 0) + 1)
          )) |>
  score(metrics = c("interval_score")) |>
  summarise_scores(by = c("location",
                          "target_end_date", "forecast_date",
                          "horizon", "scale"),
                   na.rm = TRUE)

write_csv(scores, here("data", "scores-raw.csv"))

# Score all forecasts relative to each other (pairwise) -----
# set variables to group scores within

score_pairwise <- function(scores,
                           score_by = c("model", "scale")) {
  scores_pairwise <- scores |>
    pairwise_comparison(
      metric = "interval_score",
      baseline = "EuroCOVIDhub-ensemble",
      by = score_by)
  scores_pairwise <- scores_pairwise |>
    filter(compare_against == "EuroCOVIDhub-ensemble") |>
    select(model, all_of(score_by),
           rel_wis = scaled_rel_skill)
  return(scores_pairwise)
}


# All time / all location / all horizon
scores_pairwise <- score_pairwise(scores,
                                  score_by = c("model", "scale"))
write_csv(scores_pairwise, here("data", "scores-pw.csv"))
