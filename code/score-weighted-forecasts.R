library(here)
library(dplyr)
library(scoringutils)
source(here("code", "import-data.R"))

# Current Rmd

local <- TRUE # FALSE = download data from hub git remote, TRUE = use copy in this repo

# import functions
source(here("code", "import-results.R"))
source(here("code", "create-ensembles.R"))

# Quantiles for vincent & LOP ensembles (standard hub submission format)
quantiles <- c(0.01, 0.025,
               seq(0.05, 0.95, by = 0.05),
               0.975, 0.99)
# targets
target_levels <- c("BE inc case", "NL inc case", "ES inc case", "BE inc death", "NL inc death")
target_labels <- c("Belgium cases", "Netherlands cases", "Spain cases", "Belgium deaths", "Netherlands deaths")
names(target_levels) <- target_labels
names(target_labels) <- target_levels
# Load samples from all models together with observed data
results <- import_projections(round = 2, n_model_min = 3,
                              local = local) |>
  mutate(target = ordered(x = paste(location, target_variable),
                          levels = target_levels,
                          labels = target_labels))

# Create set of weekly ensembles with progressively increasing observed data
weekly <- create_weekly_ensembles(results)
weekly_ensembles <- weekly$ensembles |>
  filter(horizon %in% c(4,8,16)) |>
  mutate(horizon_f = factor(horizon,
                            levels = c(16,8,4),
                            labels = c("16 weeks ago", "8 weeks ago", "4 weeks ago")))

# Add to Rmd --------------------------------------------------------------
# Create simple ensemble of samples across all scenarios -----
ensemble_samples_all <- results |>
  group_by(round, location, target_variable, target_end_date) |>
  reframe(
    n = n(),
    value = quantile(value_100k, quantiles),
    quantile = paste0("q", quantiles),
    model = "Trajectories-all"
  )

# Format forecasts and observations for scoring
forecasts <- weekly_ensembles |>
  mutate(model = horizon_f)


  rename(prediction = value) |>
  mutate(quantile = as.numeric(gsub(pattern = "q", replacement = "",
                                    x = quantile))) |>
  left_join(obs_data |>
              rename(true_value = obs_100k),
            by = c("target_end_date", "target")) |>
  filter(!is.na(true_value)) |>

# Score forecasts on natural and log scales
scores <- forecasts |>
  mutate(
    scale = "log",
    true_value = log(true_value + 0.00001), #1/100000
    prediction = log(pmax(prediction, 0) + 0.00001)) |>
  score(metrics = c("interval_score")) |>
  summarise_scores(by = c("location",
                          "target_end_date", "forecast_date",
                          "model", "scale"),
                   na.rm = TRUE)

write_csv(scores, here("data", "scores-raw.csv"))

# Score all forecasts relative to each other (pairwise) -----
# set variables to group scores within
score_pairwise <- pairwise_comparison(scores = scores,
                                      metric = "interval_score",
                                      baseline = "4 weeks ago",
                                      by = c("model", "scale", "location"))
scores_pairwise <- scores_pairwise |>
  filter(compare_against == "4 weeks ago") |>
  select(model, scale, location, rel_wis = scaled_rel_skill)

write_csv(scores_pairwise, here("data", "scores-pw.csv"))


