# Format raw sample results, by
# - filtering to a minimum of 3 models per target,
# - including observed data points
# - including values per 100k population
library(here)
library(dplyr)
library(lubridate)
# Example:
# source(here("import-results.R"))
# results <- import_results(round = round, local = local)
# results <- format_results()

format_results <- function(results,
                           max_obs_date = as.Date("2023-03-10"),
                           n_model_min = 3,
                           local = FALSE) {

  # Remove targets with <3 models  ------------------
  results <- anti_join(results,
                       results |>
                         group_by(round, location, scenario_id,
                                  target_end_date, target_variable) |>
                         summarise(models = n_distinct(model),
                                   .groups = "drop") |>
                         filter(models < n_model_min),
                       by = c("round", "location", "scenario_id",
                              "target_end_date", "target_variable")) |>
    ungroup()

  # Add observed data ------------------------------------------------------
  if (local) { # load a local copy
    obs <- read_csv(here("data", "obs.csv"))
  } else {
    # get raw JHU data
    cases <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-truth/JHU/truth_JHU-Incident%20Cases.csv") |>
      mutate(target_variable = "inc case")
    deaths <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-truth/JHU/truth_JHU-Incident%20Deaths.csv") |>
      mutate(target_variable = "inc death")
    pop <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-locations/locations_eu.csv") |>
      select(location, population)

    # weekly incidence
    obs <- bind_rows(cases, deaths) |>
      mutate(value = ifelse(value  < 0, NA, value),
             year = epiyear(date),
             week = epiweek(date)) |>
      group_by(location, location_name,
               target_variable,
               year, week) |>
      summarise(target_end_date = max(date),
                value = sum(value, na.rm = TRUE)) |>
      ungroup() |>
      select(-year, -week) |>
      left_join(pop) |>
      mutate(model = "Observed")

    write_csv(obs, here("data", "obs.csv"))
  }

  pop <- distinct(obs,
                  location, population)

  obs <- obs |>
    # filter(target_end_date >= min(results$target_end_date) &
    #        target_end_date <= max_obs_date) |>
    rename(obs = value) |>
    select(-c(model, population))

  results <- left_join(results, obs)

  # Add values per 100k population --------------------------------------
  results <- results |>
    # get population size (from obs dataset)
    left_join(pop) |>
    # take value per 100k
    mutate(value_100k = value / population * 100000,
           obs_100k = obs / population * 100000)

  return(results)
}
