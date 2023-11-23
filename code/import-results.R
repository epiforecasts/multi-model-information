# Get data: observed data and scenario projections
# import_observed()
# import_projections()
#
library(readr)
library(stringr)
library(purrr)
library(here)
library(dplyr)
library(lubridate)
library(arrow)
library(curl)
library(readr)

# Get observed data
import_observed <- function(update = FALSE) {

  file_dir <- here("data")
  dir.create(file_dir, showWarnings = FALSE) ## create if does not exist
  file_path <- file.path(file_dir, "obs.csv")
  local <- file.exists(file_path)
  if (update || !local) { # need to generate local copy
    # get raw JHU data
    cases <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-truth/JHU/truth_JHU-Incident%20Cases.csv") |>
      mutate(target_variable = "inc case")
    deaths <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-truth/JHU/truth_JHU-Incident%20Deaths.csv") |>
      mutate(target_variable = "inc death")
    pop <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-locations/locations_eu.csv") |>
      select(location, population)
    # weekly incidence
    obs <- bind_rows(cases, deaths) |>
      mutate(year = epiyear(date),
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
    write_csv(obs, file_path)
    message("Data updated and saved locally")
  }

  obs <- read_csv(file_path)
  return(obs)
}

# Get and format projections from local repo -----
import_projections <- function(round = 2,
                           update = FALSE,
                           n_model_min = 3) {

  file_dir <- here("data")
  dir.create(file_dir, showWarnings = FALSE) ## create if does not exist
  scenarios_file <- file.path(file_dir, "scenarios.rds")
  data_name <- paste0("round", round, ".parquet")
  data_file <- file.path(file_dir, data_name)
  if (!file.exists(scenarios_file) || update) {
    # get scenario metadata
    source("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-scenario-hub-europe-website/main/code/load/scenarios.R")
    saveRDS(scenarios, scenarios_file)
  }
  if (!file.exists(data_file) || update) {
    # get round path
    url <- paste0("https://github.com/covid19-forecast-hub-europe/covid19-scenario-hub-europe/releases/download/round",
                  round, "/", data_name)

    # Load from parquet storage
    curl::curl_download(url, data_file)
  }
  scenarios <- readRDS(scenarios_file)

  # Formatting results data ------------------------------------------------
  results <- try(arrow::read_parquet(data_file))

  # results loaded from parquet seem to have mixed target end dates
  results <- results |>
    mutate(horizon = as.numeric(substr(horizon, 1, 2)),
           target_end_date = as.Date(scenarios[[paste0("round-", !!round)]][["origin_date"]]) +
             lubridate::weeks(horizon) - lubridate::days(1),
           round = round)

  # Remove targets with <n models
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

  # Add observed data
  obs <- import_observed(update = update)
  results <- left_join(results,
                       obs |>
                         select(obs = value,
                                location, target_variable, target_end_date),
                       by = c("location", "target_variable", "target_end_date"))

  # Add values per 100k population
  results <- results |>
    # get population size (from obs dataset)
    left_join(obs |>
                group_by(location, location_name, target_variable) |>
                filter(target_end_date == min(target_end_date)) |>
                select(location, target_variable,
                       location_name, population),
              by = c("location", "target_variable")) |>
    # take value per 100k
    mutate(value_100k = value / population * 100000,
           obs_100k = obs / population * 100000)

  return(results)
}
