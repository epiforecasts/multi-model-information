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
import_observed <- function(local = TRUE) {

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
    write_csv(obs, here("data", "obs.csv"))
  }

  return(obs)
}

# Get and format projections from local repo -----
import_projections <- function(round = 2,
                           local = TRUE,
                           n_model_min = 3) {

  if (local) {
    # if hub is cloned locally, get path to hub/data-processed
    hub_data_path <- gsub("aggregation-info-loss", # this dir
                          "covid19-scenario-hub-europe", # hub dir
                          here::here("data-processed"))

    # get scenarios metadata
    scenarios <- read_rds(here("data", "scenarios.rds"))

    # get csv paths
    model_results <- list.files(hub_data_path,
                                full.names = TRUE, recursive = TRUE)
    model_results <- model_results[grepl(".csv", model_results)]

    # get model date and name
    models <- tibble(file = model_results) |>
      mutate(date = as.Date(str_extract(file,
                                        "2022-[:digit:][:digit:]-[:digit:][:digit:]")),
             model = str_remove(file, hub_data_path),
             model = str_extract(model, "\\/(.+)\\/"),
             model = str_remove_all(model, "\\/"))

    # keep only files within round specific dates
    origin_date <- scenarios[[paste0("round-", round)]][["origin_date"]]
    window_end <- scenarios[[paste0("round-", round)]][["submission_window_end"]]
    valid_dates <- seq.Date(from = as.Date(origin_date),
                            to = as.Date(window_end), by = 1)
    models <- filter(models, date %in% valid_dates)

    # read csv
    safely_read <- safely(~ read_csv(., show_col_types = FALSE))
    results <- map(.x = models$file,
                   ~ safely_read(.x))
    names(results) <- models$model
    results <- transpose(results)
    results <- bind_rows(results$result, .id = "model") |>
      mutate(round = round)

    # add dates
    results <- results |>
      mutate(horizon = as.numeric(str_remove(horizon, " wk")),
             target_end_date = origin_date + lubridate::weeks(horizon) - 1)

    # models:
    #   In round 1, USC submitted 2 sets of results from the same model
    #    ("USC-SIkJalpha"), using only new data ~3 weeks apart ("USC-SIkJalpha_update").
    #    Note the parquet version only includes the first of these.
    #    If loading from local, use only "USC-SIkJalpha"
    if (round == 1) {
      models <- distinct(results, model) |>
        filter(grepl("^USC-SIkJalpha", model))
      if (nrow(models) > 1) {
        results <- filter(results,
                          !grepl("^USC-SIkJalpha_update$", model))
      }
    }

  } else { # Get from remote source -----

    # get scenario metadata
    try(source("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-scenario-hub-europe-website/main/code/load/scenarios.R"))

    # get round path
    data_path <- tempfile(fileext = ".parquet")
    url <- paste0("https://github.com/covid19-forecast-hub-europe/covid19-scenario-hub-europe/releases/download/round",
                  round, "/round", round,
                  ".parquet")

    # Load from parquet storage
    try(curl::curl_download(url, data_path))
    results <- try(arrow::read_parquet(data_path))

    # results loaded from parquet seem to have mixed target end dates
    results <- results |>
      mutate(horizon = as.numeric(substr(horizon, 1, 2)),
             target_end_date = as.Date(scenarios[[paste0("round-", !!round)]][["origin_date"]]) +
               lubridate::weeks(horizon) - lubridate::days(1),
             round = round)
  }

  # Formatting results data ------------------------------------------------

  # Remove targets with <3 models
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
  obs <- import_observed(local = local)
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
