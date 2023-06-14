# Compare cumulative outcomes
library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)

source(here("code", "import-results.R"))
obs <- get_obs_data()

get_cumulative <- function(compare_last_year = TRUE) {
  # get cumulative [code from @sbfnk] ----------------------------------------
  ## take final cumulative values
  summary <- data |>
    group_by(location, scenario_label, target_variable, model,
             sample) |>
    arrange(target_end_date) |>
    mutate(cumulative = cumsum(value))
  final <- summary |>
    filter(target_end_date == max(target_end_date)) |>
    ungroup()

  ## take quantiles across samples
  quantile_levels = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)
  summary_quantiles <- final |>
    group_by(location, scenario_label, target_variable, model) |>
    summarise(
      value = round(quantile(cumulative, quantile_levels)),
      name = paste0("q", sub("\\.", "_", quantile_levels)),
      .groups = "drop"
    )

  ## calculate projected values as %
  summary_proj <- summary_quantiles |>
    left_join(distinct(truth,
                       location, population),
              by = "location") |>
    mutate(value_p = value / population,
           target_variable = gsub("^inc", "cum", target_variable))

  #  previous total --------------------------------------------
  summary_truth <- truth |>
    filter(target_end_date <= scenarios[[round_text]][["origin_date"]])

  if (compare_last_year) {
    summary_truth <- summary_truth |>
      filter(target_end_date >
               scenarios[[round_text]][["origin_date"]] - lubridate::weeks(52))
    plot_caption <- paste0("Dotted line at 52 weeks' cumulative total prior to start of projections")
  } else {
    plot_caption <- "Dotted line at all-time cumulative total before projections start"
  }
  summary_truth <- summary_truth |>
    group_by(location, target_variable) |>
    arrange(target_end_date) |>
    mutate(cumulative = cumsum(value),
           current_p = cumulative / population,
           target_variable = gsub("^inc", "cum", target_variable)) |>
    filter(target_end_date == max(target_end_date)) |>
    select(location, target_variable, current_p)
}
