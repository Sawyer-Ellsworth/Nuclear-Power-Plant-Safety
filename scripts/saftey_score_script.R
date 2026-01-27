library(tidyverse)
library(readxl)
library(janitor)

materials_licensing <- read_csv("data/processed/mat_licens_clean.csv")
materials_licensing |>
  mutate(just_year = year(received)) |>
  mutate(just_year = as.factor(just_year)) |>
  count(just_year)

fire_inspection <- read_csv("data/processed/fire_clean.csv")
fire_inspection |>
  count(year_first_observed)

fire_inspection |>
  filter(.by = "site")

materials_licensing |>
  count()

power <- read_csv("data/processed/nuke_master.csv")
colnames(power)

Nuclear_Metrics <- read_csv("data/processed/Nuclear_Metrics.csv")

Nuclear_Metrics_fin <- Nuclear_Metrics |>
  mutate(across(
    c(
      drill_exercise_performance,
      ero_drill_participation,
      alert_notification_system
    ),
    ~ as.numeric(gsub("%", "", .)) / 100
  )) |>
  mutate(across(
    c(
      unplanned_scrams_per_7000_critical_hours,
      unplanned_power_changes_per_7000_critical_hrs,
      reactor_coolant_system_activity,
      reactor_coolant_system_leakage
    ),
    as.numeric
  )) |>
  mutate(across(where(is.numeric) & !power_generated, ~ as.vector(scale(.)))) |>
  mutate(
    overall_safety_score = -1 *
      rowMeans(
        # *-1 to make positive score mean safer
        pick(!c(state_of_location, date, power_generated)),
        na.rm = TRUE
      ),
  )

Results <- Nuclear_Metrics_fin |>
  select(state_of_location, date, power_generated, overall_safety_score)

Results |>
  write_csv("data/processed/Nuclear_Metrics_Scores.csv")
