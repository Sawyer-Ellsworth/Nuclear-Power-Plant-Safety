library(tidyverse)
library(readxl)
library(janitor)

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

safe_num <- function(x) as.numeric(gsub(",", "", as.character(x)))

cor_by_month_year <- Results |>
  mutate(
    date = as.Date(date),
    power_generated = safe_num(power_generated),
    overall_safety_score = as.numeric(overall_safety_score)
  ) |>
  group_by(date) |>
  summarize(
    n = sum(complete.cases(power_generated, overall_safety_score)),
    correlation = if (n >= 2) {
      cor(power_generated, overall_safety_score, use = "complete.obs")
    } else {
      NA_real_
    },
    direction = case_when(
      is.na(correlation) ~ NA_character_,
      correlation > 0 ~ "positive",
      correlation < 0 ~ "negative",
      TRUE ~ "zero"
    ),
    .groups = "drop"
  ) |>
  arrange(date)

cor_by_month_year |>
  write_csv("data/processed/Safety_Power_Correlation_By_Month_Year.csv")
