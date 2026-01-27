library(tidyverse)
library(readxl)
library(janitor)
library(googlesheets4)

#power out put from 2007,2008,2009
nuke_07 <- read_xlsx("data/raw/nuke_07.xlsx") |>
  clean_names() |>
  select(!x3) |>
  pivot_longer(
    cols = -c(state_of_location, net_capacity),
    names_to = "date",
    values_to = "power_generated"
  )

nuke_08 <- read_xlsx("data/raw/nuke_08.xlsx") |>
  clean_names() |>
  select(!x3) |>
  pivot_longer(
    cols = -c(state_of_location, net_capacity),
    names_to = "date",
    values_to = "power_generated"
  )

nuke_09 <- read_xlsx("data/raw/nuke_09.xlsx") |>
  clean_names() |>
  select(!x3) |>
  pivot_longer(
    cols = -c(state_of_location, net_capacity),
    names_to = "date",
    values_to = "power_generated"
  )

nuke_master <- bind_rows(
  nuke_07 |> mutate(date = paste0(date, "_2007")),
  nuke_08 |> mutate(date = paste0(date, "_2008")),
  nuke_09 |> mutate(date = paste0(date, "_2009"))
) |>
  mutate(date = my(date)) |>
  arrange(state_of_location) |>
  filter(!state_of_location %in% toupper(state.name)) |>
  mutate(state_of_location = str_remove(state_of_location, " One")) |>
  write_csv("data/processed/nuke_master.csv")

#Performance Indicator Google Sheet
pi1 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1Rhy4jn8CCNvngwP8cGZ5I-aD06hOhZex-zHYYf3Jsmc/edit?usp=sharing",
  na = "Not in source"
) |>
  clean_names()

pi2 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/14WOAJtgvF2DW6uzseJR71B1M4pbrvbo8DNy38ptczUU/edit?usp=sharing",
  na = "Not in source"
) |>
  clean_names()

pi3 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1VHXXQ95ZITMHrQA98hko2bLJVAP7GPncidLEVVpALig/edit?usp=sharing",
  na = "Not in source"
) |>
  clean_names()

pi4 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1AXOhRuurskFlALTSA8fSKqJMR1ul8SHlUk_xJCln2Tk/edit?usp=sharing",
  na = "Not in source"
) |>
  clean_names()

pi_master <- bind_rows(pi1, pi2, pi3, pi4) |>
  arrange(reactor_name) |>
  mutate(date = my(date)) |>
  mutate(
    reactor_name = reactor_name |>
      str_replace(
        "Columbia Generating Station",
        "Columbia Generating Sta\\.$"
      ) |>
      str_replace("La Salle", "LaSalle Country") |>
      str_replace("South Texas", "South Texas Project") |>
      str_remove(" Reactor") |>
      str_trim()
  ) |>
  write_csv("data/processed/pi_master.csv")

Nuclear_Metrics <- inner_join(
  nuke_master,
  pi_master,
  by = c("state_of_location" = "reactor_name", "date" = "date")
) |>
  select(
    !c(
      mitigating_systems_performance_index_emergency_ac_power_system,
      source,
      net_capacity
    )
  ) |>
  write_csv("data/processed/nuclear_metrics.csv")
