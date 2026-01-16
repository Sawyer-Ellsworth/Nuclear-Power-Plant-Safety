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
