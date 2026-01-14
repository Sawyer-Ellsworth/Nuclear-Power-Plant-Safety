library(tidyverse)
library(readxl)
library(janitor)

## only 2023
inspection_reports <- read_xlsx(
  "Personal Project/data/raw/list-of-insp-rpts-lastyear-dataset.xlsx"
) |>
  clean_names() |>
  mutate(doc_dt = mdy(doc_dt))

##2007 to 2011
materials_licensing <- read_xls(
  "Personal Project/data/raw/materials-licensing-apps.xls"
) |>
  clean_names() |>
  mutate(received = excel_numeric_to_date(received))

##2005 to 20111
waste_incidental <- read_xls(
  "Personal Project/data/raw/wir-documents.xls",
  skip = 1
) |>
  select(!...6) |>
  clean_names() |>
  mutate(date = excel_numeric_to_date(date))

fire_inspection <- read_xls(
  "Personal Project/data/raw/fire-inspection-findings.xls",
  skip = 1
) |>
  clean_names()


inspection_reports |>
  count(SITE_NAME) |> #??? 55 names would it be ok to put the names into ai and have it
  print(n = 55) #??? find the state they are in

inspection_reports |>
  mutate(just_year = year(doc_dt)) |>
  mutate(just_year = as.factor(just_year)) |>
  count(just_year)

materials_licensing |>
  mutate(just_year = year(received)) |>
  mutate(just_year = as.factor(just_year)) |>
  count(just_year)

waste_incidental |>
  mutate(just_year = year(date)) |>
  mutate(just_year = as.factor(just_year)) |>
  count(just_year)

fire_inspection |>
  count(year_first_observed)

materials_licensing |>
  count()
