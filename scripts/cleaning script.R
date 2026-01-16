library(tidyverse)
library(readxl)
library(janitor)

#2007 to 2009
fire_inspection <- read_xls(
  "data/raw/fire-inspection-findings.xls",
  skip = 1
) |>
  clean_names() |>
  write_csv(file = "data/processed/fire_clean.csv")

##2007 to 2011
materials_licensing <- read_xls(
  "data/raw/materials-licensing-apps.xls"
) |>
  clean_names() |>
  mutate(received = excel_numeric_to_date(received)) |>
  write_csv(file = "data/processed/mat_licens_clean.csv")

#power out put from 2007,2008,2009
nuke_07 <- read_xlsx(
  "data/raw/nuke_07.xlsx"
) |>
  clean_names() |>
  select(!x3)

nuke_07_fin <- as.data.frame(t(nuke_07))
colnames(nuke_07_fin) <- nuke_07_fin[1, ]
nuke_07_fin <- nuke_07_fin[-1, ]
nuke_07_fin <- nuke_07_fin |>
  rownames_to_column(var = "month_variable") |>
  mutate(month_variable = paste0(month_variable, "_2007"))
nuke_07_fin |> write_csv("data/processed/nuke_07.csv")

nuke_08 <- read_xlsx(
  "data/raw/nuke_08.xlsx"
) |>
  clean_names() |>
  select(!x3)

nuke_08_fin <- as.data.frame(t(nuke_08))
colnames(nuke_08_fin) <- nuke_08_fin[1, ]
nuke_08_fin <- nuke_08_fin[-1, ]
nuke_08_fin <- nuke_08_fin |>
  rownames_to_column(var = "month_variable") |>
  mutate(month_variable = paste0(month_variable, "_2008"))
nuke_08_fin |> write_csv("data/processed/nuke_08.csv")

nuke_09 <- read_xlsx(
  "data/raw/nuke_09.xlsx"
) |>
  clean_names() |>
  select(!x3)

nuke_09_fin <- as.data.frame(t(nuke_09))
colnames(nuke_09_fin) <- nuke_09_fin[1, ]
nuke_09_fin <- nuke_09_fin[-1, ]
nuke_09_fin <- nuke_09_fin |>
  rownames_to_column(var = "month_variable") |>
  mutate(month_variable = paste0(month_variable, "_2009"))
nuke_09_fin |> write_csv("data/processed/nuke_09.csv")

#master power output

nuke_master <- bind_rows(nuke_07_fin, nuke_08_fin, nuke_09_fin)
nuke_master |>
  write_csv("data/processed/nuke_master.csv")

#unused data

# only 2023
#inspection_reports <- read_xlsx(
#  "data/raw/list-of-insp-rpts-lastyear-dataset.xlsx"
#) |>
#  clean_names() |>
#  mutate(doc_dt = mdy(doc_dt))

##2005 to 20111
#waste_incidental <- read_xls(
#  "data/raw/wir-documents.xls",
#  skip = 1
#) |>
#  select(!...6) |>
#  clean_names() |>
#  mutate(date = excel_numeric_to_date(date))
