library(tidyverse)
library(here)
library(fs)
library(sf)

swe <- readxl::read_xlsx(
  path = "data/Sweden/SWE_Occu_region_2014_2018.xlsx",
  sheet = 6, skip = 2, col_types = c(rep("text",3), rep("numeric", 5))) |>
  janitor::clean_names()

swe2 <- readxl::read_xlsx(
  path = "data/Sweden/Swe_occu_3digi_municipality.xlsx",
  sheet = 1, skip = 2, col_types = c(rep("text",5), rep("numeric", 1))) |>
  janitor::clean_names() |> select(-x3) |>
  rename(ppl_2009 = x2019) |>
  fill(municipality, .direction = "down") |>
  filter(!is.na(ocupation))

swe_match <- readxl::read_xlsx(path = "data/Sweden/Swe_occu_ONETSOC.xlsx")|>
  janitor::clean_names() |>
  rename(swe_occ = occupation, en_occ = sw_occ)

## the tail is info from the stats bureau, delete
swe2 |> tail(n = 50) |> print(n = 50)

swe <- swe |>
  rename(county = x1, occupation = x2, gender = x3)  |>
  filter(!is.na(gender)) |>  # this get rid of the last info rows
  fill(occupation, county) |>
  pivot_longer(cols = starts_with("x"), names_to = "year", values_to = "people") |>
  mutate(year = str_remove_all(year, "x")) |>
  mutate(year = as.numeric(year))

swe <- swe |>
  mutate(name = str_remove_all(county, "\\d{2} | county"))

swe |> pull(name) |> unique()


swe |> skimr::skim()

swe_map <- st_read("~/Documents/Projects/DATA/GADM_maps/gadm40_SWE_shp/gadm40_SWE_1.shp")
swe_map$NAME_1 %in% swe$name

## So the labour data is at fist level admin unit:
swe_map$NAME_1[! swe_map$NAME_1 %in% swe$name]
swe$name[! swe$name %in% swe_map$NAME_1] |> unique()
# swe_map |>
#   ggplot() +
#   geom_sf()

## this is already done in swe2
# swe |>
#   filter(name == "Stockholm", year ==2018 ) |>
#   mutate(occupation = str_remove(occupation, "\\d{4} ")) |>
#   group_by(county, occupation, year, name) |>
#   summarize(workers = sum(people)) |>
#   ungroup() |>
#   left_join(swe_match, by = c("occupation" = "en_occ"))

swe2 |>
  filter(str_detect(municipality, "Stockholm"))

