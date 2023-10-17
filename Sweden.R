library(tidyverse)
library(here)
library(fs)
library(sf)

skills <- readxl::read_xlsx(path = "data/Skill_ONET.xlsx", sheet = 1) |>
  janitor::clean_names()
skills |> skimr::skim() # 873 unique occupations

# historical 2014-18 employment at regional (county N=21) level with 4-digit occupations
swe <- readxl::read_xlsx(
  path = "data/Sweden/SWE_Occu_region_2014_2018.xlsx",
  sheet = 6, skip = 2, col_types = c(rep("text",3), rep("numeric", 5))) |>
  janitor::clean_names()

# 4 digit municipality, but only 3-digit occupations
swe2 <- readxl::read_xlsx(
  path = "data/Sweden/Swe_occu_3digi_municipality.xlsx",
  sheet = 1, skip = 2, col_types = c(rep("text",5), rep("numeric", 1))) |>
  janitor::clean_names() |> select(-x3) |>
  rename(ppl_2009 = x2019) |>
  fill(municipality, .direction = "down") |>
  filter(!is.na(ocupation))

swe2 |> skimr::skim() # 291 municipalities, 149 occupations, only one year

# Data downloaded manually from https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__AM__AM0208__AM0208M/YREG60N/table/tableViewLayout1/
# The first file is a json-stats format, the second is just json. For some reason they
# are difficult (json-stat) to parse from normal libraries in R.
#swe3 <- jsonify::from_json(json = "data/Sweden/000005G2_20231013-104715.json") # works
#swe4 <- jsonlite::fromJSON(json = "data/Sweden/000005G2_20231013-111437.json") # doesn't work

## three digit match between Swedish classification and US ONET
swe_match <- readxl::read_xlsx(path = "data/Sweden/Swe_occu_ONETSOC.xlsx")|>
  janitor::clean_names() |>
  rename(swe_occ = occupation, en_occ = sw_occ)

sum(swe_match$onet_soc %in% unique(skills$title)) # 143 occupations matched out of 149
swe_match$onet_soc[!(swe_match$onet_soc %in% unique(skills$title))] # seems to be problems of spelling

# correct the problematic cases on Carla's manual match:
swe_match <- swe_match |>
  mutate(onet_soc = case_when(
    onet_soc == "Administrative Services and Facilities Managers" ~ "Administrative Services Managers",
    onet_soc == "Chief executives" ~ "Chief Executives",
    onet_soc == "Industrial production managers" ~ "Industrial Production Managers",
    onet_soc == "Legislators" ~ "Judicial Law Clerks" ,
    onet_soc == "No information" ~ NA,
    onet_soc == "Public Relations and Fundraising Managers" ~ "Public Relations Specialists",
    TRUE ~ onet_soc
  ))

swe_match

## there are several Swedish occupations that get different English translations but
## the same onet_soc classification. Thus one need to add them up.
# swe2 |>
# rename(occupation = ocupation) |>
#   left_join(swe_match |> rename(occupation = swe_occ)) |>
#   dplyr::group_by(municipality, onet_soc) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)
swe2 <- swe2 |>
  rename(occupation = ocupation) |>
  left_join(swe_match |> rename(occupation = swe_occ)) |>
  select(-occupation, -swedish_occupation_english, -soc_classification, -en_occ) |>
  group_by(municipality, onet_soc) |>
  summarize(jobs = sum(ppl_2009)) |>
  ungroup() |>
  group_by(municipality) |>
  ## Do we need to rescale to proportion of the popoulation per place?
  mutate(prop_jobs = jobs / sum(jobs)) |>
  arrange(municipality) |> mutate(municipality = as_factor(municipality)) |>
  arrange(onet_soc) |> mutate(onet_soc = as_factor(onet_soc))
  #ggplot(aes(municipality, onet_soc)) + geom_tile(aes(fill = prop_jobs)) + theme(axis.text = element_blank())
  #pivot_wider(names_from = onet_soc, values_from = ppl_2009, values_fn = sum)



# Now swe2 has the info of the matrix with proportion of population per place who works in certain jobs.

# this is the data for the matrix of occupations and skills (by importance)
skills |>
  select(occupation = title, element_name, scale_name, data_value) |>
  filter(scale_name == "Importance") |> select(-scale_name) |>
  filter(occupation %in% unique(swe2$onet_soc)) |>
  ggplot(aes(element_name, occupation)) +
  geom_tile(aes(fill = data_value)) +
  scale_fill_viridis_c() + theme(axis.text = element_blank())






## Can we use 4 digit classification? - possible but only one year of data
skills$title |> unique() # 873 occupations
swe$occupation |> unique() # 430 occupations
# only 12 match... I need >400 matchs
sum((swe$occupation |> unique() |> str_remove(pattern = "\\d{4} ")) %in% (skills$title |> unique()) )

swe_codes <- swe |>
  select(occupation) |>
  unique() |>
  mutate(code = str_sub(occupation, start = 1L, end = 4L)) |>
  mutate(occupation = str_sub(occupation, start = 6L, end = -1L))

skills_codes <- skills |>
  select(code = o_net_soc_code, occupation = title) |>
  unique()

## the codes don't match so one needs manual matching.

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

swe |> pull(name) |> unique() # 21 regions

# one could use gender...
swe |> skimr::skim()

# aggregate by gender and try to combine with Onet

swe |>
  group_by(year, occupation, county, name) |>
  summarize(people_total = sum(people))

unique(swe$occupation)



#### Leftovers ####
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

