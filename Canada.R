library(tidyverse)
library(tictoc)
library(fs)

#### Read data ####
onet <- readxl::read_xlsx(path = "data/Canada/Canada  ONET.xlsx")

## 4digit classification at regional level
fls <- dir_ls("data/Canada/Region_4digits_2016_CA/") |> str_subset("CSV$")
dat <- map(fls, read_csv, skip = 6)

regions <- fls |> str_remove("data/Canada/Region_4digits_2016_CA/") |>
  str_remove("_4dig_2016_CA.CSV") |> str_remove("_4dig_2016_Ca.CSV")

dat <- map2(dat, regions, function(x,y) {x$region <- y; return(x)}) |>
  bind_rows()

dat |> janitor::clean_names() |> #names()
  rename(occupation = 1, ppl = 2) |>
  select(occupation, ppl, region) |>
  filter(str_detect(occupation, "^\\d{4} ")) |>  # filter strings that starts with 4 digits
  # ggplot(aes(occupation, region)) + geom_tile(aes(fill = ppl))
  skimr::skim() # 500 occupations, 10 regions.

## Subregional
fls <- dir_ls("data/Canada/Sub_Region_1digits_2016_CA/") |> str_subset("csv$")
subnat <- map(fls, read_csv)

subnat <- subnat |> bind_rows()
cats <- subnat$`DIM: Profile of Census Subdivisions (2247)` |> unique()
cats <- cats[1058:1093] # selection of categories that falls within jobs or industries.

subnat <- subnat |> filter(`DIM: Profile of Census Subdivisions (2247)` %in% cats)
subnat |> pull(GEO_NAME) |> unique() #107 municipalities
cats # 10 occupations, 20 industries


