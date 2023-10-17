library(tidyverse)
library(fs)
library(here)
library(tictoc)


fls <- dir_ls("~/Documents/Projects/DATA/ONET/db_28_0_excel/")
## this one selects the files I manually selected with the yellow flag :)
fls <- system("tag -f 'Yellow' /Users/juanrocha/Documents/Projects/DATA/ONET/db_28_0_excel/", intern = TRUE) |>
  try()

tic()
onet <- fls |>
  map(readxl::read_xlsx, n_max = 100) |>
  map(janitor::clean_names)
toc() # 10s

key_vars <- c("title", "scale_name", "data_value")

cats <- onet |> map(names) |>
  map(.f = function(x) str_detect(x, "category")) |>
  map(.f = function(x) any(x)) |>
  unlist() |>
  which() # files with categories

# example <- readxl::read_xlsx(fls[2]) |> janitor::clean_names()
# example |> print(n=100)
# example |> pull(data_value) |> range()

samestr <- c(1,4,5,7,9) # datasets with the same structure as skills
# files used
fls[samestr]

tic()
onet <- fls[samestr] |>
  map(readxl::read_xlsx) |>
  map(janitor::clean_names)
toc() # 10s

onet <- bind_rows(onet)

onet |> pull(element_id) |> unique() |> length() # 177 features
onet |> pull(title) |> unique() |> length() # 873 professions

onet |>
  select(occupation = title, element_name, scale_name, data_value) |>
  filter(scale_name == "Importance") |> select(-scale_name) |>
  #filter(occupation %in% unique(swe2$onet_soc)) |>
  ggplot(aes(element_name, occupation)) +
  geom_tile(aes(fill = data_value)) +
  scale_fill_viridis_c() + theme(axis.text = element_blank())


save(onet, file = "data/onet.Rda")
