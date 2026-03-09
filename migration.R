## migration data: Finland

library(tidyverse)
library(tictoc)
library(fs)

#### Load and clean data ####
fls <- dir_ls("data/Finland/migration/") |> str_subset( pattern = "csv")

dat <- map(
  fls |> str_subset("2024", negate = TRUE),
  read_csv, skip = 1, locale=locale(encoding="latin1"))

# there are some files (7:9) with arrivals on the rows, departures on the columns
dat[[10]]

c(1:6, 10)

dat <- map()

dat[c(1:6, 10)]  <- dat[c(1:6, 10)] |> map(function(x){
  x |> filter(`Area of departure` != "WHOLE COUNTRY") |>
    pivot_longer(2:last_col(), names_to = "Area of destination", values_to = "ppl") |>
    mutate(dataset = str_sub(`Area of destination`, 1L, 36L),
           `Area of destination` = str_sub(`Area of destination`, 37L, -1L)) |>
    filter(`Area of destination` != "WHOLE COUNTRY") |>
    mutate(year = str_extract(dataset, "\\d{4}")) |>
    select(-dataset) |>
    rename("departure" = 1, "destination" = 2) |>
    mutate(departure = str_remove(departure, "Departure - "),
           destination = str_remove(destination, "Arrival - "))
})


dat[c(7:9)]  <- dat[c(7:9)] |> map(function(x){
  x |>
  filter(`Area of arrival` != "WHOLE COUNTRY") |>
  pivot_longer(2:last_col(), names_to = "Area of departure", values_to = "ppl") |>
  mutate(dataset = str_sub(`Area of departure`, 1L, 36L),
         `Area of departure` = str_sub(`Area of departure`, 37L, -1L)) |>
  filter(`Area of departure` != "WHOLE COUNTRY") |>
  mutate(year = str_extract(dataset, "\\d{4}")) |>
  select(-dataset) |>
  rename("departure" = 2, "destination" = 1) |>
  mutate(departure = str_remove(departure, "Departure - "),
         destination = str_remove(destination, "Arrival - "))
})

dat

### networks ####
tic()
dat <- map(dat, function(x){
  x |> select(departure, destination, ppl) |>
    filter(departure != destination) |>
    network::network(directed = TRUE, ignore.eval = FALSE)
})
toc()

dat
