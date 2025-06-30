library(tidyverse)
library(sf)
library(tictoc)
library(patchwork)
library(rgdal)
library(RSQLite)

#### Finland ####
lyrs <- ogrListLayers("data/Finland/Finland map/SuomenHallinnollisetKuntajakopohjaisetAluejaot_2023_10k.gpkg")
# dta <- src_sqlite ("data/Finland/Finland map/SuomenHallinnollisetKuntajakopohjaisetAluejaot_2023_10k.gpkg")
# fin1 <- tbl(dta, "Valtakunta")


fin <- read_sf(
  "data/Finland/Finland map/SuomenHallinnollisetKuntajakopohjaisetAluejaot_2023_10k.gpkg",
  layer = lyrs[4])

ggplot(fin) +
  geom_sf(aes(fill = totalarea))



#### Sweden ####

swe_map <- st_read("~/Documents/Projects/DATA/GADM_maps/gadm40_SWE_shp/gadm40_SWE_2.shp")
swe_map$NAME_2 %in% str_remove(rownames(job_mat), "\\d{4} ")

## So the labour data is at fist level admin unit:
# swe_map$NAME_1[! swe_map$NAME_1 %in% swe$name]
# swe$name[! swe$name %in% swe_map$NAME_1] |> unique()
swe_map |>
  ggplot() +
  geom_sf()

#### Canada ####


#### USA ####



#### MARAT case studies ####
## From 04-map.R in Arctic_QCA repository
coords <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1qX_Eg1wdExa0SyV2sqR-8DfHoD_6KfEbrGqxkkmO-ZA/edit#gid=1573239536",
  sheet = 2)

coords <- coords %>%
  mutate(type = case_when(
    output == 1 ~ "Resilience",
    output == 0.5 ~ "Transformation",
    output == 0 ~ "Loss of resilience"
  ),
  type = as_factor(type))

## case 5 has two coordiantes because is two places, reduce to one.
coords$lon[[5]] <- coords$lon[[5]] |> str_remove(pattern = "(;\\\n.*)") |> as.numeric()
coords$lat[[5]] <- coords$lat[[5]] |> str_remove(pattern = "(;\\\n.*)") |> as.numeric()
coords$lon[[31]] <- coords$lon[[31]] |> str_remove(pattern = "(\\\n.*)") |> as.numeric()
coords$lat[[31]] <- coords$lat[[31]] |> str_remove(pattern = "(\\\n.*)") |> as.numeric()

coords <- coords |> unnest(cols = c(lon, lat))

world <- map_data("world")
arctic_map <- ggplot(world, aes(x = long, y = lat)) +
  geom_path(aes(group = group), size = 0.1, color = "grey50") +
  coord_map(projection = "orthographic", orientation = c(90,0,0)) +
  ylim(45,90) +
  geom_point(
    data = coords,
    aes(x = lon, y = lat, color = type, size = pop_size),
    alpha = 0.5
  ) +
  ggrepel::geom_text_repel(data = coords, aes(lon, lat, label = `Case name`), size = 2) +
  scale_color_manual(
    name = "Case", values = c("#3E97F7", "#8829F3", "#EB4891"),
    guide = guide_legend(title.position = "top")) + # or nice orange "#FF7F00"
  scale_size("Population") +
  theme_void(base_size = 10) +
  theme(legend.position = c(0.2, 0.1), legend.key.size = unit(0.5, "cm"),
        legend.direction = "vertical", legend.box = "horizontal")

arctic_map



#### NUTS3 data for Europe ####
# Not useful, only regions not municipalities for Finland
# nuts <- read_sf("/Users/juanrocha/Documents/Projects/DATA/EuroStats_NUTS/NUTS_RG_20M_2021_3035.shp/NUTS_RG_20M_2021_3035.shp")
#
# nuts |> skimr::skim()
#
# nuts <- nuts |>
#   st_transform(4326) |>
#   mutate(center = st_centroid(geometry)) |>
#   mutate(center = as.character(center)) |>
#   mutate(center = str_remove_all(center, "c|\\(|\\)")) |>
#   separate(center, into = c("lon", "lat"), sep = ", ") |>
#   mutate(across(lon:lat, as.numeric)) |>
#   filter(lat > 60)
#
# plot(st_geometry(nuts))
#
# nut_codes <- nuts |>
#   as.data.frame() |>
#   filter(LEVL_CODE == 3) |>
#   pull(NUTS_ID)
