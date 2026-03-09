library(tidyverse)
library(here)
library(fs)
library(sf)
library(tictoc)
library(EconGeo)
library(ggnetwork)
library(patchwork)
library(tictoc)

#### Read data ####\
# load onet
# load("data/onet.Rda") # 34MB
# # Canadian classification translated to ONET
# canada_onet <- readxl::read_xlsx(path = "data/Canada/Canada  ONET.xlsx") |>
#   janitor::clean_names()

## Canadian own calssification, no need to use our ONET translation
canada_noc <- read_csv("data/Canada/noc_2021_version_1.0_-_classification_structure.csv") |>
  janitor::clean_names()

## New high res classification: 2021
tic()
dat <- read_csv("data/Canada/98100594-eng/98100594.csv") |>
  janitor::clean_names()
toc() # 50s
# N = 55M obs, treat with care, over 15Gb on RAM
names(dat)
head(dat)
# geo: is geography. It contains country (Canada), ~10 Provinces, and admin-2. All = 174 units
dat |> pull(geo) |> unique() |>
  str_subset(pattern = "\\(", negate = T) |>
  str_subset(pattern = "Canada", negate = TRUE)

dat$gender_3 |> unique() # keep only "Total - Gender"
dat$age_5a |> unique() # keep only "Total - Age"
dat$industry_sectors_north_american_industry_classification_system_naics_2017_23a |>
  unique() # keep only "Total - Industry - Sectors - North American Industry Classification System (NAICS) 2017"
dat$statistics_3 |> unique() # keep "Count"
dat$coordinate |> unique() # remove
dat$symbol_11 |> unique() # from meta data, NA, ... means too unreliable
dat$occupation_minor_group_national_occupational_classification_noc_2021_309a |>
  unique() |> str_subset(pattern = "\\d{4}") |> #162 professions 4digit depth
  str_subset(pattern = "2021", negate = TRUE) # remove year 2021

tic() # free up memory
dat <- dat |>
  ## Remove national stat and admin 2, keep only admin 1:
  filter(str_detect(geo, pattern = "\\(", negate = FALSE)) |> # if FALSE, working at admin 2 level
  filter(str_detect(geo, pattern = "Canada", negate = TRUE)) |>
  filter(gender_3 == "Total - Gender",
         age_5a == "Total - Age",
         industry_sectors_north_american_industry_classification_system_naics_2017_23a ==
           "Total - Industry - Sectors - North American Industry Classification System (NAICS) 2017",
         statistics_3 == "Count") |>
  filter(str_detect(occupation_minor_group_national_occupational_classification_noc_2021_309a,
                    pattern = "\\d{4}"),
         str_detect(occupation_minor_group_national_occupational_classification_noc_2021_309a,
                    pattern = "2021", negate = TRUE))
toc() # 6s in admin 1 | 27s in admin 2
gc()

lobstr::obj_size(dat)

## extract geographic ids before creating matrix
geo <- dat |>
  select(geo, dguid) |>
  unique()

dat <- dat |>
  select(geo, noc = occupation_minor_group_national_occupational_classification_noc_2021_309a,
         ppl =  labour_force_status_3_total_labour_force_status_1) |>
  separate(noc, into = c("noc_code", "noc_title"), sep = 5) |>
  mutate(noc_code = str_trim(noc_code, "both")) |>
  select(-noc_title) |>
  arrange(geo) |>
  mutate(ppl = log1p(ppl)) |>
  pivot_wider(names_from = geo, values_from = ppl, values_fill = 0)

job_mat <- dat |>
  select(-noc_code) |>
  as.matrix()

## rows are noc_codes in ascending order, 4 digits.

dim(job_mat) # 162 census regions, 162 professions left
rownames(job_mat) <- dat$noc_code
colnames(job_mat)

## Skills from Canadian classification. It has 7d depth, so how do I aggregate?
can_skills <- read_csv("data/Canada/OaSIS/skills2025v1.csv")
# one option is assume the basic level e.g. 0001 would be 00010,00, which in both are legislators
can_skills <- can_skills |>
  rename(code = 1, title = 2) |>
  mutate(noc4 = str_sub(code, 1L, 4L)) |>
  filter(noc4 %in% dat$noc_code) |>  # All noc_codes are in the skills set
  select(-code, -title) |>
  group_by(noc4) |>
  summarize(across(where(is.double), mean))

can_skills |> pivot_longer(2:last_col(), names_to = "skill", values_to = "value") |>
  #pull(value) |> is.na() |> any()
  ggplot(aes(noc4, skill)) +
  geom_tile(aes(fill = value)) +
  scale_fill_viridis_c()

# order of noc4 codes (jobs) is the same as previous matrix (ordered), skills
# enter in the same order as original data.
can_mat <- can_skills |>
  select(-noc4) |>
  as.matrix()

rownames(can_mat) <- dat$noc_code

#### Skill space ####
## J250703: Update, using EconGeo package for simplicity
rca01 <- location_quotient(can_mat, binary = TRUE)

## Effective use = phi or theta aka proximity: can be calculated with
phi <- herfindahl(can_mat)

## Relatedness: can be calculated with other methods of distance, but currently produces
## the probability of changing jobs.
M_skills <- relatedness(t(rca01) %*% rca01, method = "prob")
M_jobs <- relatedness(rca01 %*% t(rca01), method = "prob")

e_jobs <- svd(M_jobs)
e_skills <- svd(M_skills)

df_jobs <- tibble(
  jobs = rownames(can_mat),
  eci_mor = mort(t(can_mat)),
  herfindahl = herfindahl(can_mat), krugman = krugman_index(can_mat),
  diversity = EconGeo::diversity(rca01)
) |> arrange(desc(eci_mor)) |>
  left_join(
    tibble(
      jobs = colnames(M_jobs)[order(colSums(M_jobs))],
      eci_svd = ( e_jobs$d - mean(e_jobs$d)) / sd(e_jobs$d)
    )
  )

ggplot(df_jobs) +
  geom_point(aes(eci_mor, eci_svd)) +
  scale_y_continuous(trans = "log1p")


#### Occupations space ####
## RCA ##
job_mat <- t(job_mat)
rca <- location_quotient(job_mat, binary = TRUE)

## Effective use = phi or theta aka proximity
phi <- herfindahl(rca)

## Relatedness:
# w <- rca01 %*% phi / colSums(phi)
M_jobs2 <- relatedness(t(rca) %*% rca, method = "prob")# jobs given their geography, not skills
M_towns <-  relatedness(rca %*% t(rca), method = "prob")

## SVD: singular value decomposition method
d_jobs <- svd(M_jobs2)
d_towns <- svd(M_towns)



## transition probs for jobs
spectralGP::image_plot(
  t(M_jobs2),
  nlevel = 10, legend.width = 0.025, col = hcl.colors(10, "YlOrRd", rev = TRUE))

df_jobs2 <- tibble(
  jobs = colnames(M_jobs2),
  eci_mor = mort((rca)),
) |> arrange(desc(eci_mor)) |>
  left_join(
    tibble(
      jobs = colnames(M_jobs2)[order(rowSums(M_jobs2), decreasing = TRUE)],
      eci_svd = ( d_jobs$d - mean(d_jobs$d)) / sd(d_jobs$d)
    )
  ) |>
  mutate(rank_mor = order(eci_mor), rank_svd = order(eci_svd))


df_jobs2 |>
  ggplot(aes(rank_mor, rank_svd)) +
  geom_point(aes(color = eci_svd > 0))

cor(df_jobs2$rank_mor, df_jobs2$rank_svd)

## To make places comparable, one need to transform number of people per profession to proportion of population: workers / total population
# dat |>
#   group_by(region) |>
#   mutate(prop_workers = ppl / sum(ppl))
#
# onet

df_towns <- tibble(
  towns = rownames(job_mat) ,
  shannon = entropy(exp((job_mat))), # the matrix was on log units, needs to be exp
  eci_mor = mort(t(rca))
) |> arrange(desc(eci_mor)) |>
  left_join(
    tibble(
      towns = rownames(job_mat) [order(rowSums(M_towns), decreasing = TRUE)],
      eci_svd = (d_towns$d - mean(d_towns$d) / sd(d_towns$d))
    )
  ) |>
  mutate(rank_mor = order(eci_mor), rank_svd = order(eci_svd, decreasing = TRUE))

df_towns |>
  ggplot(aes(rank_svd, rank_mor)) +
  geom_point(aes(color = shannon, alpha = eci_svd > 0)) +
  scale_color_viridis_c()

cor.test(df_towns$eci_mor, df_towns$eci_svd, method = "spearman")

df_towns |>
  ggplot(aes(rank_mor, shannon)) +
  geom_point(aes(color = rank_svd)) +
  geom_text(aes(label = ifelse(shannon< 5.8, towns, NA))) +
  scale_color_viridis_c()

df_towns <- df_towns |>
  left_join(geo, by = c("towns" = "geo"))

## No need of this anymore, matching with DGUID
df_towns <- df_towns |>
  mutate(st = str_locate(towns, "\\(")[,"start"]) |>
  mutate(towns = str_sub(towns, 1L, st-2L)) |>
  # mutate(st = str_locate(towns, "\\-")[,"start"]) |>
  # mutate(towns = case_when(
  #   !is.na(st) ~ str_sub(towns, 1L, st-2L),
  #   .default = towns
  # )) |>
  select(-st)



towns <- df_towns$towns


#### Maps ####
## slow plotting in RStudio: https://forum.posit.co/t/ggplot2-geom-sf-performance/3251/3
## and: https://github.com/tidyverse/ggplot2/issues/2655
#X11.options(type = "cairo")
dev.off()
dev.list()
options(bitmapType = "cairo")

## It is not the best visualization because we have 160 towns with data, but the
## admin 3 has 5581 polygons, so most of them will be filled as NAs. Perhaps keep only
## networks.
# library(rgdal)
# lyrs <- ogrListLayers("~/Documents/Projects/DATA/GADM_maps/gadm41_CAN.gpkg")
can0 <- st_read("~/Documents/Projects/DATA/GADM_maps/gadm41_CAN.gpkg", layer = "ADM_ADM_1")
# can0 <- st_read("data/Canada/lpr_000b21a_e/lpr_000b21a_e.shp")
can <- st_read("data/Canada/lcma000b21a_e/lcma000b21a_e.shp")
can

tic()
can0 |> #st_transform(crs = terra::crs(can)) |>
  ggplot() +
  geom_sf(fill = NULL)
toc() # 266.554 sec elapsed incredibly slow | 3s in external quartz

tic()
can |>
  left_join(df_towns, by = c("DGUID" = "dguid")) |>
  tmap::tm_shape() +
  tmap::tm_polygons(
    fill = "eci_svd",
    fill.scale = tmap::tm_scale_continuous(
      limits = c(10, 60),
      values = "scico.hawaii") )
toc() #95s in normal RStudio

tic()
a <- can |>
  left_join(df_towns, by = c("DGUID" = "dguid")) |>
  ggplot() +
  geom_sf(data =  can0 |> st_transform(crs = terra::crs(can)), fill = NA) +
  geom_sf(aes(fill = shannon), show.legend = TRUE) +
  scale_fill_viridis_c(name = "Shannon diversity") +
  labs(tag = "A") + #scale_x_continuous(n.breaks = 2) +
  theme_light(base_size = 6) +
  theme(legend.title.position = "top",
        legend.position = "bottom",
        legend.key.width = unit(5,"mm"),
        legend.key.height = unit(2, "mm"))
toc() # 23s | 6.7s in external quartz

lobstr::obj_size(can0)
plot(can0)


df_towns

#### Networks ####
library(sigmaNet)
library(igraph)
# Calculationg M_jobs with EconGeo does not produce a transition probability matrix,
# the colSums is not 1. To approximate a prob matrix, divide (normalize) by the colSums
plot(density(M_jobs2 )) #/ colSums(M_jobs2)

# check natural resource related professions with:
canada_noc |>
  filter(code_noc_2021_v1_0 %in% colnames(M_jobs2)) |>
  select(code_noc_2021_v1_0, class_title) |> print(n=200)

nat_profs <- c(
  "2133" , # Natural resources engineers
  "3220", # Practitioners of natural healing
  "6432" , # Tourism and amusement services occupations
  "6520", # Food support occupations
  "8001" , # Managers in natural resources production and fishing
  "8002" , # Managers in agriculture, horticulture and aquaculture
  "8201", # Supervisors, logging and forestry
  "8203", # Contractors and supervisors, agriculture, horticulture and related operations and services
  "8311" , #Logging machinery operators
  "8312", # Fishing vessel masters and fishermen/women
  "8411", # Logging and forestry workers
  "8412", # Workers in agriculture and fishing occupations
  "8510", # Agriculture, horticulture and harvesting labourers and related occupations
  "8512" # Logging, forestry, landscaping and other related labourers
)

## network given relatedness / proximity in geography
net <- graph_from_adjacency_matrix(
  (M_jobs2 > quantile(M_jobs2,0.90) ), "undirected")


edge_density(net)

net$weight <-  M_jobs2
V(net)$eci_svd <- df_jobs2 |> arrange(jobs) |> pull(eci_svd) > 0
V(net)$st_johns <- rca[127,] |> as.logical() # St. John's New Founland
V(net)$name <- canada_noc |>
  filter(code_noc_2021_v1_0 %in% colnames(M_jobs2)) |>
  select(code_noc_2021_v1_0, class_title) |> pull(class_title)
V(net)$nat_prof <- colnames(M_jobs2) %in% nat_profs
V(net)$prop_towns <- colSums(rca) / nrow(rca)


lyt <- layout_nicely(net)
sig <- sigmaFromIgraph(net, lyt) |>
  addEdgeSize(oneSize = 0.5) |>
  addEdgeColors(oneColor = "#e1e0df") |>
  addNodeColors(colorAttr = "st_johns", colorPal = "Paired") |>
  addNodeSize(sizeVector = ifelse(V(net)$nat_prof, 5, 2))

sig

## network of jobs given the places where they occur
plot.igraph(
  net, layout = lyt, vertex.color = ifelse(V(net)$eci_svd, "#f1a340", alpha("grey50", 0.5)),
  vertex.size =  ifelse(V(net)$st_johns, 5, 3),
  vertex.label = NA, vertex.frame.color = ifelse(V(net)$nat_prof, "red", NA),
  vertex.alpha = 0.5, edge.width = 0.5)


b <- ggplot(net, aes(x = x, y = y, xend = xend, yend = yend), layout = lyt ) +
  geom_edges(color = "grey50", alpha = 0.15, linewidth = 0.25) +
  geom_nodes(
    aes(fill = prop_towns, color = nat_prof),
    shape = 21, show.legend = TRUE, size = 1, alpha = 0.8) +
  #scale_alpha_manual(values = c(0.5,1)) +
  #scale_size_manual("Inari \n ECI > 0", values = c(2,3)) +
  scale_fill_viridis_c("Proportion of towns\nwith RCA > 1", option="E") +
  scale_color_manual("Natural resources\ndependent occupations",values = c("grey50" , "red")) +
  guides(alpha = "none") +
  labs(tag = "B") + coord_fixed() +
  theme_void(base_size = 6) +
  theme(legend.position = "bottom", legend.title.position = "top",
        legend.key.height = unit(2,"mm"),
        legend.key.width = unit(5,'mm'))

b

c <- ggplot(net, aes(x = x, y = y, xend = xend, yend = yend), layout = lyt ) +
  geom_edges(color = "grey50", alpha = 0.15, linewidth = 0.25) +
  geom_nodes(
    aes(fill = st_johns, color = nat_prof),
    shape = 21, show.legend = TRUE, size = 1, alpha = 0.8) +
  #scale_alpha_manual(values = c(0.5,1)) +
  #scale_size_manual("Kiruna \n ECI > 0", values = c(2,3)) +
  #scale_fill_viridis_c("Proportion of towns with RCA > 1") +
  scale_fill_manual("St. John's: RCA > 0", values = c( "grey50", "#f1a340")) +
  scale_color_manual("Natural resources\ndependent occupations",values = c("grey" , "red")) +
  guides(alpha = "none", color = "none") +
  labs(tag = "C") + coord_fixed() +
  theme_void(base_size = 6) +
  theme(legend.position = "bottom", legend.title.position = "top")
c


a+b+c + plot_layout(guides="collect", widths = c(1.2, 1,1)) & theme(legend.position = "bottom")

ggsave(
  filename = "fig3_canada.png", path = "img/",
  plot = (a+b+c+ plot_layout(guides="collect", widths = c(1.2, 1,1)) ), device = "png", bg = "white", dpi = 400,
  width = 7, height = 2.5
)




#### Subregional ####
fls <- dir_ls("data/Canada/Sub_Region_1digits_2016_CA/") |>
  str_subset("csv$")

subnat <- map(fls, read_csv)

subnat <- subnat |> bind_rows()
cats <- subnat$`DIM: Profile of Census Subdivisions (2247)` |> unique()
cats <- cats[1058:1093] # selection of categories that falls within jobs or industries.

subnat <- subnat |> filter(`DIM: Profile of Census Subdivisions (2247)` %in% cats)
subnat |> pull(GEO_NAME) |> unique() #107 municipalities
cats # 10 occupations, 20 industries


#### leftovers ####

## 4digit classification at regional level: old classification from 2016
# fls <- dir_ls("data/Canada/Region_4digits_2016_CA/") |>
#   str_subset("CSV$")
#
# dat <- map(fls, read_csv, skip = 6)
#
# regions <- fls |> str_remove("data/Canada/Region_4digits_2016_CA/") |>
#   str_remove("_4dig_2016_CA.CSV") |> str_remove("_4dig_2016_Ca.CSV")
#
# dat <- map2(dat, regions, function(x,y) {x$region <- y; return(x)}) |>
#   bind_rows()
#
# dat <- dat |> janitor::clean_names() |> #names()
#   rename(occupation = 1, ppl = 2) |>
#   select(occupation, ppl, region) |>
#   filter(str_detect(occupation, "^\\d{4} ")) # |>
# filter strings that starts with 4 digits
# ggplot(aes(occupation, region)) + geom_tile(aes(fill = ppl))
#skimr::skim() # 500 occupations, 10 regions.

## 6 digit classification from 2021: not really 6 digits, aggregation of 2
# dat <- readr::read_csv("data/Canada/14100411-eng/14100411.csv") |>
#   janitor::clean_names()
#
# dat |> skimr::skim()
#
# dat <- dat |>
#   filter(ref_date == 2024, geo != "Canada", str_detect(gender, "Total")) |>
#   select(noc = national_occupational_classification_noc, geo,value)
#
# ## another more recent classification: 10 provinces, 43 industries
# dat <- read_csv("data/Canada/2025-12-CSV/pub1225.csv") |>
#   janitor::clean_names()
# dat
#
# # provides less industries, but longer time coverage, I'll have to download them
# # if needed
# dat |>
#   select(survyear, prov, lfsstat, noc_43) |>
#   filter(lfsstat <=2 ) |> # remove unemployeed people
#   select(-lfsstat) |>
#   group_by(prov, noc_43) |>
#   summarize(n=n())


##
# canada_onet <- canada_onet |>
#   rename(occupation = 1, onet = 2) |>
#   filter(!is.na(onet)) |>
#   left_join(onet |> select(onet = 1, title) |> unique() )

## Jobs matrix per town

# job_mat <- dat |>
#   left_join(canada_onet) |>
#   group_by(title, region) |>
#   summarize(ppl = sum(ppl)) |>
#   mutate(log_jobs = log1p(ppl)) |>
#   #mutate(prop_jobs = log_jobs/ sum(log_jobs)) |>
#   #ggplot(aes(prop_jobs)) + geom_density()
#   select(-ppl) |>
#   arrange(title) |>
#   filter(!is.na(title)) |>
#   pivot_wider(
#     names_from = title, values_from = log_jobs) |>
#   select(-region) |>
#   as.matrix()

## Matrix is in alphabethic order both for skills and jobs
# can_mat <- onet |>
#   filter(scale_id == "IM") |>  # retain only importance scores
#   select(-scale_id, -scale_name, onet = o_net_soc_code, -c(n:domain_source)) |>
#   select(-element_id) |>
#   mutate(data_value = scales::rescale(data_value, to = c(0,1))) |>
#   group_by(onet, title, element_name) |>
#   # for combinations with more than one value we average
#   summarize(value = mean(data_value)) |>
#   ungroup() |>
#   arrange((title)) |>
#   filter(title %in% profs) |>
#   pivot_wider(
#     names_from = element_name, values_from = value, values_fill = 0) |>
#   right_join(canada_onet) |>
#   select(-occupation) |> #names()
#   unique() |>
#   filter(!is.na(`Active Learning`))  |> # remove NAs in all variables
#   ungroup() |> unique()
#
# #can_mat |> ungroup() |> skimr::skim() # No NAs left!
#
# profs2 <- can_mat$title #professions from onet
# all(profs == profs2)
#
# can_mat <- can_mat |> ungroup() |>
#   select(where(is.numeric)) |> # this is the raw matrix for skills
#   as.matrix()
#
# class(can_mat)
# rownames(can_mat) <- profs
# dim(can_mat) # 247 jobs, 176 skills | using ONET professions, not Canadian ones.
# with canadian profs there will be >400 but then results from skill nets and
# occupation spaces wont be comparable.


# towns[!towns %in% can$CMANAME]
# can$CMANAME[!can$CMANAME %in% towns]
#
#
# can$NAME_3 |> str_subset("Port Hope")
# ## Modify towns names to match admin 3 names:
# df_towns <- df_towns |>
#   mutate(towns = case_when(
#     towns == "Ottawa - Gatineau" ~ "Ottawa",
#     towns ==  "St. Catharines - Niagara" ~  "St. Catharines",
#     towns == "Kitchener - Cambridge - Waterloo" ~ "Kitchener",
#     towns == "Abbotsford - Mission" ~ "Abbotsford",
#     towns == "Saguenay" ~ "Petit-Saguenay",
#     towns == "Belleville - Quinte West"  ~ "Belleville",
#     towns == "Lloydminster" ~ "Lloydminster (Part)",
#     towns == "Port Hope" ~ "Port Hope and Hope",
#     .default = towns
#   ))
#
# df_towns$towns[!df_towns$towns %in% can$NAME_3]
#
# can
