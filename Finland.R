library(tidyverse)
library(tictoc)
library(EconGeo)
library(patchwork)
library(ggnetwork)


#### Read data ####
# load onet
load("data/onet.Rda") # 34MB

tic()
dat <- readxl::read_xlsx(
  path = "data/Finland/Municipality_4digits_2010_2019_FIN.xlsx",
  skip = 6)
toc() #17s

pop <- read_csv(file = "data/Finland/population_001_11s1_2023_20241108-190513.csv")

skimr::skim(dat)
# years: 2010-19
# 309 municipalities, 459 occupations

codes_en <- dat |>
  select(code_isco4, isco4_name_en) |>
  unique() # 459 professionss
codes_en |> print(n=100) # there are 37 occupations that were not coded at 4-digit depth

df_match <- readxl::read_xlsx("data/Finland/finaland onet.xlsx", sheet = 3) |>
  janitor::clean_names()

df_match <- df_match |> select(-x4) |>
  filter(!is.na(onet)) |>
  rename(code_isco4 = isco_code_4_digits, ) |>
  mutate(code_isco4  = str_remove_all(code_isco4 , pattern = "\'"))

#### Corrections ####
#### to find out useful matches
#### onet_occ |> filter(str_detect(title, "Mining"))
df_match[df_match$occupation == "Real estate agents and property managers", "onet"] <- "11-9141.00"
df_match[df_match$occupation == "Business services and administration managers not elsewhere classified", "onet"] <- "11-1021.00"
df_match[df_match$occupation == "Managing directors and chief executives", "onet"] <- "11-1011.00"
df_match[df_match$occupation == "Senior government officials", "onet"] <- "13-1041.04"
df_match[df_match$occupation == "Legislators", "onet"] <- "23-1011.00" #lawyers
df_match[df_match$occupation == "Sports, recreation and cultural centre managers", "onet"] <- "27-2021.00"
df_match[df_match$occupation == "Physicists and astronomers", "onet"] <- "19-2012.00"
df_match[df_match$occupation == "Mining engineers, metallurgists and related professionals", "onet"] <- "17-2151.00"
df_match[df_match$occupation == "Engineering professionals not elsewhere classified", "onet"] <- "17-3025.00"
df_match[df_match$occupation == "Landscape architects", "onet"] <- "17-1012.00"
df_match[df_match$occupation == "Generalist medical practitioners", "onet"] <- "29-1216.00"
df_match[df_match$occupation == "Specialist medical practitioners", "onet"] <- "19-1042.00"
df_match[df_match$occupation == "Traditional and complementary medicine professionals", "onet"] <- "31-9092.00"
df_match[df_match$occupation == "Paramedical practitioners", "onet"] <- "29-1229.04"
df_match[df_match$occupation == "Dieticians and nutritionists", "onet"] <- "29-1031.00"
df_match[df_match$occupation == "Audiologists and speech therapists", "onet"] <- "29-1181.00"
df_match[df_match$occupation == "Early childhood educators", "onet"] <- "25-2011.00"
df_match[df_match$occupation == "Software developers", "onet"] <- "15-1253.00"
df_match[df_match$occupation == "Web and multimedia developers", "onet"] <- "15-1254.00"
df_match[df_match$occupation == "Software and applications developers and analysts not elsewhere classified", "onet"] <- "15-1251.00"
df_match[df_match$occupation == "Database designers and administrators", "onet"] <- "15-1242.00"
df_match[df_match$occupation == "Computer network professionals", "onet"] <- "15-1241.00"
df_match[df_match$occupation == "Psychologists", "onet"] <- "19-3033.00"
df_match[df_match$occupation == "Social work and counselling professionals", "onet"] <- "19-3034.00"
df_match[df_match$occupation == "Chemical and physical science technicians", "onet"] <- "19-4031.00"
df_match[df_match$occupation == "Medical imaging and therapeutic equipment technicians", "onet"] <- "29-2012.00"
df_match[df_match$occupation == "Valuers and loss assessors", "onet"] <- "13-2023.00"
df_match[df_match$occupation == "Government social benefits officials", "onet"] <- "43-4061.00"
df_match[df_match$occupation == "Information and communications technology operations technicians", "onet"] <- "11-3021.00"
df_match[df_match$occupation == "Information and communications technology user support technicians", "onet"] <- "15-1244.00"
df_match[df_match$occupation == "Computer network and systems technicians", "onet"] <- "15-1231.00"
df_match[df_match$occupation == "Web technicians", "onet"] <- "15-1241.00"
df_match[df_match$occupation == "Medical and pathology laboratory technicians", "onet"] <- "29-2012.00"
df_match[df_match$occupation == "Legislators", "onet"]



onet_occ <- onet |>
  select(onet =o_net_soc_code, title) |>
  unique()

df_match |> left_join(onet_occ) |>
  print(n=500)


#onet_occ |> filter(str_detect(title, "Milk"))

### Preserve onet professions reported in Finland
# onet |>
#   filter(scale_id == "IM") |>  # retain only importance scores
#   select(-scale_id, -scale_name, -c(n:domain_source), onet = o_net_soc_code) |>
#   inner_join(df_match, relationship = "many-to-many") |>
#   select(onet, title, code_isco4, occupation) |>
#   unique() |>
#   # this is to check manually potential mistakes and update corrections above.
#   write_csv(file = "data/Finland/onet_matching.csv")

## Matrix for Finland
dat |>
  select(-isco4_name_sv, -code_name_isco4_en, -code_name_isco4_sv) |>
  mutate(employed_persons = case_when(employed_persons == "…" ~ "0",
                                      TRUE ~ employed_persons)) |>
  mutate(employed_persons = as.numeric(employed_persons)) |>
  filter(!str_detect(code_isco4, "\\d\\.")) |>
  # negating the following results in army workers, but they do not exist on onet skills
  filter(code_isco4 %in% df_match$code_isco4) |>
  select(-municipality_code, -municipality_name_sv) |>
  #pivot_wider(names_from = municipality_name_en, values_from = employed_persons, values_fill = 0) |>
  filter(year == 2019) |>
  ggplot(aes(code_isco4, municipality_name_en)) +
  geom_tile(aes(fill = employed_persons)) +
  scale_fill_viridis_c() +
  theme(axis.text = element_blank())



## Onet reduced for Finland
# Decision point: Do we duplicate rows in onet, or keep as is and reduce rows in
# Finland data? I think row duplication is nicer because it retains the original finish
# classification. It will create jobs with the same complexity, but that happens in any case
# in raw onet if two jobs have the same skill vector. One strong reason for going this pathway is to
# keep the same dimensions between this `fin_onet` data and the Finish data `dat`.
# J250703: Keeping the ONET names in both networks allows to see for example proportion of towns
# where a job has competitive advantage in the country (a feature of both nets).
fin_onet <- onet |>
  filter(scale_id == "IM") |>  # retain only importance scores
  select(-scale_id, -scale_name, onet = o_net_soc_code, -c(n:domain_source)) |>
  #filter(onet %in% df_match$onet) |> # filter reduces to matches
  # we lose occupations the moment we merge different jobs on the finish classification
  # under the same onet codes. It is unavoidable. To avoid that use right_join
  select( -element_id) |> #pull(data_value) |> range()
  mutate(data_value = scales::rescale(data_value, to = c(0,1))) |>
  group_by(onet, title, element_name) |>
  # for combinations with more than one value we average
  summarize(value = mean(data_value)) |>
  pivot_wider(names_from = element_name, values_from = value, values_fill = 0) |>
  # rigth join keeps all rows of df_match, so duplicate vectors for finish occupations
  # with the same onet code.
  right_join(df_match) |>
  select(onet, title, code_isco4, occupation, 4:last_col())

# title is the profession in onet, occupation is the profession in Finlad stats classification
fin_onet <- fin_onet |> ungroup() |>
  select(-occupation, -code_isco4) |> #remove finish classification
  unique() # keep non duplicates from ONET
# 312 professions left

profs <- fin_onet$title

fin_mat <- fin_onet |> ungroup() |>
  filter(title %in% profs2) |>
  select(-c(onet:title)) |>
  as.matrix()
dim(fin_mat) # 300 occupations, 175 skills, bear in mind 293 unique onet occupations

## Missing values? : problem solved!
prblm <- fin_onet |> filter(is.na(`Active Listening`)) #|>
  #select(onet:occupation)

# calculate RCA


#### Skillscape ####
rca01 <- location_quotient(fin_mat, binary = TRUE)

## Effective use = phi or theta aka proximity
#phi <- Herfindahl(fin_mat) # Herfindahl is a proxy of diversity

## Relatedness: can be calculated with other methods of distance, but currently produces
## the probability of changing jobs.
M_skills <- relatedness(t(rca01) %*% rca01, method = "prob")
M_jobs <- relatedness(rca01 %*% t(rca01), method = "prob")

## plot the transition probability of jobs:
spectralGP::image_plot(
  t(M_jobs),
  nlevel = 10, legend.width = 0.025, col = hcl.colors(10, "YlOrRd", rev = TRUE))

## `svd` returns `u` and `v` which are the left and right singular vectors of the matrix
## data x. `d` is the vector of singular values of x sorted decreasingly. I need them
## unsorted to later match jobs or skills (to give them identity). So I recalculated
## below `d` manually following the formulas on the `svd` function help().
## When doing manually I end up with the same ordering. However, Hidalgo clarifes that the
## matrix entering the SVD is organized by diversity (ColSums)
## rearranging the matrix as M_jobs[order(colSums(M_jobs)),order(colSums(M_jobs))]
## does not change the results. So just keep the order to identify key elements.
e_jobs <- svd(M_jobs)
e_skills <- svd(M_skills)

## this is the method in Hidalgo 2021
#eci_jobs2 <-( e_jobs$d - mean(e_jobs$d)) / sd(e_jobs$d)
#eci_skills2 <- (e_skills$d - mean(e_skills$d) / sd(e_skills$d))

## this is the method of reflections in Hidalgo 2007
#MORt(t(fin_mat)) |> sort()

## Both ways achieve qualitatively similar results
df_jobs <- tibble(
  jobs = profs2,
  eci_jobs = mort(t(fin_mat)),
  herfindahl = herfindahl(fin_mat), krugman = krugman_index(fin_mat),
  diversity = EconGeo::diversity(rca01)
) |> arrange(desc(eci_jobs)) |>
  left_join(
    tibble(
      jobs = profs2[order(colSums(M_jobs))],
      eci_job2 = ( e_jobs$d - mean(e_jobs$d)) / sd(e_jobs$d)
    )
  ) |>
  mutate(rank_mor = order(eci_jobs), rank_svd = order(eci_job2))

df_jobs |>
  ggplot(aes(rank_mor, rank_svd)) +
  geom_point(aes(color = eci_job2 > 0))

cor(df_jobs$rank_mor, df_jobs$rank_svd)

#### Occupations space ####
## Matrix for Finland
fin2019 <- dat |>
  filter(year == 2019) |>
  select(-isco4_name_sv, -code_name_isco4_en, -code_name_isco4_sv) |>
  mutate(employed_persons = case_when(employed_persons == "…" ~ "0",
                                      TRUE ~ employed_persons)) |>
  mutate(employed_persons = as.numeric(employed_persons)) |>
  filter(!str_detect(code_isco4, "\\d\\.")) |> #select(isco4_name_en, code_isco4) |> unique() |> print(n=40)
  # negating the following results in army workers, but they do not exist on onet skills
  filter(code_isco4 %in% df_match$code_isco4) |>
  select(-municipality_code, -municipality_name_sv) |>
  #pivot_wider(names_from = municipality_name_en, values_from = employed_persons, values_fill = 0) |>
  left_join(df_match |> left_join(onet_occ)) |>
  group_by(title, municipality_name_en) |>
  # sum people by profession given that some onet profs are duplicates:
  summarize(employed_persons = sum(employed_persons, na.rm = TRUE)) |>
  # remove zero sum rows in the matrix
  filter(sum(employed_persons) > 0) |>
  ungroup() |> group_by(municipality_name_en) |>
  filter(sum(employed_persons) > 0) |>
  # working on log-units
  mutate(employed_persons = log1p(employed_persons)) |>
  ungroup() |>
  pivot_wider(
    names_from = municipality_name_en, values_from = employed_persons, values_fill = 0)

## there is one profession in fin2019 data that has different names in Swedish, different
## code ID but the same profession name in English. I will change the name, be aware if combining with results from skills analysis above
# fin2019 <- fin2019 |>
#   mutate(isco4_name_en = case_when(
#     code_isco4 == "3411" ~ "Judicial assistant",
#     .default = isco4_name_en
#   ))

zero_cols <- fin2019 |> map_lgl(.f = function(x) ifelse(is.character(x), TRUE, sum(x) > 0))
all(zero_cols) # no need to delete municipalities
fin2019 |> rowwise() |>
  mutate(zero_rows = sum(Akaa:Luhanka)>0) |>
  filter(zero_rows == FALSE)
fin2019 |> unique()

#skimr::skim(fin2019)
job_mat <- as.matrix(fin2019 |> select(-1))

dim(job_mat) # 300 jobs, 308 municipalities, 13 jobs and 1 town removed due to zeroes. The municipality removed is Sottunga (309)
# 100 jobs removed when switching to ONET classification
range(job_mat) # no NAs, all in log units

profs2 <- fin2019$title # jobs with non-zero values on the municipalities

## RCA ##
rca <- location_quotient(job_mat, binary = TRUE)

## Effective use = phi or theta aka proximity
#phi <- Herfindahl(rca)

## Relatedness:
# w <- rca01 %*% phi / colSums(phi)
M_towns <- relatedness(t(rca) %*% rca, method = "prob")
M_jobs2 <-  relatedness(rca %*% t(rca), method = "prob") # jobs given their geography, not skills



## SVD: singular value decomposition method
d_jobs <- svd(M_jobs2)
d_towns <- svd(M_towns)

# eci_jobs2 <- (d_jobs$d - mean(d_jobs$d)) / sd(d_jobs$d)
# eci_towns <- (d_towns$d - mean(d_towns$d) / sd(d_towns$d))


## plot the transition probability of jobs:
# spectralGP::image_plot(
#   t(M_jobs2),
#   nlevel = 10, legend.width = 0.025, col = hcl.colors(10, "YlOrRd", rev = TRUE))

df_jobs2 <- tibble(
  jobs = profs2,
  eci_mor = mort(t(rca)),
  herfindahl = herfindahl(rca), krugman = krugman_index(rca)
) |> arrange(desc(eci_mor)) |>
  left_join(
    tibble(
      jobs = profs2[order(rowSums(M_jobs2), decreasing = TRUE)],
      eci_svd = ( d_jobs$d - mean(d_jobs$d)) / sd(d_jobs$d)
    )
  ) |>
  mutate(rank_mor = order(eci_mor), rank_svd = order(eci_svd))

## completely anti-correlated! change order in creating jobs to ease comparison
df_jobs2 |>
  ggplot(aes(rank_mor, rank_svd)) +
  geom_point(aes(color = eci_svd > 0))

cor(df_jobs2$rank_mor, df_jobs2$rank_svd)

df_towns <- tibble(
  towns = colnames(job_mat),
  shannon = entropy(t(exp(job_mat))), # the matrix was on log units, needs to be exp
  eci_mor = mort(rca),
  eci_eig = kci(t(rca)),
  herfindahl = herfindahl(t(rca)), krugman = krugman_index(t(rca)),
  ubiquity = ubiquity(rca), diversity = EconGeo::diversity(t(rca))
) |> arrange(desc(eci_mor)) |>
  left_join(
    tibble(
      towns = colnames(job_mat)[order(rowSums(M_towns), decreasing = TRUE)],
      eci_svd = (d_towns$d - mean(d_towns$d) / sd(d_towns$d))
    )
  ) |>
  mutate(rank_mor = order(eci_mor), rank_svd = order(eci_svd), rank_eig = order(eci_eig))

a <- df_towns |>
  ggplot(aes(rank_svd, rank_mor)) +
  geom_point(aes(color = shannon, alpha = eci_svd > 0), size = 1) +
  scale_color_viridis_c("Shannon diversity") +
  scale_alpha_manual("ECI > 0", values = c(0.2,0.8)) +
  labs(title = "Economic complexity", x = "Rank with singular value decomposition",
       y = "Rank with method of reflections", tag = "A") +
  coord_fixed() +
  theme_light(base_size = 6) +
  theme(legend.position.inside = c(0.3, 0.8), legend.position = "inside",
        legend.key.width = unit(5,"mm"), legend.key.height = unit(2,"mm"),
        legend.direction = "horizontal", legend.title.position = "top",
        legend.key.spacing.x = unit(0.5,'mm'), legend.key.spacing.y = unit(0.5,'mm'),
        legend.box.background = element_blank())
a

#### Networks ####
#library(network)
library(sigmaNet)
library(igraph)
# Calculationg M_jobs with EconGeo does not produce a transition probability matrix,
# the colSums is not 1. To approximate a prob matrix, divide (normalize) by the colSums
plot(density(M_jobs2 )) #/ colSums(M_jobs2)
# old list from the finish classification
#nat_profs <- c("Wood treaters","Travel guides","Traditional and complementary medicine associate professionals", "Textile, fur and leather products machine operators not elsewhere classified", "Sewing, embroidery and related workers","Livestock and dairy producers", "Inland and coastal waters fishery workers", "Hunters and trappers", "Handicraft workers in wood, basketry and related materials" ,"Handicraft workers in textile, leather and related materials", "Glass makers, cutters, grinders and finishers", "Gardeners, horticultural and nursery growers", "Garden and horticultural labourers" ,"Fumigators and other pest and weed controllers" , "Fruit, vegetable and related preservers" ,"Forestry labourers","Forestry and related workers" ,"Field crop and vegetable growers" ,  "Farming, forestry and fisheries advisers" ,"Dairy-products makers", "Crop farm labourers" ,"Butchers, fishmongers and related food preparers", "Aquaculture workers" , "Animal producers not elsewhere classified")
# list with onet professions
nat_profs <- c(
  "Animal Caretakers", "Biological Technicians", "Butchers and Meat Cutters" ,
  "Cabinetmakers and Bench Carpenters" , "Carpenters","Farmers, Ranchers, and Other Agricultural Managers" ,
  "Farmworkers and Laborers, Crop, Nursery, and Greenhouse",
  "Farmworkers, Farm, Ranch, and Aquacultural Animals" ,
  "Fishing and Hunting Workers" ,"Foresters",
  "Pesticide Handlers, Sprayers, and Applicators, Vegetation")
## network given relatedness / proximity in geography
# net <- graph_from_adjacency_matrix(
#   (M_jobs2 > quantile(M_jobs2, 0.9)) , "undirected")
#
# net$weight <-  M_jobs2
# V(net)$eci_svd <- df_jobs2 |> arrange(jobs) |> pull(eci_svd) > 0
# V(net)$Inari <- rca[,45] |> as.logical() # Inari is the region where Naatamo is
# V(net)$name <- profs2
# V(net)$nat_prof <- profs2 %in% nat_profs
#
# lyt <- layout_nicely(net)
# sig <- sigmaFromIgraph(net, lyt) |>
#   addEdgeSize(oneSize = 0.5) |>
#   addEdgeColors(oneColor = "#e1e0df") |>
#   addNodeColors(colorAttr = "eci_svd", colorPal = "Paired") |>
#   addNodeSize(oneSize = 2)
#
# sig
#
# # htmlwidgets::saveWidget(sig, file= "img/finland_net.html", background = "#191919")
#
# ## network of jobs given the places where they occur
# plot.igraph(
#   net, layout = lyt, vertex.color = ifelse(V(net)$eci_svd, "#f1a340", alpha("grey50", 0.5)),
#   vertex.size =  ifelse(V(net)$Inari, 5, 3),
#   vertex.label = NA, vertex.frame.color = ifelse(V(net)$name %in% nat_profs, "red", NA),
#   vertex.alpha = 0.5, edge.width = 0.5)
#
# c <- ggplot(net, aes(x = x, y = y, xend = xend, yend = yend), layout = lyt) +
#   geom_edges(color = "grey50", alpha = 0.15, linewidth = 0.25) +
#   geom_nodes(aes(fill = eci_svd, alpha = eci_svd, size = Inari, color = nat_prof),
#              shape = 21, show.legend = TRUE) +
#   scale_alpha_manual(values = c(0.5,1)) +
#   scale_size_manual("Inari \n ECI > 0", values = c(0.5,1)) +
#   scale_fill_manual("Finland \n ECI > 0",values = c( "grey50", "#f1a340")) +
#   scale_color_manual("Natural resources\ndependent occupations",values = c( "white", "red")) + guides(alpha = "none") + coord_fixed() +
#   labs(tag = "C") +
#   theme_void(base_size = 6) +
#   theme(legend.position = "bottom", legend.title.position = "top")
#
# c


skill_net <- graph_from_adjacency_matrix(
  (M_jobs > quantile(M_jobs, 0.9) ), "undirected")
skill_net$weight <-  M_jobs
V(skill_net)$eci_svd <- df_jobs |> arrange(jobs) |> pull(eci_job2) > 0
V(skill_net)$Inari <- rca01[,45] |> as.logical() # Inari is the region where Naatamo is
V(skill_net)$name <- profs2
V(skill_net)$nat_prof <- profs2 %in% nat_profs
V(skill_net)$prop_towns <- rowSums(rca) / ncol(rca)
lyt2 <- layout_nicely(skill_net)

## network of jobs given the skills
plot.igraph(
  skill_net, layout = lyt2,
  vertex.color = ifelse(V(skill_net)$eci_svd, "#f1a340", alpha("grey50", 0.5)),
  vertex.size = 3,
  vertex.label = NA,
  vertex.frame.color = ifelse(V(skill_net)$name %in% nat_profs, "red", NA),
  vertex.alpha = 0.5, edge.width = 0.5)

c <- ggplot(skill_net, aes(x = x, y = y, xend = xend, yend = yend), layout = lyt2 ) +
  geom_edges(color = "grey50", alpha = 0.15, linewidth = 0.25) +
  geom_nodes(
    aes(fill = prop_towns, color = nat_prof),
    shape = 21, show.legend = TRUE, size = 1, alpha = 0.8) +
  #scale_alpha_manual(values = c(0.5,1)) +
  #scale_size_manual("Inari \n ECI > 0", values = c(2,3)) +
  scale_fill_viridis_c("Proportion of towns\nwith RCA > 1", option="E") +
  scale_color_manual("Natural resources\ndependent occupations",values = c("grey50" , "red")) + guides(alpha = "none") +
  labs(tag = "C") + coord_fixed() +
  theme_void(base_size = 6) +
  theme(legend.position = "bottom", legend.title.position = "top",
        legend.key.height = unit(2,"mm"),
        legend.key.width = unit(5,'mm'))

c



d <- ggplot(skill_net, aes(x = x, y = y, xend = xend, yend = yend), layout = lyt2 ) +
  geom_edges(color = "grey50", alpha = 0.15, linewidth = 0.25) +
  geom_nodes(
    aes(fill = Inari, color = nat_prof),
    shape = 21, show.legend = TRUE, size = 1, alpha = 0.8) +
  #scale_alpha_manual(values = c(0.5,1)) +
  #scale_size_manual("Kiruna \n ECI > 0", values = c(2,3)) +
  #scale_fill_viridis_c("Proportion of towns with RCA > 1") +
  scale_fill_manual("Inari: RCA > 0", values = c( "grey50", "#f1a340")) +
  scale_color_manual("Natural resources\ndependent occupations",values = c("grey" , "red")) + guides(alpha = "none", color = "none") +
  labs(tag = "D") + coord_fixed() +
  theme_void(base_size = 6) +
  theme(legend.position = "bottom", legend.title.position = "top")
d



sig <- sigmaFromIgraph(skill_net, lyt2) |>
  addEdgeSize(oneSize = 0.5) |>
  addEdgeColors(oneColor = "#e1e0df") |>
  addNodeColors(colorAttr = "eci_svd", colorPal = "Paired") |>
  addNodeSize(oneSize = 2)

sig

# network((M_jobs2 / colSums(M_jobs2) )> 0.0045, directed = FALSE) |>
#   sna::gplot( gmode = 'graph', usearrows = FALSE,
#              edge.col = 'grey50', edge.lwd = 0.1, vertex.border = NULL)
# colnames(M_jobs2) <- fin2019$isco4_name_en
# rownames(M_jobs2) <- fin2019$isco4_name_en
# heatmaply::heatmaply(
#     (M_jobs2 / colSums(M_jobs2)),
#     fontsize_col = 1, fontsize_row =1, branches_lwd=0.1,
#     file = "img/finland_heatmap.html", width = 300, height = 300)



#### Maps ####
library(sf)
library(rgdal)
lyrs <- ogrListLayers("data/Finland/Finland map/SuomenHallinnollisetKuntajakopohjaisetAluejaot_2023_10k.gpkg")
# dta <- src_sqlite ("data/Finland/Finland map/SuomenHallinnollisetKuntajakopohjaisetAluejaot_2023_10k.gpkg")
# fin1 <- tbl(dta, "Valtakunta")

lyrs <- st_layers("data/Finland/Finland map/SuomenHallinnollisetKuntajakopohjaisetAluejaot_2023_10k.gpkg")

fin <- read_sf(
  "data/Finland/Finland map/SuomenHallinnollisetKuntajakopohjaisetAluejaot_2023_10k.gpkg",
  layer = "Kunta") #

fin$nameswe [!fin$nameswe %in% df_towns$towns]
df_towns$towns[!df_towns$towns %in% fin$nameswe] %in% fin$namefin

load("~/Documents/Projects/MARAT/Arctic_QCA/data/coords.RData")

b <- fin |>
  mutate(towns = case_when(
    nameswe %in% df_towns$towns ~ nameswe,
    namefin %in% df_towns$towns ~ namefin
  )) |>
  left_join(df_towns ) |>
  ggplot() +
  geom_sf(aes(fill = shannon), linewidth = 0.01, show.legend = FALSE) +
  geom_sf(data = fin |> filter(namefin == "Inari"), color = "orange", fill = NA, linewidth = 0.5) +
  scale_fill_viridis_c(name = "Shannon\ndiversity") + #ggdark::dark_mode() +
  labs(tag = "B") +
  theme_light(base_size = 6) #+
  # theme(legend.position.inside = c(0.18, 0.8), legend.position = "inside",
  #      # plot.background = element_rect(fill = "#191919")
  #      legend.key.width = unit(2,"mm"), legend.key.height = unit(5,"mm")
  #       )

b + geom_sf_label( data = (fin |>
  terra::vect() |>
  terra::centroids() |>
  sf::st_as_sf()),
  aes(label = namefin), size = 3
)



fin |> ggplot() +
  geom_sf(aes(color = (namefin == "Inari")), show.legend = FALSE)

ggsave(
  file = "fig1_Finland.png", device = "png", path = "img/", width = 5, height = 5,
  bg = "white", dpi = 500,
  plot = ((a+b)/(c+d + plot_layout(guides = "collect") & theme(legend.position = "bottom")))
)


 library(tmap)

fin |>
  mutate(towns = case_when(
    nameswe %in% df_towns$towns ~ nameswe,
    namefin %in% df_towns$towns ~ namefin
  )) |>
  left_join(df_towns ) |>
  tm_shape() +
  tm_polygons("shannon")

#old_par <- par()
## Not working do all in ggplot later
# par(mar = c(0,0,0,0), bg = NA)
# ggsave(
#   filename = "img/finland_nets.pdf", device = "pdf", width = 7, height = 3, dpi = 300,
#   plot = a | (wrap_elements(
#     plot = ~plot.igraph(
#       net, layout = lyt, vertex.color = ifelse(V(net)$eci_svd, "#f1a340", alpha("grey50", 0.5)),
#       vertex.size =  ifelse(V(net)$Inari, 5, 3),
#       vertex.label = NA, vertex.frame.color = ifelse(V(net)$name %in% nat_profs, "red", NA),
#       vertex.alpha = 0.5, edge.width = 0.5, margin = 0), clip = TRUE) +
#     labs(tag = "B") + theme_void(base_size = 6) + theme(plot.margin = margin(0,0,0,0) ) /
#     wrap_elements(
#       plot = ~plot.igraph(
#         skill_net, layout = lyt2,
#         vertex.color = ifelse(V(skill_net)$eci_svd, "#f1a340", alpha("grey50", 0.5)),
#         vertex.size = 3,
#         vertex.label = NA,
#         vertex.frame.color = ifelse(V(skill_net)$name %in% nat_profs, "red", NA),
#         vertex.alpha = 0.5, edge.width = 0.5, margin = 0), clip = TRUE) +
#     labs(tag = "C") + theme_void(base_size = 6) + theme(plot.margin = margin(0,0,0,0) )) + plot_layout(widths = c(1,1), heights = c(1,2,2))
# )

#par(old_par)

# ggsave(filename = "img/finland_nets.png", device = "png", width = 7, height = 3, dpi = 500, bg = NULL, plot = last_plot())
#invert_geom_defaults()

#### Leftovers ####
# rca <- matrix(nrow = nrow(fin_mat), ncol = ncol(fin_mat))
# This follows the notation of Alabdulkareem 2018, where the s' is interpreted as the complement to s.
  # tic()
  # for (i in 1:nrow(fin_mat)){
  #   for (j in 1:ncol(fin_mat)){
  #     rca[i,j] <- (fin_mat[i,j] / sum(fin_mat[i,-j]))/ (sum(fin_mat[-i,j]) / sum(fin_mat[-i,-j]))
  #   }
  # }
  # toc() # 58s
  #
  # ## This follows the notation of Hidalgo 2007
  # tic()
  # for (i in 1:nrow(fin_mat)){
  #   for (j in 1:ncol(fin_mat)){
  #     rca[i,j] <- (fin_mat[i,j] / sum(fin_mat[i,]) )/ (sum(fin_mat[,j]) / sum(fin_mat))
  #   }
  # }
  # toc()
  #
  # ## This follows the notation of Hidalgo 2021
  # rca2 <- matrix(nrow = nrow(fin_mat), ncol = ncol(fin_mat))
  # tic()
  # for (i in 1:nrow(fin_mat)){
  #   for (j in 1:ncol(fin_mat)){
  #     rca2[i,j] <- (fin_mat[i,j] * sum(fin_mat) )/ (sum(fin_mat[,j]) * sum(fin_mat[i,]))
  #   }
  # }
  # toc() #20s
  #
  # phi <- matrix(ncol = ncol(fin_mat), nrow = ncol(fin_mat)) # cols are skills
  #
  # tic()
  # for (i in 1:nrow(phi)){
  #   for (j in 1:ncol(phi)){
  #     phi[i,j] <- (sum(rca01[,i] * rca01[,j] ) )/ (max(sum(rca01[,i]), sum(rca01[,j])))
  #   }
  # }
  # toc()


## Relatedness: This worked well, but EconGeo code is slightly faster. Note I used the bipartite project, in EconGeo one needs to calculate the one-mode projections.
# w <- matrix(nrow = nrow(fin_mat), ncol = ncol(fin_mat))
# tic()
# for (i in 1:nrow(w)){
#     w[i,] <- (sum( rca01[,-i] * phi[,-i] ) ) / ( colSums(phi[,-i]) )
# }
# toc()
#
# w <- rca01 %*% phi / colSums(phi)
# w2 <- relatedness(rca01)
#
# M_jobs <- matrix(ncol = nrow(rca01), nrow = nrow(rca01))
# M_skills <- matrix(ncol = ncol(rca01), nrow = ncol(rca01))
#
# tic()
# for (i in 1:nrow(M_jobs)){
#   for (j in 1:ncol(M_jobs)){
#     M_jobs[i,j] <- sum( (rca01[i,] * rca01[j, ]) / ( sum(rca01[i, ]) * colSums(rca01) ) )
#   }
# }
# toc() # 21s
#
# # M_jobs and M_skills should be a transition probability ranging 0:1
#
# tic()
# for (i in 1:nrow(M_skills)){
#   for (j in 1:ncol(M_skills)){
#     M_skills[i,j] <- sum( (rca01[,i] * rca01[,j]) / ( sum(rca01[, i]) * rowSums(rca01) ) )
#   }
# }
# toc() # 8s
#
# range(M_skills)
# rowSums(M_skills) |>
#   as.logical() |>
#   all() # all ones as expected
# rowSums(M_jobs) |>
#   as.logical() |>
#   all() # all ones as expected
