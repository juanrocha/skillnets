library(tidyverse)
library(here)
library(fs)
library(sf)
library(tictoc)
library(EconGeo)
library(ggnetwork)


#### Read data ####
# load onet
load("data/onet.Rda") # 34MB

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
  rename(ppl_2019 = x2019) |>
  fill(municipality, .direction = "down") |>
  filter(!is.na(ocupation))

swe2 |> skimr::skim() # 291 municipalities, 149 occupations, only one year
swe |> skimr::skim() # 45 regions, 430 occupations


# Data downloaded manually from https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__AM__AM0208__AM0208M/YREG60N/table/tableViewLayout1/
# The first file is a json-stats format, the second is just json. For some reason they
# are difficult (json-stat) to parse from normal libraries in R.
#swe3 <- jsonify::from_json(json = "data/Sweden/000005G2_20231013-104715.json") # works
#swe4 <- jsonlite::fromJSON(json = "data/Sweden/000005G2_20231013-111437.json") # doesn't work

## three digit match between Swedish classification and US ONET
swe_match <- readxl::read_xlsx(path = "data/Sweden/Swe_occu_ONETSOC.xlsx")|>
  janitor::clean_names() |>
  rename(swe_occ = occupation, en_occ = sw_occ)

sum(swe_match$onet_soc %in% unique(onet$title)) # 143 occupations matched out of 149
swe_match$onet_soc[!(swe_match$onet_soc %in% unique(onet$title))] # seems to be problems of spelling

#### Corrections ####

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
  select(-occupation, -soc_classification, -en_occ) |>
  group_by(municipality, onet_soc, swedish_occupation_english) |>
  summarize(jobs = sum(ppl_2019)) |>
  ungroup() |>
  group_by(municipality) |>
  ## Do we need to rescale to proportion of the popoulation per place?
  #mutate(prop_jobs = jobs / sum(jobs)) |>
  arrange(municipality) |> mutate(municipality = as_factor(municipality)) |>
  arrange(onet_soc) |> mutate(onet_soc = as_factor(onet_soc))
  #ggplot(aes(municipality, onet_soc)) + geom_tile(aes(fill = prop_jobs)) + theme(axis.text = element_blank())
  #pivot_wider(names_from = onet_soc, values_from = ppl_2009, values_fn = sum)

swe2 |> pull(swedish_occupation_english) |> unique() |> length()

# Now swe2 has the info of the matrix with proportion of population per place who works in certain jobs.

# this is the data for the matrix of occupations and skills (by importance)
swe_mat <- onet |>
  filter(scale_id == "IM") |>  # retain only importance scores
  select(-scale_id, -scale_name, onet = o_net_soc_code, -c(n:domain_source)) |>
  select(-element_id) |>
  mutate(data_value = scales::rescale(data_value, to = c(0,1))) |>
  group_by(onet, title, element_name) |>
  # for combinations with more than one value we average
  summarize(value = mean(data_value)) |>
  pivot_wider(names_from = element_name, values_from = value, values_fill = 0) |>
  right_join(swe_match |> rename(title = onet_soc)) |>
  filter(!is.na(`Active Learning`)) # there is one row with all NAs

## remove obs that are in one data and not the other
swe_mat$title %in% swe2$onet_soc |> all()
unique(swe2$swedish_occupation_english)[!unique(swe2$swedish_occupation_english) %in% swe_mat$title]


# simply updates the df with the codes and title names from onet. The order of rows
# is the same as in the matrix in case you need the names later.
#swe_match <- swe_mat |> select(where(is.character))

profs <- swe_mat$en_occ #professions from swe_match, so we keep unique values

swe_mat <- swe_mat |> ungroup() |>
  select(where(is.numeric)) |> # this is the raw matrix for skills
  as.matrix()

class(swe_mat)
rownames(swe_mat) <- profs

# gplots::heatmap.2(swe_mat, trace = "none", keysize = 1, mar = c(15,15))
# heatmaply::heatmaply(swe_mat, labRow = NULL, labCol= NULL)

#### Skillscape ####
# J240808: Update, moving a lot of development code with errors to the leftover section
# using now EconGeo package instead of my attempts to code some of the functions.
rca01 <- location_quotient(swe_mat, binary = TRUE)

## Effective use = phi or theta aka proximity: can be calculated with
phi <- herfindahl(swe_mat)

## Relatedness: can be calculated with other methods of distance, but currently produces
## the probability of changing jobs.
M_skills <- relatedness(t(rca01) %*% rca01, method = "prob")
M_jobs <- relatedness(rca01 %*% t(rca01), method = "prob")

e_jobs <- svd(M_jobs)
e_skills <- svd(M_skills)

# eci_jobs <-( e_jobs$d - mean(e_jobs$d)) / sd(e_jobs$d)
# eci_skills <- (e_skills$d - mean(e_skills$d) / sd(e_skills$d))

df_jobs <- tibble(
  jobs = rownames(swe_mat),
  eci_mor = mort(t(swe_mat)),
  herfindahl = herfindahl(swe_mat), krugman = krugman_index(swe_mat),
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
## Matrix for Sweden: logtransform because of the long tails
swe2 |> ggplot(aes(jobs)) + geom_density() + scale_x_log10()

job_mat <- swe2 |>
  # can't do this, there is ~10 jobs in the swedish system that have the same ONet
  #left_join(swe_match |> select(-swe_occ)) |>
  mutate(log_jobs = log1p(jobs)) |>
  #mutate(prop_jobs = log_jobs / sum(log_jobs)) |>
  #ggplot(aes(prop_jobs, municipality)) + geom_boxplot()
  select(-jobs, -onet_soc) |>
  filter(!is.na(swedish_occupation_english)) |>
  pivot_wider(names_from = swedish_occupation_english,
              values_from = log_jobs, values_fill = 0) |>
  ungroup() |> #municipality is a factor organized in desc order by codes
  select(-municipality) |>
  as.matrix()

dimnames(job_mat) # nrow 291 municipalities, ncol 149 jobs
#image(job_mat)
rownames(job_mat) <- swe2$municipality |> unique()
rowSums(job_mat) |> as.logical() |> all()

## RCA ##
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

# eci_jobs2 <- (d_jobs$d - mean(d_jobs$d)) / sd(d_jobs$d)
# eci_towns <- (d_towns$d - mean(d_towns$d) / sd(d_towns$d))


## plot the transition probability of jobs:
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

## completely anti-correlated! change order in creating jobs to ease comparison
df_jobs2 |>
  ggplot(aes(rank_mor, rank_svd)) +
  geom_point(aes(color = eci_svd > 0))

cor(df_jobs2$rank_mor, df_jobs2$rank_svd)



## plot the transition probability of jobs:
# spectralGP::image_plot(
#   t(M_jobs2),
#   nlevel = 10, legend.width = 0.025, col = hcl.colors(10, "YlOrRd", rev = TRUE))
# nrow 291 municipalities, ncol 139 jobs

df_towns <- tibble(
  towns = rownames(job_mat) |> str_remove(pattern = "\\d{4} ") ,
  shannon = entropy(exp(job_mat)), # the matrix was on log units, needs to be exp
  eci_mor = mort(t(rca))
) |> arrange(desc(eci_mor)) |>
  left_join(
    tibble(
      towns = str_remove(rownames(job_mat), pattern = "\\d{4} ")[order(colSums(M_towns), decreasing = TRUE)],
      eci_svd = (d_towns$d - mean(d_towns$d) / sd(d_towns$d))
    )
  ) |>
  mutate(rank_mor = order(eci_mor), rank_svd = order(eci_svd, decreasing = TRUE))

df_towns |>
  ggplot(aes(rank_svd, rank_mor)) +
  geom_point(aes(color = shannon, alpha = eci_svd > 0)) +
  scale_color_viridis_c()


# df_towns |>
#   arrange(eci_mor) |>
#   mutate(towns = as_factor(towns)) |>
#   #slice_max(n=25, order_by = eci_towns) |>
#   ggplot(aes(eci_mor, shannon)) +
#   geom_point() + ggrepel::geom_text_repel(aes(label = towns))  +
#   scale_x_continuous(trans = "log1p")

#231222: It seems that for municipalities higher diversity is better, so the orderning
# needs to be decreasing = TRUE. However, it seems that more specific / higly skill
# jobs are the ones with lower eci_jobs... the colSums or ubiquity seems to make
# more sense when decreasing = FALSE. Check with Cesar or collabs.

# tibble(
#   occupation = colnames(job_mat)[order(colSums(rca01), decreasing = FALSE)],
#   eci_jobs2 = eci_jobs2
# ) |>
#   arrange(eci_jobs2) |>
#   mutate(occupation = as_factor(occupation)) |>
#   slice_max(n=25, order_by = eci_jobs2) |>
#   ggplot(aes(eci_jobs2, occupation)) +
#   geom_point() +
#   scale_x_continuous(trans = "log1p")
#
#
#
# range(M_jobs2)
# plot(density(M_jobs2))
#
# colnames(M_jobs2) <- colnames(job_mat)
# rownames(M_jobs2) <- colnames(job_mat)

#
# heatmaply::heatmaply(
#   M_jobs2, fontsize_col = 1, fontsize_row =1, branches_lwd=0.1,
#   file = "img/sweden_heatmap.html", width = 300, height = 300)
#
# p0 <- "University and higher education teachers"
# #"Tailors, upholsterers and leather craftsmen"
# # "Teaching professionals not elsewhere classified"
#
# tibble(
#   profs = names(M_jobs2[rownames(M_jobs2) == p0 ,]),
#   prob = M_jobs2[rownames(M_jobs2) == p0 ,]
# ) |>
#   top_n(30, prob) |> arrange((prob)) |>
#   mutate(profs = as_factor(profs)) |>
#   ggplot(aes(prob, profs)) +
#   geom_point()

#
# M_jobs2[rownames(M_jobs2) == "Tailors, upholsterers and leather craftsmen" ,]
#
# dim(w)

#### map ####
swe_map <- st_read("~/Documents/Projects/DATA/GADM_maps/gadm40_SWE_shp/gadm40_SWE_2.shp")

df_towns <- df_towns |>
  mutate(towns = str_remove(towns, "\\d{4} "))

df_towns$towns[!df_towns$towns %in% swe_map$NAME_2]
swe_map$NAME_2[!swe_map$NAME_2 %in% df_towns$towns]

df_towns <- df_towns |>
  mutate(towns = case_when(
    towns == "Upplands Väsby" ~ "Upplands-Väsby",
    towns == "Malung-Sälen" ~ "Malung",
    .default = towns
  ))

swe_map |>
  left_join(df_towns, by = c("NAME_2" = "towns")) |>
  ggplot() +
  geom_sf(aes(fill = shannon), linewidth = 0.01) +
  scale_fill_viridis_c(name = "Shannon\ndiversity") +
  labs(tag = "A") +
  #ggdark::dark_mode() +
  theme_light(base_size = 10) +
  theme(legend.position.inside = c(0.8, 0.2),
        legend.position = "inside")
        #plot.background = element_rect(fill = "#191919"))

# ggsave(filename = "img/sweden_diversity.png", device = "png", width = 3, height = 4.5, dpi = 500, bg = NULL)

#### Network ####
library(sigmaNet)
library(igraph)
# Calculationg M_jobs with EconGeo does not produce a transition probability matrix,
# the colSums is not 1. To approximate a prob matrix, divide (normalize) by the colSums
plot(density(M_jobs2 )) #/ colSums(M_jobs2)

nat_profs <- c(
  "Animal breeders and keepers" ,
  "Butchers, bakers and food processors",
  "Biologists, pharmacologists and specialists in agriculture and forestry",
  "Specialists within environmental and health protection",
  "Forestry and agricultural production managers",
  "Berry pickers and planters",
  "Mixed crop and animal breeders" ,
  "Aquaculture and fishery workers",
  "Forestry and related workers",
  "Market gardeners and crop growers",
  "Wood treaters, cabinet-makers and related trades workers",
  "Wood processing and papermaking plant operators",
  "Veterinarians", "Veterinary assistants"
)

net <- graph_from_adjacency_matrix(
  (M_jobs2 > quantile(M_jobs2,0.90) ), "undirected")

edge_density(net)

net$weight <-  M_jobs2
V(net)$eci_svd <- df_jobs2 |> arrange(jobs) |> pull(eci_svd) > 0
V(net)$Kiruna <- rca[290,] |> as.logical() # Kiruna
V(net)$name <- colnames(M_jobs2)
V(net)$nat_prof <- colnames(M_jobs2) %in% nat_profs


lyt <- layout_nicely(net)
sig <- sigmaFromIgraph(net, lyt) |>
  addEdgeSize(oneSize = 0.5) |>
  addEdgeColors(oneColor = "#e1e0df") |>
  addNodeColors(colorAttr = "eci_svd", colorPal = "Paired") |>
  addNodeSize(oneSize = 2)

sig

htmlwidgets::saveWidget(sig, file= "img/sweden_net.html", background = "#191919")

## network of jobs given the places where they occur
plot.igraph(
  net, layout = lyt, vertex.color = ifelse(V(net)$eci_svd, "#f1a340", alpha("grey50", 0.5)),
  vertex.size =  ifelse(V(net)$Kiruna, 5, 3),
  vertex.label = NA, vertex.frame.color = ifelse(V(net)$name %in% nat_profs, "red", NA),
  vertex.alpha = 0.5, edge.width = 0.5)
## It's not so interesting, it means people can go to another place to get the same job given they have a similar job market with comparative advantage.
b <- ggplot(
  net, aes(x = x, y = y, xend = xend, yend = yend),
  layout = lyt) +
  geom_edges(color = "grey50", alpha = 0.15, linewidth = 0.25) +
  geom_nodes(aes(fill = eci_svd, alpha = eci_svd, size = Kiruna, color = nat_prof),
             shape = 21, show.legend = TRUE) +
  scale_alpha_manual(values = c(0.5,1)) +
  scale_size_manual("Kiruna \n ECI > 0", values = c(0.5,1)) +
  scale_fill_manual("Sweden \n ECI > 0",values = c( "grey50", "#f1a340")) +
  scale_color_manual("Natural resources\ndependent occupations",values = c( "white", "red")) + guides(alpha = "none") + coord_fixed() +
  labs(tag = "B") +
  theme_void(base_size = 10) +
  theme(legend.position = "bottom", legend.title.position = "top")

b
## network of jobs given the skills at which they have RCA
skill_net <- graph_from_adjacency_matrix(
  (M_jobs > quantile(M_jobs, 0.9) ), "undirected")
skill_net$weight <-  M_jobs
V(skill_net)$eci_svd <- df_jobs |> arrange(jobs) |> pull(eci_svd) > 0
#V(net)$Inari <- rca01[,] # Inari is the region where Naatamo is
V(skill_net)$name <- profs
V(skill_net)$nat_prof <- profs %in% nat_profs
lyt2 <- layout_nicely(skill_net)

## network of jobs given the skills
plot.igraph(
  skill_net, layout = lyt2,
  vertex.color = ifelse(V(skill_net)$eci_svd, "#f1a340", alpha("grey50", 0.5)),
  vertex.size = 3,
  vertex.label = NA,
  vertex.frame.color = ifelse(V(skill_net)$name %in% nat_profs, "red", NA),
  vertex.alpha = 0.5, edge.width = 0.5)

d <- ggplot(skill_net, aes(x = x, y = y, xend = xend, yend = yend), layout = lyt2 ) +
  geom_edges(color = "grey50", alpha = 0.15, linewidth = 0.25) +
  geom_nodes(aes(fill = eci_svd, alpha = eci_svd, color = nat_prof),
             shape = 21, show.legend = TRUE) +
  scale_alpha_manual(values = c(0.5,1)) +
  #scale_size_manual("Inari \n ECI > 0", values = c(2,3)) +
  scale_fill_manual("Sweden \n ECI > 0",values = c( "grey50", "#f1a340")) +
  scale_color_manual("Natural resources\ndependent occupations",values = c("white" , "red")) + guides(alpha = "none") +
  labs(tag = "D") + coord_fixed() +
  theme_void(base_size = 6) +
  theme(legend.position = "bottom", legend.title.position = "top")



#### Tests ####
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
# ## Effective use = phi or theta aka proximity
# phi <- matrix(ncol = ncol(swe_mat), nrow = ncol(swe_mat)) # cols are skills
# tic()
# for (i in 1:nrow(phi)){
#   for (j in 1:ncol(phi)){
#     phi[i,j] <- (sum(rca01[,i] * rca01[,j] ) )/ (max(sum(rca01[,i]), sum(rca01[,j])))
#   }
# }
# toc() #0.5s
