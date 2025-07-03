library(tidyverse)
library(here)
library(fs)
library(sf)
library(tictoc)
library(EconGeo)
library(ggnetwork)
library(patchwork)

#### Read data ####\
# load onet
load("data/onet.Rda") # 34MB
# Canadian classification translated to ONET
canada_onet <- readxl::read_xlsx(path = "data/Canada/Canada  ONET.xlsx") |>
  janitor::clean_names()

## 4digit classification at regional level
fls <- dir_ls("data/Canada/Region_4digits_2016_CA/") |>
  str_subset("CSV$")

dat <- map(fls, read_csv, skip = 6)

regions <- fls |> str_remove("data/Canada/Region_4digits_2016_CA/") |>
  str_remove("_4dig_2016_CA.CSV") |> str_remove("_4dig_2016_Ca.CSV")

dat <- map2(dat, regions, function(x,y) {x$region <- y; return(x)}) |>
  bind_rows()

dat <- dat |> janitor::clean_names() |> #names()
  rename(occupation = 1, ppl = 2) |>
  select(occupation, ppl, region) |>
  filter(str_detect(occupation, "^\\d{4} ")) # |>
# filter strings that starts with 4 digits
  # ggplot(aes(occupation, region)) + geom_tile(aes(fill = ppl))
  #skimr::skim() # 500 occupations, 10 regions.

canada_onet <- canada_onet |>
  rename(occupation = 1, onet = 2) |>
  filter(!is.na(onet)) |>
  left_join(onet |> select(onet = 1, title) |> unique() )

## Jobs matrix per town

job_mat <- dat |>
  left_join(canada_onet) |>
  group_by(title, region) |>
  summarize(ppl = sum(ppl)) |>
  mutate(log_jobs = log1p(ppl)) |>
  #mutate(prop_jobs = log_jobs/ sum(log_jobs)) |>
  #ggplot(aes(prop_jobs)) + geom_density()
  select(-ppl) |>
  arrange(title) |>
  filter(!is.na(title)) |>
  pivot_wider(
    names_from = title, values_from = log_jobs) |>
  select(-region) |>
  as.matrix()

dim(job_mat) # 10 regions, 287 professions left

# there are zero sum columns that need to be dropped:
job_mat <- job_mat[,colSums(job_mat) > 0]
dim(job_mat) # 10 regions, 247professions left
rownames(job_mat) <- dat$region |> unique()
rowSums(job_mat) |> as.logical() |> all()

profs <- colnames(job_mat)

## Matrix is in alphabethic order both for skills and jobs
can_mat <- onet |>
  filter(scale_id == "IM") |>  # retain only importance scores
  select(-scale_id, -scale_name, onet = o_net_soc_code, -c(n:domain_source)) |>
  select(-element_id) |>
  mutate(data_value = scales::rescale(data_value, to = c(0,1))) |>
  group_by(onet, title, element_name) |>
  # for combinations with more than one value we average
  summarize(value = mean(data_value)) |>
  ungroup() |>
  arrange((title)) |>
  filter(title %in% profs) |>
  pivot_wider(
    names_from = element_name, values_from = value, values_fill = 0) |>
  right_join(canada_onet) |>
  select(-occupation) |> #names()
  unique() |>
  filter(!is.na(`Active Learning`))  |> # remove NAs in all variables
  ungroup() |> unique()

#can_mat |> ungroup() |> skimr::skim() # No NAs left!

profs2 <- can_mat$title #professions from onet
all(profs == profs2)

can_mat <- can_mat |> ungroup() |>
  select(where(is.numeric)) |> # this is the raw matrix for skills
  as.matrix()

class(can_mat)
rownames(can_mat) <- profs
dim(can_mat) # 247 jobs, 176 skills | using ONET professions, not Canadian ones.
# with canadian profs there will be >400 but then results from skill nets and
# occupation spaces wont be comparable.

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
  shannon = entropy(exp(job_mat)), # the matrix was on log units, needs to be exp
  eci_mor = mort(t(rca))
) |> arrange(desc(eci_mor)) |>
  left_join(
    tibble(
      towns = rownames(job_mat) [order(colSums(M_towns), decreasing = TRUE)],
      eci_svd = (d_towns$d - mean(d_towns$d) / sd(d_towns$d))
    )
  ) |>
  mutate(rank_mor = order(eci_mor), rank_svd = order(eci_svd, decreasing = TRUE))

df_towns |>
  ggplot(aes(rank_svd, rank_mor)) +
  geom_point(aes(color = shannon, alpha = eci_svd > 0)) +
  scale_color_viridis_c()


### Voy aqui:
### - weird shape in results of ranks
### - check if there is more data for all canada, only 10 regions now
### - make map and networks with current analysis








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


