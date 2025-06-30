library(tidyverse)
library(tictoc)
library(fs)

#### Read data ####\
# load onet
load("data/onet.Rda") # 34MB
# Canadian classification translated to ONET
canada_onet <- readxl::read_xlsx(path = "data/Canada/Canada  ONET.xlsx")

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

can_mat <- onet |>
  filter(scale_id == "IM") |>  # retain only importance scores
  select(-scale_id, -scale_name, onet = o_net_soc_code, -c(n:domain_source)) |>
  select(-element_id) |>
  mutate(data_value = scales::rescale(data_value, to = c(0,1))) |>
  group_by(onet, title, element_name) |>
  # for combinations with more than one value we average
  summarize(value = mean(data_value)) |>
  ungroup() |>
  pivot_wider(
    names_from = element_name, values_from = value, values_fill = 0) |>
  right_join(canada_onet |>
               rename(onet = ONETcode) |>
               filter(!is.na(onet))) |>
  filter(!is.na(`Active Learning`)) # remove NAs in all variables

can_mat |> ungroup() |> skimr::skim() # No NAs left!

profs <- can_mat$title #professions from onet

can_mat <- can_mat |> ungroup() |>
  select(where(is.numeric)) |> # this is the raw matrix for skills
  as.matrix()

class(can_mat)
rownames(can_mat) <- profs

#### Skill space ####
rca2 <- matrix(nrow = nrow(can_mat), ncol = ncol(can_mat))
tic()
for (i in 1:nrow(can_mat)){
  for (j in 1:ncol(can_mat)){
    rca2[i,j] <- (can_mat[i,j] * sum(can_mat) )/ (sum(can_mat[,j]) * sum(can_mat[i,]))
  }
}
toc() #19s

rca01 <- rca2>1

phi <- matrix(ncol = ncol(can_mat), nrow = ncol(can_mat)) # cols are skills
tic()
for (i in 1:nrow(phi)){
  for (j in 1:ncol(phi)){
    phi[i,j] <- (sum(rca01[,i] * rca01[,j] ) )/ (max(sum(rca01[,i]), sum(rca01[,j])))
  }
}
toc() #0.6s
## Relatedness:
w <- rca01 %*% phi / colSums(phi)

M_jobs <- matrix(ncol = nrow(rca01), nrow = nrow(rca01))
M_skills <- matrix(ncol = ncol(rca01), nrow = ncol(rca01))

tic()
for (i in 1:nrow(M_jobs)){
  for (j in 1:ncol(M_jobs)){
    M_jobs[i,j] <- sum( (rca01[i,] * rca01[j, ]) / ( sum(rca01[i, ]) * colSums(rca01) ) )
  }
}
toc() # 19s

# M_jobs and M_skills should be a transition probability ranging 0:1
tic()
for (i in 1:nrow(M_skills)){
  for (j in 1:ncol(M_skills)){
    M_skills[i,j] <- sum( (rca01[,i] * rca01[,j]) / ( sum(rca01[, i]) * rowSums(rca01) ) )
  }
}
toc() # 8s

range(M_skills)
rowSums(M_skills) # all ones as expected
rowSums(M_jobs) # all ones as expected
colnames(M_jobs) <- profs
rownames(M_jobs) <- profs

gplots::heatmap.2(M_jobs, trace = "none", keysize = 1, mar = c(15,15))
heatmaply::heatmaply(
  M_jobs, fontsize_col = 6, fontsize_row =6)

e_jobs <- svd(M_jobs)
e_skills <- svd(M_skills)

eci_jobs <-( e_jobs$d - mean(e_jobs$d)) / sd(e_jobs$d)
eci_skills <- (e_skills$d - mean(e_skills$d) / sd(e_skills$d))

#### Occupations space ####
job_mat <- dat |>
  mutate(log_jobs = log1p(ppl)) |>
  mutate(prop_jobs = log_jobs/ sum(log_jobs)) |>
  #ggplot(aes(prop_jobs)) + geom_density()
  select(-ppl, -log_jobs) |>
  # using the codes as colnames to avoid errors
  mutate(occupation = str_sub(occupation, start = 1L, end = 4L)) |>
  pivot_wider(
    names_from = occupation, values_from = prop_jobs, values_fill = 0) |>
  select(-region) |>
  as.matrix()

# there are zero sum columns that need to be dropped:
job_mat <- job_mat[,colSums(job_mat) > 0]
dim(job_mat) # 10 regions, 400 professions left
rownames(job_mat) <- dat$region |> unique()
rowSums(job_mat) |> as.logical() |> all()


## RCA ##
## This follows the notation of Hidalgo 2021
rca <- matrix(nrow = nrow(job_mat), ncol = ncol(job_mat))
tic()
for (i in 1:nrow(job_mat)){
  for (j in 1:ncol(job_mat)){
    rca[i,j] <- (job_mat[i,j] * sum(job_mat) )/ (sum(job_mat[,j]) * sum(job_mat[i,]))
  }
}
toc() #0.1s

rca01 <- rca>1

## Effective use = phi or theta aka proximity
phi <- matrix(ncol = ncol(job_mat), nrow = ncol(job_mat)) # cols are towns
tic()
for (i in 1:nrow(phi)){
  for (j in 1:ncol(phi)){
    phi[i,j] <- (sum(rca01[,i] * rca01[,j] ) )/ (max(sum(rca01[,i]), sum(rca01[,j])))
  }
}
toc() # 0.7s

## Relatedness:
w <- rca01 %*% phi / colSums(phi)
# nrow 291 municipalities, ncol 139 jobs
M_towns <- matrix(ncol = nrow(rca01), nrow = nrow(rca01))
M_jobs2 <- matrix(ncol = ncol(rca01), nrow = ncol(rca01))

tic()
for (i in 1:nrow(M_towns)){
  for (j in 1:ncol(M_towns)){
    M_towns[i,j] <- sum( (rca01[i,] * rca01[j,]) / ( sum(rca01[i, ]) * colSums(rca01) ) )
  }
}
toc() # 6s

# M_x should be a transition probability ranging 0:1

tic()
for (i in 1:nrow(M_jobs2)){
  for (j in 1:ncol(M_jobs2)){
    M_jobs2[i,j] <- sum( (rca01[,i] * rca01[,j ]) / ( sum(rca01[,i ]) * rowSums(rca01) ) )
  }
}
toc() # 14s

range(M_jobs2)
rowSums(M_towns) |> as.logical() |> all() # all ones as expected
rowSums(M_jobs2) |> as.logical() |> all() # all ones as expected

e_jobs2 <- svd(M_jobs2)
e_towns <- svd(M_towns)

eci_jobs2 <- (e_jobs2$d - mean(e_jobs2$d)) / sd(e_jobs2$d)
eci_towns <- (e_towns$d - mean(e_towns$d) / sd(e_towns$d))


## transition probs for jobs
tibble(
  towns = rownames(job_mat)[order(rowSums(rca01), decreasing = TRUE)],
  eci_towns = eci_towns,
  diversity = rowSums(rca01)[order(rowSums(rca01), decreasing = TRUE)]
) |>
  arrange(eci_towns) |>
  mutate(towns = as_factor(towns)) |>
  slice_max(n=25, order_by = eci_towns) |>
  ggplot(aes(eci_towns, diversity)) +
  geom_point() + ggrepel::geom_text_repel(aes(label = towns))  +
  scale_x_continuous(trans = "log1p")




## To make places comparable, one need to transform number of people per profession to proportion of population: workers / total population
dat |>
  group_by(region) |>
  mutate(prop_workers = ppl / sum(ppl))

onet

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


