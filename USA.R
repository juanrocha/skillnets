library(tidyverse)
library(tictoc)
library(fs)

#### Read data ####\
# load onet
load("data/onet.Rda") # 34MB
# dat <- readxl::read_excel(
#   path = "data/USA/Employment_occupation_USA_2021.xlsx",
#   sheet = 1
# ) |> janitor::clean_names()
#
# key <- readxl::read_excel(
#   path = "data/USA/Employment_occupation_USA_2021.xlsx",
#   sheet = 2
# ) |> janitor::clean_names()
dat <- readxl::read_xlsx(path = "data/USA/laucnty22.xlsx", sheet = 1)

dat <- read_tsv("data/USA/oe.data.1.AllData.txt")

# check the meaning of column names:
key |> select (-x3) |> print(n=43)

dat |> names() # 147k obs

# filter info only for Alaska (AK): reduced to 652obs
dat <- dat |>
  filter(prim_state == "AK")

skimr::skim(dat)

# job titles exist in onet
#unique(onet$title)[
 sum( (dat |> pull(occ_title) |> unique()) %in% unique(onet$title))
#]

# codes exist but need to drop the last two digits:
(unique(onet$o_net_soc_code) |> str_sub(start = 1L, end = -4L) |> unique()) %in% unique(dat$occ_code) |> sum()

usa_mat <-  onet |>
   filter(scale_id == "IM") |>  # retain only importance scores
   select(-scale_id, -scale_name, onet = o_net_soc_code,
          -c(n:domain_source)) |>
   select(-element_id) |>
   mutate(data_value = scales::rescale(data_value, to = c(0,1))) |>
   group_by(onet, title, element_name) |>
   # for combinations with more than one value we average
   summarize(value = mean(data_value)) |>
   ungroup() |>
   pivot_wider(
     names_from = element_name, values_from = value, values_fill = 0) |>
   mutate(onet = str_sub(onet, start = 1L, end = -4L)) |>
   right_join(
     dat |>
       select(onet = occ_code, title = occ_title, jobs_1000) |>
       filter(jobs_1000 != "**")
   ) |>
  filter(!is.na(`Active Learning`)) # remove NAs in all variables

usa_mat |> skimr::skim() # 503 professions, no NAs

profs <- usa_mat$title #professions from onet

usa_mat <- usa_mat |> ungroup() |>
  select(where(is.numeric)) |> # this is the raw matrix for skills
  as.matrix()

class(usa_mat)
rownames(usa_mat) <- profs


#### Skill space ####
rca2 <- matrix(nrow = nrow(usa_mat), ncol = ncol(usa_mat))
tic()
for (i in 1:nrow(usa_mat)){
  for (j in 1:ncol(usa_mat)){
    rca2[i,j] <- (usa_mat[i,j] * sum(usa_mat) )/ (sum(usa_mat[,j]) * sum(usa_mat[i,]))
  }
}
toc() #28s

rca01 <- rca2>1

phi <- matrix(ncol = ncol(usa_mat), nrow = ncol(usa_mat)) # cols are skills
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
toc() # 35s

# M_jobs and M_skills should be a transition probability ranging 0:1
tic()
for (i in 1:nrow(M_skills)){
  for (j in 1:ncol(M_skills)){
    M_skills[i,j] <- sum( (rca01[,i] * rca01[,j]) / ( sum(rca01[, i]) * rowSums(rca01) ) )
  }
}
toc() # 9s

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
## Failure: data only includes anchorage and fairbanks
job_mat <- dat |>
  filter(jobs_1000 != "**") |>
  mutate(ppl = as.numeric(tot_emp)) |>
  mutate(log_jobs = log1p(ppl)) |>
  mutate(prop_jobs = log_jobs/ sum(log_jobs)) |>
  #ggplot(aes(prop_jobs)) + geom_density()
  select(-ppl, -log_jobs) |>
  select(area_title, occ_title, prop_jobs) |>
  pivot_wider(
    names_from = occ_title, values_from = prop_jobs, values_fill = 0) |>
  select(-area) |>
  as.matrix()

# there are zero sum columns that need to be dropped:
job_mat <- job_mat[,colSums(job_mat) > 0]
dim(job_mat) # 10 regions, 400 professions left
rownames(job_mat) <- dat$region |> unique()
rowSums(job_mat) |> as.logical() |> all()
