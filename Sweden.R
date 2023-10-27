library(tidyverse)
library(here)
library(fs)
library(sf)
library(tictoc)

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
  select(-occupation, -swedish_occupation_english, -soc_classification, -en_occ) |>
  group_by(municipality, onet_soc) |>
  summarize(jobs = sum(ppl_2019)) |>
  ungroup() |>
  group_by(municipality) |>
  ## Do we need to rescale to proportion of the popoulation per place?
  mutate(prop_jobs = jobs / sum(jobs)) |>
  arrange(municipality) |> mutate(municipality = as_factor(municipality)) |>
  arrange(onet_soc) |> mutate(onet_soc = as_factor(onet_soc))
  #ggplot(aes(municipality, onet_soc)) + geom_tile(aes(fill = prop_jobs)) + theme(axis.text = element_blank())
  #pivot_wider(names_from = onet_soc, values_from = ppl_2009, values_fn = sum)



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

# simply updates the df with the codes and title names from onet. The order of rows
# is the same as in the matrix in case you need the names later.
swe_match <- swe_mat |> select(where(is.character))

swe_mat <- swe_mat |> ungroup() |>
  select(where(is.numeric)) |> # this is the raw matrix for skills
  as.matrix()

#### Skillscape ####
# code copied from Finland script but simplified
rca <- matrix(nrow = nrow(swe_mat), ncol = ncol(swe_mat))
## This follows the notation of Hidalgo 2007
tic()
for (i in 1:nrow(swe_mat)){
  for (j in 1:ncol(swe_mat)){
    rca[i,j] <- (swe_mat[i,j] / sum(swe_mat[i,]) )/ (sum(swe_mat[,j]) / sum(swe_mat))
  }
}
toc() # 2.5s

## This follows the notation of Hidalgo 2021
rca2 <- matrix(nrow = nrow(swe_mat), ncol = ncol(swe_mat))
tic()
for (i in 1:nrow(swe_mat)){
  for (j in 1:ncol(swe_mat)){
    rca2[i,j] <- (swe_mat[i,j] * sum(swe_mat) )/ (sum(swe_mat[,j]) * sum(swe_mat[i,]))
  }
}
toc() #2.5s

## Both notations are equivalent because (a/b)/(c/d) = a*d / b*c
identical((rca>1),(rca2>1)) # TRUE
rca01 <- rca2>1

## Effective use = phi or theta aka proximity
phi <- matrix(ncol = ncol(swe_mat), nrow = ncol(swe_mat)) # cols are skills

tic()
for (i in 1:nrow(phi)){
  for (j in 1:ncol(phi)){
    phi[i,j] <- (sum(rca01[,i] * rca01[,j] ) )/ (max(sum(rca01[,i]), sum(rca01[,j])))
  }
}
toc() #0.5s

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
toc() # 1.1s

# M_jobs and M_skills should be a transition probability ranging 0:1
tic()
for (i in 1:nrow(M_skills)){
  for (j in 1:ncol(M_skills)){
    M_skills[i,j] <- sum( (rca01[,i] * rca01[,j]) / ( sum(rca01[, i]) * rowSums(rca01) ) )
  }
}
toc() # 3.8s

range(M_skills)
rowSums(M_skills) # all ones as expected
rowSums(M_jobs) # all ones as expected
e_jobs <- svd(M_jobs)
e_skills <- svd(M_skills)

eci_jobs <-( e_jobs$d - mean(e_jobs$d)) / sd(e_jobs$d)
eci_skills <- (e_skills$d - mean(e_skills$d) / sd(e_skills$d))

#### Occupations space ####
## Matrix for Sweden: logtransform because of the lonng tails
swe2 |> ggplot(aes(prop_jobs)) + geom_density() + scale_x_log10()

job_mat <- swe2 |>
  mutate(log_jobs = log1p(jobs)) |>
  mutate(prop_jobs = log_jobs / sum(log_jobs)) |>
  #ggplot(aes(prop_jobs, municipality)) + geom_boxplot()
  select(-jobs, -log_jobs) |>
  pivot_wider(names_from = onet_soc, values_from = prop_jobs, values_fill = 0) |>
  ungroup() |> #municipality is a factor organized in desc order by codes
  select(-municipality) |>
  as.matrix()

dim(job_mat) # 140 jobs, 291 municipalities
image(job_mat)






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

