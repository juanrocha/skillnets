library(tidyverse)
library(tictoc)

#### Read data ####

# load onet
load("data/onet.Rda") # 34MB

tic()
dat <- readxl::read_xlsx(
  path = "data/Finland/Municipality_4digits_2010_2019_FIN.xlsx",
  skip = 6)
toc() #17s

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
  select(o_net_soc_code, title) |>
  unique()

#onet_occ |> filter(str_detect(title, "Milk"))

### Preserve onet professions reported in Finland
onet |>
  filter(scale_id == "IM") |>  # retain only importance scores
  select(-scale_id, -scale_name, -c(n:domain_source), onet = o_net_soc_code) |>
  inner_join(df_match, relationship = "many-to-many") |>
  select(onet, title, code_isco4, occupation) |>
  unique() |>
  # this is to check manually potential mistakes and update corrections above.
  write_csv(file = "data/Finland/onet_matching.csv")

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
fin_onet <- onet |>
  filter(scale_id == "IM") |>  # retain only importance scores
  select(-scale_id, -scale_name, onet = o_net_soc_code, -c(n:domain_source)) |>
  #filter(onet %in% df_match$onet) |> # filter reduces to matches
  # we lose occupations the moment we merge different jobs on the finish classification
  # under the same onet codes. It is unavoidable. To avoid that use right_join
  select(-title, -element_id) |> #pull(data_value) |> range()
  mutate(data_value = scales::rescale(data_value, to = c(0,1))) |>
  group_by(onet, element_name) |>
  # for combinations with more than one value we average
  summarize(value = mean(data_value)) |>
  pivot_wider(names_from = element_name, values_from = value, values_fill = 0) |>
  # rigth join keeps all rows of df_match, so duplicate vectors for finish occupations
  # with the same onet code.
  right_join(df_match) |>
  select(onet, code_isco4, occupation, 4:last_col())


fin_mat <- fin_onet |> ungroup() |> select(-c(onet:occupation)) |>
  as.matrix()
dim(fin_mat) # 436 occupations, 176 skills, bear in mind 293 unique onet occupations

## Missing values? : problem solved!
prblm <- fin_onet |> filter(is.na(`Active Listening`)) |>
  select(onet:occupation)
# calculate RCA


#### Skillscape ####

rca <- matrix(nrow = nrow(fin_mat), ncol = ncol(fin_mat))
# This follows the notation of Alabdulkareem 2018, where the s' is interpreted as the complement to s.
tic()
for (i in 1:nrow(fin_mat)){
  for (j in 1:ncol(fin_mat)){
    rca[i,j] <- (fin_mat[i,j] / sum(fin_mat[i,-j]))/ (sum(fin_mat[-i,j]) / sum(fin_mat[-i,-j]))
  }
}
toc() # 58s

## This follows the notation of Hidalgo 2007
tic()
for (i in 1:nrow(fin_mat)){
  for (j in 1:ncol(fin_mat)){
    rca[i,j] <- (fin_mat[i,j] / sum(fin_mat[i,]) )/ (sum(fin_mat[,j]) / sum(fin_mat))
  }
}
toc()

## This follows the notation of Hidalgo 2021
rca2 <- matrix(nrow = nrow(fin_mat), ncol = ncol(fin_mat))
tic()
for (i in 1:nrow(fin_mat)){
  for (j in 1:ncol(fin_mat)){
    rca2[i,j] <- (fin_mat[i,j] * sum(fin_mat) )/ (sum(fin_mat[,j]) * sum(fin_mat[i,]))
  }
}
toc() #20s

## Both notations are equivalent because (a/b)/(c/d) = a*d / b*c
identical((rca>1),(rca2>1)) # TRUE

rca01 <- rca2>1

## Effective use = phi or theta aka proximity

phi <- matrix(ncol = ncol(fin_mat), nrow = ncol(fin_mat)) # cols are skills

tic()
for (i in 1:nrow(phi)){
  for (j in 1:ncol(phi)){
    phi[i,j] <- (sum(rca01[,i] * rca01[,j] ) )/ (max(sum(rca01[,i]), sum(rca01[,j])))
  }
}
toc()

## Relatedness:
w <- matrix(nrow = nrow(fin_mat), ncol = ncol(fin_mat))
tic()
for (i in 1:nrow(w)){
    w[i,] <- (sum( rca01[,-i] * phi[,-i] ) ) / ( colSums(phi[,-i]) )
}
toc()

w <- rca01 %*% phi / colSums(phi)

M_jobs <- matrix(ncol = nrow(rca01), nrow = nrow(rca01))
M_skills <- matrix(ncol = ncol(rca01), nrow = ncol(rca01))

tic()
for (i in 1:nrow(M_jobs)){
  for (j in 1:ncol(M_jobs)){
    M_jobs[i,j] <- sum( (rca01[i,] * rca01[j, ]) / ( sum(rca01[i, ]) * colSums(rca01) ) )
  }
}
toc() # 21s

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
e_jobs <- svd(M_jobs)
e_skills <- svd(M_skills)

eci_jobs <-( e_jobs$d - mean(e_jobs$d)) / sd(e_jobs$d)
eci_skills <- (e_skills$d - mean(e_skills$d) / sd(e_skills$d))

#### Occupations space ####
## Matrix for Finland
fin2019 <- dat |>
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
  pivot_wider(names_from = municipality_name_en, values_from = employed_persons, values_fill = 0)

job_mat <- as.matrix(fin2019 |> select(-c(1:3)))

dim(job_mat) # 413 jobs, 309 municipalities






#### Maps ####

