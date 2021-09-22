## Read data
## Juan Rocha

library(tidyverse)

dat <- readxl::read_xlsx("data/Skill_ONET.xlsx") %>%
  janitor::clean_names()

dat %>% pull(element_name) %>% unique() # 35 skills
dat %>% pull(title) %>% unique() # 873 professions

# matrix of professions vs skills by importance
mat <- dat %>%
  filter(scale_name == "Importance") %>%
  select(title, element_name, data_value) %>%
  arrange(title, element_name) %>%
  pivot_wider(names_from = element_name, values_from = data_value) %>%
  select(-title) %>%
  as.matrix()

## manually calculate relative competitive advantage, rca

rca <- matrix(nrow = nrow(mat), ncol = ncol(mat))

for (i in 1:nrow(mat)){
  for(j in 1:ncol(mat)){
    rca[i,j] <- ( mat[i,j] / sum(mat[i,])) / ((sum(mat[,j]))/sum(mat))
  }
}

effective_use <- rca > 1

theta <- matrix(nrow = ncol(effective_use), ncol = ncol(effective_use))

for (i in 1:nrow(theta)){
  for(j in 1:ncol(theta)){
    theta[i,j] <- ( sum(effective_use[,i]*effective_use[,j]) / max(sum(effective_use[,j]), sum(effective_use[,i])))
  }
}

dim(theta)
