## Read data
## Juan Rocha

library(tidyverse)
library(tictoc)
library(network)


dat <- readxl::read_xlsx("data/Skill_ONET.xlsx") %>%
  janitor::clean_names()

dat %>% pull(element_name) %>% unique() # 35 skills
dat %>% pull(title) %>% unique() # 873 professions
dat |> pull(date) |> unique() |> lubridate::parse_date_time(orders = "my") |> sort() # 10yrs

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

tic()
for (i in 1:nrow(mat)){
  for(j in 1:ncol(mat)){
    rca[i,j] <- ( mat[i,j] / sum(mat[i,])) / ((sum(mat[,j]))/sum(mat))
  }
}
toc() #2.5s


effective_use <- rca > 1

# theta is the matrix of proximity across skills
theta <- matrix(nrow = ncol(effective_use), ncol = ncol(effective_use))

tic()
for (i in 1:nrow(theta)){
  for(j in 1:ncol(theta)){
    theta[i,j] <- ( sum(effective_use[,i]*effective_use[,j]) / max(sum(effective_use[,j]), sum(effective_use[,i])))
  }
}
toc()


dim(theta)
image(theta)

# phi is the matrix of proximity across professions
phi <- matrix(nrow = nrow(effective_use), ncol = nrow(effective_use))

effective_use <- t(effective_use)

tic()
for (i in 1:nrow(phi)){
  for(j in 1:ncol(phi)){
    phi[i,j] <- ( sum(effective_use[,i]*effective_use[,j]) / max(sum(effective_use[,j]), sum(effective_use[,i])))
  }
}
toc()

image(phi)
plot(density(phi))
quantile(phi, 0.9)
plot(density(theta))

library(network)

net_phi <- network(phi > quantile(phi, 0.9), matrix.type = "adjacency", directed = FALSE)

net_phi %e% "weight" <- phi

tic()
plot(net_phi) # takes ages
toc() #30s

net_theta <- network(theta > quantile(theta, 0.75), matrix.type = "adjacency", directed = FALSE)
net_theta %e% "weight" <- theta


plot(net_theta)



