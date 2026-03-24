library(tidyverse)
library(igraph)
library(patchwork)

load("data/Finland/ECI_2019_Finland.Rda")
fin_net <- skill_net
load("data/Sweden/ECI_Sweden.Rda")
swe_net <- skill_net
load("data/Canada/ECI_Canada.Rda")
can_net <- net

rm(net, skill_net, df_jobs, df_jobs2, df_towns)


#### SM - Degree distribution ####
a <- ggplot() +
  geom_density(data = tibble(deg = degree(fin_net)), aes(deg)) +
  labs(tag = "A", x = "Degree", y = "Density") +
  theme_light(base_size = 8)

b <- ggplot() +
  geom_density(data = tibble(deg = degree(swe_net)), aes(deg)) +
  labs(tag = "B", x = "Degree", y = "Density") +
  theme_light(base_size = 8)

c <- ggplot() +
  geom_density(data = tibble(deg = degree(can_net)), aes(deg)) +
  labs(tag = "C", x = "Degree", y = "Density") +
  theme_light(base_size = 8)

(a+b+c)

#### Networks ####

tibble(
  bet = betweenness(fin_net, normalized = TRUE),
  deg = degree(fin_net),
  job = V(fin_net)$name
) |>
  mutate(label = ifelse(bet > 0.1, job, NA)) |>
  ggplot(aes(deg, bet)) +
  geom_point() +
  geom_text(aes(label = label), hjust = 0)

fin_net
