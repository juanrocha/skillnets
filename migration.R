## migration data: Finland

library(tidyverse)
library(tictoc)
library(fs)
library(plm)
library(patchwork)

#### Load and clean data ####
fls <- dir_ls("data/Finland/migration/") |> str_subset( pattern = "csv")

dat <- map(
  fls |> str_subset("2024", negate = TRUE),
  read_csv, skip = 1, locale=locale(encoding="latin1"))

pop <- read_csv(
  fls |> str_subset("2024", negate = FALSE), locale=locale(encoding="latin1"),
  skip = 1
) |> filter(Area != "WHOLE COUNTRY")

pop <- pop |> pivot_longer(cols = `1990`:last_col(), names_to = "year", values_to = "value") |>
  pivot_wider(names_from = Information, values_from = value) |>
  janitor::clean_names() |>
  filter(year >= 2010, year <= 2019) # align panel

load("data/Finland/ECI_Finland.Rda")


# there are some files (7:9) with arrivals on the rows, departures on the columns
dat[[10]]

c(1:6, 10)

dat[c(1:6, 10)]  <- dat[c(1:6, 10)] |> map(function(x){
  x |> filter(`Area of departure` != "WHOLE COUNTRY") |>
    pivot_longer(2:last_col(), names_to = "Area of destination", values_to = "ppl") |>
    mutate(dataset = str_sub(`Area of destination`, 1L, 36L),
           `Area of destination` = str_sub(`Area of destination`, 37L, -1L)) |>
    filter(`Area of destination` != "WHOLE COUNTRY") |>
    mutate(year = str_extract(dataset, "\\d{4}")) |>
    select(-dataset) |>
    rename("departure" = 1, "destination" = 2) |>
    mutate(departure = str_remove(departure, "Departure - "),
           destination = str_remove(destination, "Arrival - "))
})


dat[c(7:9)]  <- dat[c(7:9)] |> map(function(x){
  x |>
  filter(`Area of arrival` != "WHOLE COUNTRY") |>
  pivot_longer(2:last_col(), names_to = "Area of departure", values_to = "ppl") |>
  mutate(dataset = str_sub(`Area of departure`, 1L, 36L),
         `Area of departure` = str_sub(`Area of departure`, 37L, -1L)) |>
  filter(`Area of departure` != "WHOLE COUNTRY") |>
  mutate(year = str_extract(dataset, "\\d{4}")) |>
  select(-dataset) |>
  rename("departure" = 2, "destination" = 1) |>
  mutate(departure = str_remove(departure, "Departure - "),
         destination = str_remove(destination, "Arrival - "))
})

dat |>
  bind_rows() |> #skimr::skim()
  ggplot(aes(log1p(ppl))) +
  geom_density()

## Recover out and in migreation
pop <- dat |> bind_rows() |> group_by(departure, year) |>
  summarize(outmigration = sum(ppl)) |>
  rename(area = departure) |>
  ungroup() |>
  right_join(pop)

pop <- dat |> bind_rows() |> group_by(destination, year) |>
  summarize(inmigration = sum(ppl)) |>
  rename(area = destination) |>
  ungroup() |>
  right_join(pop)

pop <- pop |>
  mutate(
    prop_outmig = outmigration / population_31_dec,
    prop_inmig = inmigration / population_31_dec
  ) |>
  mutate(mig_ratio = prop_inmig/prop_outmig)

ggplot(pop, aes(prop_inmig, prop_outmig)) +
  geom_point(aes(color = mig_ratio), alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = 2) +
  scale_color_gradient2(midpoint = 1, mid = "grey")


### networks ####
tic()
dat <- map(dat, function(x){
  x |> select(departure, destination, ppl) |>
    filter(departure != destination) |>
    filter(ppl > 0) |>
    network::network(directed = TRUE, ignore.eval = FALSE)
})
toc() #1.6s

map(dat, network::network.density) # ~0.2

tic()
df_nets <- map(dat, function(x){
  tibble(
    towns = network::network.vertex.names(x),
    indeg = sna::degree(x, gmode = "digraph", cmode = "indegree"),
    outdeg = sna::degree(x, gmode = "digraph", cmode = "outdegree"),
    betw = sna::betweenness(x, gmode = "digraph")
  )
})
toc() #1.7s


df_nets <- map2(
  df_nets,
  c(2010:2019),
  function(x,y) {
    x$year <- y
    return(x)
  }
) |> bind_rows()

df_towns2 <- map2(
  df_towns2,
  names(df_towns2) |> as.numeric(),
  function(x,y) {
    x$year <- y
    return(x)
  }
) |> bind_rows()

# test all town names are compatible and consistent
all(unique(df_nets$towns ) %in% unique(df_towns2$towns))
all(unique(df_nets$towns) %in% unique(pop$area))
all(unique(pop$area) %in% unique(df_nets$towns))

df_towns2 <- df_towns2 |>
  left_join(df_nets) |>
  left_join(pop |> mutate(year = as.numeric(year)) |> rename(towns = area))

df_towns2 |>
  ggplot(aes(prop_outmig, prop_inmig)) +
  geom_point(aes(color = diversity)) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_viridis_c(direction = 1)

## the most diverse towns seems to be the more stable migration wise
df_towns2 |>
  # group_by(towns) |>
  # # summarize(across(.cols = c("mig_ratio", "herfindahl", "diversity"), .fns = mean)
  # # ) |>
  ggplot(aes(diversity, mig_ratio)) +
  geom_point(aes(color = herfindahl)) +
  geom_path(aes(color = herfindahl, group = towns), alpha = 0.24) +
  geom_hline(yintercept = 1, color = "red", linetype = 2) +
  labs(x = "Diversity based on RCA", y = "Migration ratio") + theme_light(base_size = 6) +
  scico::scale_color_scico("Herfindahl index", palette = "roma", direction = -1) +
  theme(legend.position = c(0.75, 0.9), legend.direction = "horizontal",
        legend.title.position = "top", legend.key.height = unit(1, "mm"),
        legend.key.width = unit(5, 'mm'))

ggsave(
  filename = "sm_finland_migration.png", path = "img/", device = "png",
  width = 3, height = 2.5, bg = "white", dpi = 500, plot = last_plot()
)


df_towns2 |>
  group_by(towns) |>
  summarize(across(c(prop_outmig, prop_inmig, diversity), mean)) |>
  ggplot(aes(prop_outmig, prop_inmig)) +
  geom_point(aes(color = diversity))


df_towns2 |>
  ggplot(aes(eci_eig, rank_mor)) +
  geom_point(aes(color = shannon))

a <- df_towns2 |>
  mutate(ratio = prop_inmig/ prop_outmig) |>
  ggplot(aes(diversity, ratio)) +
  geom_point(aes(size = population_31_dec))# +
  geom_hline(yintercept = 1, color = "red", linetype = 2) +
  scale_color_viridis_c(option = "A")

plotly::ggplotly(a)



#### Regressions ####
library(plm)

df_towns2 <- pdata.frame(df_towns2, index = c("towns", "year"))
head(df_towns2)

frml <- "prop_outmig ~ log10(population_31_dec) + eci_eig + herfindahl + krugman +  indeg + outdeg + betw + lag(eci_eig,1) " # + log10(population_31_dec) * herfindahl

# fix effects
fixed <- plm(frml, data = df_towns2, model = "within", efect = "twoway")
summary(fixed)
# random effects
rand <- plm(frml, data = df_towns2, model = "random") # random intercept, partial pooling

# fixed or random
phtest(fixed, rand) # p is < 0.05, fixed effects preferred


# time fixed effects
fixed_time <- plm(
  prop_outmig ~ diversity + log10(population_31_dec) + eci_mor + eci_svd + eci_eig +
    rank_mor + rank_svd + shannon + indeg + outdeg + betw + herfindahl + krugman + factor(year),
  data = df_towns2,
  model = "within")
summary(fixed_time)
## Do we need time fixed effects? If both p_vals are lower than 0.05, time fixed effects
## should be used
pFtest(fixed_time, fixed) # p < 0.05
plmtest(fixed, c("time"), type = ("bp")) # p < 0.05

twoway <- plm(frml,
  data = df_towns2,
  model = "within", effect = 'twoways')
summary(twoway)
# this is identical as fixed_time, so they are equivalent
pFtest(twoway, fixed) # p < 0.05

## Pooling model?
pool <- plm(frml, data = df_towns2, model = "pooling")
plmtest(pool, type = "bp") # p is < 0.05, suggest there is significant differences
# across units, hence we need a panel model, an OLS will not do.

## Test for cross sectional dependence or contemporaneous correlation:
pcdtest(twoway, test = "lm") # p < 0.05, fail
pcdtest(twoway, test = "cd") # p = 0.04, fail
# p <0.05 suggest cross-sectional dependence, need to correct for this

## Test for serial correlation
pbgtest(twoway)
# p < 0.05, there is serial autocorrelation.

## Testing for stationarity
tseries::adf.test(
  df_towns2 |> filter(!is.na(prop_outmig)) |> pull(prop_outmig),
  k = 2
)
# p < 0.05, no unit roots present (if unit root present, one can take the first-difference)

## Test for heteroskedasticity
lmtest::bptest(
  prop_outmig ~ diversity + log10(population_31_dec) + eci_mor + eci_svd + eci_eig +
    rank_mor + rank_svd + shannon + indeg + outdeg + betw + herfindahl + krugman + factor(towns) + factor(year),
               studentize = FALSE, data = df_towns2)
# p < 0.05 means there is heteroskedasticity. Need to use robust covariance matrix

lmtest::coeftest(
  twoway, vcovHC(twoway, type = "HC4", method = "arellano", cluster = "time")) |>
  broom::tidy() |>
  mutate(p_value = ifelse(p.value < 0.05, "p value < 0.05", "p value > 0.05")) |>
  mutate(term = as_factor(term) |> fct_rev()) |>
  ggplot(aes(estimate, term)) +
  geom_point(aes(color = p_value)) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate+std.error, color = p_value))


#### Figure ideas ####
# a - a map with the response variable prop_outmig
# b - lines showing who complexity change over time (one of the ranks)
# c - regression

library(sf)

fin <- read_sf(
  "data/Finland/Finland map/SuomenHallinnollisetKuntajakopohjaisetAluejaot_2023_10k.gpkg",
  layer = "Kunta")


a <- fin |>
  mutate(towns = case_when(
    nameswe %in% unique(df_towns2$towns) ~ nameswe,
    namefin %in% unique(df_towns2$towns )~ namefin
  )) |>
  left_join(
    df_towns2 |> as_tibble() |>
      group_by(towns) |>
      summarize(prop_outmig = mean(prop_outmig)) |>
      mutate(towns = as.character(towns))
  ) |>  ggplot() +
  geom_sf(aes(fill = prop_outmig), linewidth = 0.01, show.legend = TRUE) +
  scale_fill_viridis_c(name = "Proportion of outmigration") + #ggdark::dark_mode() +
  labs(tag = "A") +
  theme_light(base_size = 6) +
  theme(legend.position = "bottom", legend.title.position = "top",
        legend.key.width = unit(5,"mm"), legend.key.height = unit(1,"mm"))

b <- ggplot(as_tibble(df_towns2), aes(year, eci_eig)) +
  geom_path(aes(group = towns, color = diversity), size = 0.1) +
  scico::scale_color_scico("Diversity based on RCA", palette = "roma") +
  labs(tag = "B", y = "Knowledge complexity", x = 'Year') +
  theme_light(base_size = 6) +
  theme(legend.position = "bottom", legend.title.position = "top",
        legend.key.width = unit(5,"mm"), legend.key.height = unit(1,"mm"))

c <- lmtest::coeftest(
  twoway, vcovHC(twoway, type = "HC4", method = "arellano", cluster = "time")) |>
  broom::tidy() |>
  # beautify names
  mutate(term = case_when(
    term == "log10(population_31_dec)" ~ "Population [log10]",
    term == "eci_eig" ~ "Knowledge complexity",
    term == "indeg" ~ "In-degree",
    term == 'outdeg' ~ "Out-degree",
    term == "betw" ~ "Betweenness",
    term == "herfindahl" ~ "Herfindahl index",
    term == "krugman" ~ "Krugman index"
  )) |>
  mutate(p_value = ifelse(
    p.value > 0.05, "p value > 0.05",
    ifelse(p.value < 0.01, "p value << 0.01", "0.05 > p value > 0.01")) ) |>
  mutate(term = as_factor(term) |> fct_rev(), p_value = as_factor(p_value)) |>
  ggplot(aes(estimate, term)) +
  geom_point(aes(color = p_value)) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate+std.error, color = p_value),
                 width = 0.15) +
  labs(tag = "C", y = "Regression term", x = "Estimate", title = "Proportion of outmigration") +
  scale_color_manual("Significance", values = c("#0066ff", "#ff3300", "grey40")) +
  theme_light(base_size = 6) + theme(legend.position = 'bottom')
c

ggsave(
  filename = "fig4_regression.png", device = "png", path = "paper/figs/",
  plot = (a + b + c), width = 6.5, height = 3, bg = "white", dpi = 500
)


#### left overs: ideas for SM ####

df_towns2 |> as_tibble() |>
  select(starts_with("eci_"), diversity, shannon, krugman, herfindahl) |>
  mutate(eci_svd = log1p(eci_svd)) |>
  GGally::ggpairs()

df_towns2 |> as_tibble() |>
  select(starts_with("rank_"), diversity, shannon, krugman, herfindahl) |>
  GGally::ggpairs()

ggplot(df_towns2 |> as_tibble()) +
  geom_density_2d_filled(aes(prop_outmig, prop_inmig)) +
  geom_abline(intercept = 0, slope = 1)
