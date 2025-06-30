library(tidyverse)
library(tictoc)
library(fs)

#### Read data ####\
# load onet
load("data/onet.Rda") # 34MB
# this dataset only has 1 digit municipality or 4 digit national
dat <- readxl::read_excel("data/Norway/Norway_Occupations.xlsx", sheet = 1)
dat

## alternatives
dat <- read_csv(file = "https://data.ssb.no/api/v0/dataset/44631.csv?lang=en")

skimr::skim(dat)

dat$region |> unique()


#### Norwegian API ####
install.packages('klassR')
library(klassR)

opts <- ListKlass(language = "en")
opts <- opts |> as_tibble()
opts |> print(n=150)

occ <- GetKlass(7, language = "en")


#### PxWeb ####
#### This doesn't work, returns >900 regions, but only 10 occupations.
library(pxweb)

d <- pxweb_interactive()


class(d)
names(d) # url, query, data

save(d, file = "data/Norway/labour.Rda")

## Recovers the table that Carla got from SCB, but now with two more years of data
d$data |> as_tibble()

## This is what I get when using the interactive API explorer:
## # Download data
px_data <-
  pxweb_get(url = "http://data.ssb.no/api/v0/en/table/al/al06/regsys/LonnBostYrkKjAld",
            query = "[path to jsonfile]")

# Convert to data.frame
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Get pxweb data comments
px_data_comments <- pxweb_data_comments(px_data)
px_data_comments_df <- as.data.frame(px_data_comments)

# Cite the data as
pxweb_cite(px_data)

############# CITATION #############
#' Statistics Norway (2024). “11619: Employed persons. 4th quarter, by region, sex, age, occupation,
#' contents and year.” [Data accessed 2024-03-25 15:28:46 using pxweb R package 0.16.2],
#' <http://data.ssb.no/api/v0/en/table/al/al06/regsys/LonnBostYrkKjAld>.
#'
#' A BibTeX entry for LaTeX users is
#'
#' @Misc{,
#'   title = {11619: Employed persons. 4th quarter, by region, sex, age, occupation, contents and year},
#'   author = {{Statistics Norway}},
#'   organization = {Statistics Norway},
#'   address = {Oslo, Norway},
#'   year = {2024},
#'   url = {http://data.ssb.no/api/v0/en/table/al/al06/regsys/LonnBostYrkKjAld},
#'   note = {[Data accessed 2024-03-25 15:28:46 using pxweb R package 0.16.2]},
#' }
#'
#'
#'
#'
#' Kindly cite the pxweb R package as follows:
#'
#'   Mans Magnusson, Markus Kainu, Janne Huovari, and Leo Lahti (rOpenGov).  pxweb: R tools for PXWEB API.
#' URL: http://github.com/ropengov/pxweb
#'
#' A BibTeX entry for LaTeX users is
#'
#' @Misc{,
#'   title = {pxweb: R tools for PX-WEB API},
#'   author = {Mans Magnusson and Markus Kainu and Janne Huovari and Leo Lahti},
#'   year = {2019},
#' }

############# CITATION #############
