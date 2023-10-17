# Exploring API of Statistics Sweden

#install.packages("pxweb")
library(pxweb)

d <- pxweb_interactive()

'/OV0104/v1/doris/en/ssd/AM/AM0208/AM0208M/YREG60N'


class(d)
names(d) # url, query, data

save(d, file = "data/Sweden/labour.Rda")

## Recovers the table that Carla got from SCB, but now with two more years of data
d$data |> as_tibble()

## This is what I get when using the interactive API explorer:
# Download data
px_data <-
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/en/ssd/AM/AM0208/AM0208M/YREG60N",
            query = "[path to jsonfile]")

# Convert to data.frame
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Get pxweb data comments
px_data_comments <- pxweb_data_comments(px_data)
px_data_comments_df <- as.data.frame(px_data_comments)

# Cite the data as
pxweb_cite(px_data)

############# CITATION #############
#' Statistics Sweden (2023). “Employees (the Swedish Occupational Register) 16-64 years by region
#' of work by region, occupation (SSYK 2012), sex, observations and year.” [Data accessed
#'                                                                          2023-10-13 10:13:59 using pxweb R package 0.16.2],
#' <https://api.scb.se/OV0104/v1/doris/en/ssd/AM/AM0208/AM0208M/YREG60N>.
#'
#' A BibTeX entry for LaTeX users is
#'
#' @Misc{,
#'   title = {Employees (the Swedish Occupational Register) 16-64 years by region of work by region, occupation (SSYK 2012), sex, observations and year},
#'   author = {{Statistics Sweden}},
#'   organization = {Statistics Sweden},
#'   address = {Stockholm, Sweden},
#'   year = {2023},
#'   url = {https://api.scb.se/OV0104/v1/doris/en/ssd/AM/AM0208/AM0208M/YREG60N},
#'   note = {[Data accessed 2023-10-13 10:13:59 using pxweb R package 0.16.2]},
#' }
#'
#'
#'
#'
#' Kindly cite the pxweb R package as follows:
#'
#'   Mans Magnusson, Markus Kainu, Janne Huovari, and Leo Lahti (rOpenGov).  pxweb: R tools for
#' PXWEB API.  URL: http://github.com/ropengov/pxweb
#'
#' A BibTeX entry for LaTeX users is
#'
#' @Misc{,
#'   title = {pxweb: R tools for PX-WEB API},
#'   author = {Mans Magnusson and Markus Kainu and Janne Huovari and Leo Lahti},
#'   year = {2019},
#' }

############# CITATION #############
