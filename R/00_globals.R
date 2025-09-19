# ---- globals, packages, helpers ------------------------------------------

#packages
suppressPackageStartupMessages({
  library(tidycensus)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(forcats)
})

#parameters
YEARS  <- 2006:2017
STATES <- c("CT", "FL")

#set Census API key once (uncomment & insert your key):
#tidycensus::census_api_key("API_KEY", install = TRUE)

#Inflation helpers (to 2017 dollars; matches your original factors)
inflate_to_2017 <- function(hincp, year) {
  mult <- dplyr::case_when(
    year == 2006 ~ 1.2185,
    year == 2007 ~ 1.1848,
    year == 2008 ~ 1.1409,
    year == 2009 ~ 1.1449,
    year == 2010 ~ 1.1266,
    year == 2011 ~ 1.0918,
    year == 2012 ~ 1.069,
    year == 2013 ~ 1.0535,
    year == 2014 ~ 1.036,
    year == 2015 ~ 1.0345,
    year == 2016 ~ 1.0213,
    year == 2017 ~ 1.0000,
    TRUE ~ NA_real_
  )
  hincp * mult
}

#seat-law threshold by state & year
seat_threshold <- function(st, year) {
  if (st == "CT" && year <  2005) return(3L)
  if (st == "CT" && year >= 2005) return(6L)
  if (st == "FL" && year <= 2013) return(3L)
  if (st == "FL" && year >= 2014) return(5L)
  NA_integer_
}