library(dplyr)
library(stringi)
library(lazyeval)

## Pull in data from the U.S. Census on county population centers, names, and FIPS codes
# population-weighted
census_csv <- paste0("http://www2.census.gov/geo/docs/reference/cenpop2010/",
                     "county/CenPop2010_Mean_CO.txt")
census_data <- utils::read.csv(census_csv)
census_data$COUNAME <- stringi::stri_trans_general(census_data$COUNAME, "latin-ascii")
census_data$COUNTYFP <- sprintf("%03d", census_data$COUNTYFP)
census_data <- dplyr::mutate(census_data, choro_fips = paste0(census_data$STATEFP,
                                                              census_data$COUNTYFP))

census_data$state_long <- sprintf("%02d", census_data$STATEFP)
census_data$fips <- paste0(census_data$state_long, census_data$COUNTYFP)
census_data$cname <- paste0(census_data$COUNAME, " County, ")
census_data$name <- paste0(census_data$cname, census_data$STNAME)
census_data <- dplyr::select_(census_data, .dots = c("-cname", "-state_long",
                                                     "-COUNAME", "-STNAME",
                                                     "-STATEFP", "-COUNTYFP",
                                                     "-POPULATION", "-choro_fips")) %>%
  dplyr::rename_(latitude = ~ LATITUDE, longitude = ~ LONGITUDE) %>%
  dplyr::mutate_(region = ~ as.numeric(fips)) %>%
  dplyr::filter_(!(substring(fips, 1, 1) == 7))

## https://www.census.gov/geo/maps-data/data/gazetteer2010.html
# geographic center

setwd("~/Downloads")
census_data2 <- utils::read.table("Gaz_counties_national.txt", header = TRUE,
                                  sep = "\t", quote = "", comment.char = "")
census_data2 <- as_data_frame(census_data2)
census_data2$NAME <- stringi::stri_trans_general(census_data2$NAME, "latin-ascii")
census_data2$GEOID <- sprintf("%05d", census_data2$GEOID)

census_data2 <- dplyr::mutate_(census_data2, USPS = ~ as.character(USPS)) %>%
  dplyr::select_(.dots = c("USPS", "GEOID", "NAME", "INTPTLAT", "INTPTLONG")) %>%
  dplyr::rename_(state = ~ USPS, fips = ~ GEOID, name = ~ NAME,
                 latitude = ~ INTPTLAT, longitude = ~ INTPTLONG)

# remove PR from df (don't have shapefile data)

filter_out <- lazyeval::interp(~ which_column != "PR", which_column = as.name("state"))
census_data2 <- dplyr::filter_(census_data2, filter_out)

county_names <- dplyr::select_(census_data, .dots = c("name", "fips"))
census_data2 <- dplyr::left_join(census_data2, county_names, by = "fips")
census_data2 <- dplyr::select_(census_data2, .dots = c("state", "fips", "name.y",
                                                       "latitude", "longitude")) %>%
  dplyr::rename_(name = ~ name.y) %>%
  dplyr::mutate_(region = ~ as.numeric(fips)) %>%
  dplyr::filter_(!(substring(fips, 1, 1) == 7))

county_centers <- census_data2
devtools::use_data(county_centers, overwrite = TRUE)
