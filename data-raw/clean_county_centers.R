library(dplyr)
library(stringi)

## Pull in data from the U.S. Census on county population centers, names, and FIPS codes
census_csv <- paste0("http://www2.census.gov/geo/docs/reference/cenpop2010/",
                     "county/CenPop2010_Mean_CO.txt")
census_data <- utils::read.csv(census_csv)
census_data$COUNAME <- stri_trans_general(census_data$COUNAME, "latin-ascii")
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
  dplyr::rename(latitude = LATITUDE, longitude = LONGITUDE) %>%
  dplyr::mutate(region = as.numeric(fips)) %>%
  dplyr::filter(!(substring(fips, 1, 1) == 7))
county_centers <- census_data
devtools::use_data(county_centers, overwrite = TRUE)
