library(dplyr)

# download the American FactFinder 2010 Summary File1 (SF1)
# (following instructions here: https://ask.census.gov/faq.php?id=5000&faqId=7825)
# more about the dataset: http://www.census.gov/prod/cen2010/doc/sf1.pdf

# land area is in square meters

# setwd("~/Desktop")
area <- read.csv("DEC_10_SF1_G001.csv", header = TRUE, skip = 1)

# select state and county FIPS and land area columns
cols <- c(13:14, 67)
area <- area[,cols]
colnames(area) <- c("state_fips", "county_fips", "land_area")

area$state_fips <- sprintf("%02d", area$state_fips)
area$county_fips <- sprintf("%03d", area$county_fips)
area$fips <- paste0(area$state_fips, area$county_fips)

area <- dplyr::mutate_(area, land_area_km = ~ (land_area / 1000000)) %>%
  dplyr::mutate_(county_radius = ~ sqrt(land_area_km / pi)) %>%
  dplyr::select_(.dots = c("fips", "county_radius"))

county_radius <- area
devtools::use_data(county_radius, overwrite = TRUE)
