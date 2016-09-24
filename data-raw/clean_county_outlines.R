library(rgdal)

# Download U.S. Census 2010 Cartographic Boundary Shapefiles - Counties from
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
# download cb_2015_us_county_500k.zip

# setwd("~/Desktop/cb_2015_us_county_500k")

shp <- readOGR(dsn = ".", layer = "cb_2015_us_county_500k")

shp@data <- dplyr::mutate_(shp@data, STATEFP = ~ as.character(STATEFP))
shp@data <- dplyr::mutate_(shp@data, COUNTYFP = ~ as.character(COUNTYFP))
shp@data$fips <- paste0(shp@data$STATEFP, shp@data$COUNTYFP)

county_outlines <- shp

devtools::use_data(county_outlines, overwrite = TRUE)
