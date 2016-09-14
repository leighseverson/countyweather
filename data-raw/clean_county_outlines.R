library(rgdal)

# Download U.S. Census 2015 Cartographic Boundary Shapefiles - Counties from
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
# download cb_2015_us_county_20m.zip

# setwd("~/Desktop/cb_2015_us_county_20m")
mapdata <- readOGR("cb_2015_us_county_20m.shp", layer = "cb_2015_us_county_20m")

mapdata@data <- mutate(mapdata@data, STATEFP = as.character(STATEFP))
mapdata@data <- mutate(mapdata@data, COUNTYFP = as.character(COUNTYFP))
mapdata@data$fips <- paste0(mapdata@data$STATEFP, mapdata@data$COUNTYFP)

devtools::use_data(mapdata, overwrite = TRUE)
