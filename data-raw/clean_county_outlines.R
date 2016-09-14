library(rgdal)
library(dplyr)
library(raster)

# Download U.S. Census 2015 Cartographic Boundary Shapefiles - Counties from
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
# download cb_2015_us_county_20m.zip

mapdata <- shapefile("~/Desktop/cb_2015_us_county_20m/cb_2015_us_county_20m.shp")

mapdata@data <- mutate(mapdata@data, STATEFP = as.character(STATEFP))
mapdata@data <- mutate(mapdata@data, COUNTYFP = as.character(COUNTYFP))
mapdata@data$fips <- paste0(mapdata@data$STATEFP, mapdata@data$COUNTYFP)

county_outlines <- mapdata

devtools::use_data(county_outlines, overwrite = TRUE)
