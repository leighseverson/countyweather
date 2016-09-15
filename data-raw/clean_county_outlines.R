library(rgdal)
library(dplyr)
library(raster)

# Download U.S. Census 2015 Cartographic Boundary Shapefiles - Counties from
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
# download cb_2015_us_county_20m.zip

mapdata <- shapefile("~/Desktop/cb_2015_us_county_20m/cb_2015_us_county_20m.shp")

mapdata@data <- dplyr::mutate_(mapdata@data, STATEFP = ~ as.character(STATEFP))
mapdata@data <- dplyr::mutate_(mapdata@data, COUNTYFP = ~ as.character(COUNTYFP))
mapdata@data$fips <- paste0(mapdata@data$STATEFP, mapdata@data$COUNTYFP)



miamidade <- mapdata[mapdata$fips == "12086",]

plot(miamidade)

miami_poly <- miamidade@polygons[[1]]@Polygons[[1]]@coords
miami_poly <- as.data.frame(miami_poly)




devtools::use_data(county_outlines, overwrite = TRUE)

