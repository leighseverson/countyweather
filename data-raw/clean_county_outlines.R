library(rgdal)
library(dplyr)
library(raster)

# Download U.S. Census 2010 Cartographic Boundary Shapefiles - Counties from
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
# download gz_2010_us_050_00_20m.zip

#setwd("~/Desktop/gz_2010_us_050_00_20m")

mapdata <- readOGR("gz_2010_us_050_00_20m.shp", layer = "gz_2010_us_050_00_20m")

mapdata@data <- mutate(mapdata@data, STATE = as.character(STATE))
mapdata@data <- mutate(mapdata@data, COUNTY = as.character(COUNTY))
mapdata@data$fips <- paste0(mapdata@data$STATE, mapdata@data$COUNTY)

codes <- countyweather::county_centers
fips <- codes$fips

extractCoords <- function(sp.df){
  results <- list()
  for(i in 1:length(sp.df@polygons[[1]]@Polygons))
  {
    results[[i]] <- sp.df@polygons[[1]]@Polygons[[i]]@coords
  }
  results <- Reduce(rbind, results)
  results <- as.data.frame(results)
  results
}

fp <- fips[1:75]

fp <- fips

list <- vector("list", length(fp))

for (i in 1:length(fp)){
  coords <- extractCoords(mapdata[mapdata$fips == fp[i],])
  coords$FIPS <- fp[i]

  while(i < length(fp)){
    list[[i]] <- coords
    i <- i + 1
  }
}

county_outlines <- do.call(rbind, list)

devtools::use_data(county_outlines, overwrite = TRUE)

