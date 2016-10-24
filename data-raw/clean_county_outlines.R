library(rgdal)
library(stringi)

# Download U.S. Census 2010 Cartographic Boundary Shapefiles - Counties from
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
# download gz_2010_us_050_00_500k.zip

# setwd("~/Desktop/gz_2010_us_050_00_500k")

shp <- rgdal::readOGR(dsn = ".", layer = "gz_2010_us_050_00_500k")

shp@data <- dplyr::mutate_(shp@data, STATE = ~ as.character(STATE))
shp@data <- dplyr::mutate_(shp@data, COUNTY = ~ as.character(COUNTY))
shp@data$fips <- paste0(shp@data$STATE, shp@data$COUNTY)

county_outlines <- shp
county_outlines$NAME <- factor(stringi::stri_trans_general(county_outlines$NAME,
                                                    "latin-ascii"))

devtools::use_data(county_outlines, overwrite = TRUE)
