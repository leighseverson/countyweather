options(noaakey = "PbGEAVHwjtNNuqVxSYLKMFBvQLHCvAZq")

library(ggplot2)
library(dplyr)
library(rgdal)
library(rnoaa)
library(roxygen2)

#' Count of weather stations per county.
#'
#' \code{stationspercounty} returns a plot showing the number of GHCND weather
#' stations for each U.S. county present in its arguments.
#'
#' A NOAA Token is required to use this function, which interacts with the NCDC
#' API. Request a Token from here: \url{http://www.ncdc.noaa.gov/cdo-web/token}.
#' Then run the code \code{options(noaakey = "your key")} before using this
#' function.
#'
#' @param yourvector A vector of U.S. FIPS codes in numberic or factor format.
#' @examples
#'  vec7 <- c(36081, 36085, 36087, 36119, 40017)
#'  stationspercounty(vec7)
stationspercounty <- function(yourvector){
  vec <- as.data.frame(yourvector)
  vec <- mutate(vec, FIPS = "FIPS:")
  for(i in 1:length(vec$FIPS)){
    vec$FIPS[i] <- gsub("$", vec$yourvector[i], vec$FIPS[i])
  }
  for(i in 1:length(vec$FIPS)){
    df <- rnoaa::ncdc_stations(datasetid = 'GHCND', locationid = vec$FIPS[i],
                               limit = 1000)
    df <- df$data
    df$mindate <- as.factor(df$mindate)
    df$mindate <- as.Date(df$mindate)
    df$maxdate <- as.factor(df$maxdate)
    df$maxdate <- as.Date(df$maxdate)
    max <- subset(df, maxdate < "2012-12-31")
    min <- subset(df, mindate > "1999-01-01")
    df <- df[!(df$id %in% max$is), ]
    df <- df[!(df$id %in% min$id), ]
    coverage <- subset(df, datacoverage < 0.9)
    df <- df[!(df$id %in% coverage$id), ]
    df <- as.data.frame(df)
    df <- mutate(df, fips_code = vec$FIPS[i])

    if(i ==1){
      stationinfo <- df
    } else {
      stationinfo <- rbind(stationinfo, df)
    }
  }
  stationinfo$fips_code <- gsub("FIPS:", "", stationinfo$fips_code)
  plot <- ggplot2::ggplot(data = stationinfo, aes(stationinfo$fips_code)) +
    geom_bar(stat = "count") + labs(x = "county") + theme_linedraw()
  print(plot)
}

vec7 <- c(36081, 36085, 36087, 36119, 40017)
stationspercounty(vec7)

devtools::document()

?stationspercounty

# limit of 1000 error
# will this still work when there are missing stations? - try a fake fips
# or try all of our counties/all us counties
# better way to display count than a plot - df with county and number of station
# columns

# another function - remove or do by distance
