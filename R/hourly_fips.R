# need to move these to description
library(devtools)
library(rnoaa)
library(countyweather)
library(ggplot2)
library(dplyr)
library(countyweather)
library(lubridate)
library(stringr)
library(geojsonio)
library(lawn)
library(plyr)
library(tidyr)


## want average hourly data for a particular fips, year, variables, and coverage

fips <- "12086"

#' Get station list for a particular fips
#'
#' This function serves as a wrapper to that function, allowing you to search
#' by FIPS code rather than having to know the latitude and longitude of the
#' center of each county.
#'
#' @param fips A five-digit FIPS county code.
#'
#' @return A dataframe of monitors within a given radius of the
#'    population-weighted center of the county specified by the FIPS code.
#'    This will have the same dataframe format as the output from the
#'    \code{isd_stations_search} function in the \code{rnoaa} package.
#'
#' @note We probably want to use geocodes for this instead.
#'
#' @examples
#' \dontrun{
#' ids <- isd_fips_stations(fips = "12086")
#' }
#'
#' @export
isd_fips_stations <- function(fips){
  census_data <- read.csv(paste0("http://www2.census.gov/geo/docs/reference/",
                                 "cenpop2010/county/CenPop2010_Mean_CO.txt"))
  state <- sprintf("%02d", census_data$STATEFP)
  county <- sprintf("%03d", census_data$COUNTYFP)
  FIPS <- paste0(state,county)

  loc_fips <- which(FIPS == fips)
  lat_FIPS <- census_data[loc_fips, "LATITUDE"]
  lon_FIPS <- census_data[loc_fips, "LONGITUDE"]

  stations <- rnoaa::isd_stations_search(lat = lat_FIPS, lon = lon_FIPS,
                                        radius = 50)
  return(stations)
}

#' Get hourly data for a single monitor
#'
#' This function wraps the \code{isd} function from the \code{rnoaa} package.
#'
#' @param usaf_code A character string with a six-digit [usaf?] code for the
#'    monitor.
#' @param wban_code A character string with a five-digiv [wban?] code for the
#'    monitor.
#' @param year A four-digit numeric giving the year for which to pull data.
#' @param var A character vector listing the weather variables to pull. Choices
#'    include ...
#'
#' @examples
#' \dontrun{
#' ids <- isd_fips_stations(fips = "12086")
#' onest <- int_surface_data(usaf_code = ids$usaf[1], wban_code = ids$wban[1],
#'                           year = 1992, var = c("wind_speed", "temperature"))
#' derp <- int_surface_data(usaf_code = ids$usaf[11], wban_code = ids$wban[11],
#'                          year = year, var = c("wind_speed", "temperature"))
#' }
#'
#' @export
int_surface_data <- function(usaf_code, wban_code, year, var = "all"){
  isd_df <- rnoaa::isd(usaf = usaf_code, wban = wban_code, year = year)$data
  # add date time (suggested by one of the rnoaa package vignette examples for isd())
  isd_df$date_time <- ymd_hm(sprintf("%s %s", as.character(isd_df$date), isd_df$time))
  # select variables

  w_vars <- colnames(isd_df)

  if(length(var) == 1 && var == "all"){
    var <- w_vars[9:length(w_vars)]
    remove <- c("date_time")
    var <- var[!var%in%remove]
  }

  cols <- c("usaf_station", "wban_station", "date_time", "latitude", "longitude")
  subset_vars <- append(cols, var)
  isd_df <- dplyr::select_(isd_df, .dots = subset_vars)
  # change misisng weather data values to NA - it looks like non-signed items are filled
  # with 9 (quality codes), 999 or 9999; signed items are positive filled (+9999 or +99999)
  # ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-format-document.pdf
  isd_df[,var][isd_df[,var] > 900] <- NA

  return(isd_df)
}



# 3. pull data for multiple monitors

isd_monitors_data <- function(fips, year, var = "all"){
  ids <- isd_fips_stations(fips)
  safe_int <- purrr::safely(int_surface_data)


    mult_stations <- mapply(safe_int, usaf_code = ids$usaf, wban_code =
                              ids$wban, year = year, var = var)

  # problem with mapply only allowing one variable in var argument - if
  # var = c("wind_speed", "temperature") it only includes wind_speed, for
  # example


  check_df <- data.frame(st = c(1:length(stations$usaf)), bool = NA)
  for(i in 1:length(stations$usaf)){
    if(length(mult_stations[[i]]$usaf_station) == 0){
      check_df$bool[i] = TRUE
    } else {
      check_df$bool[i] = FALSE
    }
  }

  good_st <- filter(check_df, bool == FALSE)

  st_out_list <- lapply(good_st$st, function(x) mult_stations[[x]])

  st_out_df <- dplyr::bind_rows(st_out)
  return(st_out_df)
}

stationdata <- isd_monitors_data("12086", 1992, var = c("wind_speed",
                                                        "temperature"))

# 4. average across stations

ave_hourly <- function(stationdata){
  averaged <- ddply(stationdata, c("date_time",
                                   "variable"), summarize, mean =
                      mean(value, na.rm = TRUE))
  #(not finished)
}




# for filtering based on coverage (moved from isd_fips_stations())
n_missing <- do.call("rbind", sapply(var, FUN = function(i) sum(is.na(isd_df[,i])),
                                     simplify = FALSE))
n_missing <- as.data.frame(n_missing)
n_missing <- add_rownames(n_missing, "VALUE")
colnames(n_missing) <- c("variable", "n_missing")

n_total <- do.call("rbind", sapply(var, FUN = function(i) nrow(isd_df[,i]),
                                   simplify = FALSE))
n_total <- as.data.frame(n_total)
n_total <- add_rownames(n_total, "VALUE")
colnames(n_total) <- c("variable", "n_total")

df <- full_join(n_missing, n_total, by = "variable")
df <- mutate(df, frac_missing = n_missing/n_total, coverage = 1-(n_missing/n_total))

isd_df <- gather(isd_df, "variable", "value", 1:length(var))
isd_df <- left_join(isd_df, df, by = "variable")
isd_df <- select(isd_df, -n_missing, -n_total, -frac_missing)
isd_df_coverage <- isd_df[isd_df$coverage > frac_coverage,]
isd_df_coverage <- select(isd_df_coverage, -coverage)
