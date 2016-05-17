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
isd_fips_stations <- function(fips, verbose = TRUE){
  census_data <- read.csv(paste0("http://www2.census.gov/geo/docs/reference/",
                                 "cenpop2010/county/CenPop2010_Mean_CO.txt"))
  state <- sprintf("%02d", census_data$STATEFP)
  county <- sprintf("%03d", census_data$COUNTYFP)
  FIPS <- paste0(state,county)

  loc_fips <- which(FIPS == fips)
  lat_FIPS <- census_data[loc_fips, "LATITUDE"]
  lon_FIPS <- census_data[loc_fips, "LONGITUDE"]

  if(verbose) {
    print(paste0("Getting hourly weather monitors for ",
                 census_data[loc_fips, "COUNAME"], ", ",
                 census_data[loc_fips, "STNAME"]))
  }

  quiet_station_search <- purrr::quietly(rnoaa::isd_stations_search)
  stations <- quiet_station_search(lat = lat_FIPS, lon = lon_FIPS,
                                   radius = 50)$result

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
#' @return This function returns the same type of dataframe as that returned
#'    by the \code{isd} function from the \code{rnoaa} package, but with the
#'    dataframe limited to the selected weather variables.
#'
#' @references
#' For more information on this dataset, see
#' \url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-format-document.pdf}.
#'
#' @examples
#' \dontrun{
#' ids <- isd_fips_stations(fips = "12086")
#' airport_station <- int_surface_data(usaf_code = ids$usaf[1],
#'                                     wban_code = ids$wban[1],
#'                                     year = 1992,
#'                                     var = c("wind_speed", "temperature"))
#' kendall_station <- int_surface_data(usaf_code = ids$usaf[11],
#'                                     wban_code = ids$wban[11],
#'                                     year = 1992,
#'                                     var = c("wind_speed", "temperature"))
#' }
#'
#' @export
int_surface_data <- function(usaf_code, wban_code, year, var = "all"){
  quiet_isd <- purrr::quietly(rnoaa::isd)
  isd_df <- quiet_isd(usaf = usaf_code, wban = wban_code, year = year)
  isd_df <- isd_df$result$data

  # select variables if `var` isn't "all"
  if(length(var) == 1 && var == "all"){
    w_vars <- colnames(isd_df)
    var <- w_vars[9:length(w_vars)]
  }

  # add date time (suggested by one of the rnoaa package vignette examples for isd())
  isd_df$date_time <- lubridate::ymd_hm(sprintf("%s %s",
                                                as.character(isd_df$date),
                                                isd_df$time))
  cols <- c("usaf_station", "wban_station", "date_time",
            "latitude", "longitude")
  subset_vars <- append(cols, var)
  isd_df <- dplyr::select_(isd_df, .dots = subset_vars)

  # change misisng numerical weather data values to NA - it looks like non-signed items are filled
  # with 9 (quality codes), 999 or 9999; signed items are positive filled (+9999 or +99999)
  # ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-format-document.pdf
  na_code_vars <- colnames(isd_df)[apply(isd_df, 2, max) %in%
                                 c(999, 999.9, 9999, 9999.9, 99999, 99999.9)]
  if(length(na_code_vars) > 0){
    for(na_var in na_code_vars){
      isd_df[isd_df[ , na_var] == max(isd_df[ , na_var]), na_var] <- NA
    }
  }

  return(isd_df)
}

#' Pull hourly data for multiple monitors
#'
#' @inheritParams isd_fips_stations
#' @inheritParams int_surface_data
#'
#' @examples
#' \dontrun{
#' stationdata <- isd_monitors_data(fips = "12086", year = 1992,
#'                                  var = c("wind_speed", "temperature"))
#' }
#'
#' @export
isd_monitors_data <- function(fips, year, var = "all"){
  ids <- isd_fips_stations(fips)
  safe_int <- purrr::safely(int_surface_data)

  mult_stations <- mapply(safe_int, usaf_code = ids$usaf,
                          wban_code = ids$wban,
                          year = year, var = list(var = var))

  good_st <- sapply(mult_stations, function(x) !is.null(dim(x)))
  st_out_list <- lapply(which(good_st), function(x) mult_stations[[x]])
  st_out_df <- dplyr::bind_rows(st_out_list)

  return(st_out_df)
}

#' Average across hourly stations
#'
#' @examples
#' \dontrun{
#' average_data <- ave_hourly(stationdata)
#' aug_ave <- with(average_data,
#' subset(average_data, average_data$date_time > as.POSIXct('1992-08-01 00:00:00') &
#' average_data$date_time < as.POSIXct('1992-08-31 00:00:00')))
#' ggplot(aug_ave, aes(x = date_time, y = mean)) + geom_line() + theme_minimal()
#' }
ave_hourly <- function(stationdata){
  averaged <- ddply(stationdata, c("date_time"), summarize, mean =
                      mean(wind_speed, na.rm = TRUE))
  #(not finished)
}


# for filtering based on coverage (moved from isd_fips_stations())
# n_missing <- do.call("rbind", sapply(var, FUN = function(i) sum(is.na(isd_df[,i])),
#                                      simplify = FALSE))
# n_missing <- as.data.frame(n_missing)
# n_missing <- add_rownames(n_missing, "VALUE")
# colnames(n_missing) <- c("variable", "n_missing")
#
# n_total <- do.call("rbind", sapply(var, FUN = function(i) nrow(isd_df[,i]),
#                                    simplify = FALSE))
# n_total <- as.data.frame(n_total)
# n_total <- add_rownames(n_total, "VALUE")
# colnames(n_total) <- c("variable", "n_total")
#
# df <- full_join(n_missing, n_total, by = "variable")
# df <- mutate(df, frac_missing = n_missing/n_total, coverage = 1-(n_missing/n_total))
#
# isd_df <- gather(isd_df, "variable", "value", 1:length(var))
# isd_df <- left_join(isd_df, df, by = "variable")
# isd_df <- select(isd_df, -n_missing, -n_total, -frac_missing)
# isd_df_coverage <- isd_df[isd_df$coverage > frac_coverage,]
# isd_df_coverage <- select(isd_df_coverage, -coverage)
