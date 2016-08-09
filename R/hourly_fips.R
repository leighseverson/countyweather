#' Return average hourly weather data and a plot showing the location of weather
#' stations for a particular county.
#'
#' Given a particular county FIPS code, this function returns a list with two
#' elements: "data", a dataframe of hourly average weather values, and "plot",
#' a plot showing the location of weather stations contributing to the average
#' weather in "data".
#'
#' @inheritParams hourly_fips_df
#'
#' @return A list with two elements. The first element (\code{hourly_data}) is a
#'    dataframe of daily weather data averaged across multiple stations, as well
#'    as columns (\code{"var"_reporting}) for each weather variable showing the
#'    number of stations contributing to the average for that variable for that
#'    hour The second element (\code{hourly_map}) is a plot showing points for all
#'    weather stations for a particular county satisfying the conditions present
#'    in \code{hourly_fips}'s arguments (year, coverage, and/or var).
#'
#' @export
hourly_fips <- function(fips, year, var = c("wind_direction", "wind_speed",
                                            "ceiling_height", "visibility_distance",
                                            "temperature", "temperature_dewpoint",
                                            "air_pressure"),
                        coverage = NULL, radius = 50){

  weather_data <- hourly_fips_df(fips = fips, year = year, var = var,
                                 coverage = coverage, radius = radius)
  station_map <- hourly_stationmap

  list <- list("hourly_data" = weather_data$averaged,
               "hourly_map" = station_map)
  return(list)

}

#' Return average hourly weather data for a particular county.
#'
#' \code{hourly_fips_df} returns a dataframe of average daily weather values
#' for a particular county, year, and/or specified "coverage."
#'
#' This function serves as a wrapper to several functions from the \code{rnoaa}
#' package, which provides weather data from all relevant stations in a county.
#' This function filters and averages across NOAA ISD/ISH stations based on
#' user-specified coverage specifications.
#'
#' @param fips A character string giving the five-digit U.S. FIPS county code
#'    of the county for which the user wants to pull weather data.
#' @param year The year for which you want to pull hourly data. \code{year} can
#'    be in the range from 1901 to the current year.
#' @param var A character vector specifying desired weather variables. For
#'    example, var = c("wind_speed", "temperature"). (Optional. \code{var}
#'    includes all possible weather variables by default, which include
#'    \code{c("wind_direction", "wind_speed", "ceiling_height",
#'    "visibility_distance", "temperature", "temperature_dewpoint",
#'    "air_pressure")}. Alternatively, you can specify var = "all" to include
#'    additional flag and quality codes.
#' @param radius A numeric value giving the radius, in kilometers from the
#'    county's population-weighted center, within which to pull weather
#'    monitors. \code{radius} defaults to 50.
#' @param coverage A numeric value in the range of 0 to 1 that specifies
#'    the desired percentage coverage for the weather variable (i.e., what
#'    percent of each weather variable must be non-missing to include data from
#'    a monitor when calculating daily values averaged across monitors.)
#'    (Optional.)
#'
#' @return A dataframe of hourly weather data averaged across multiple stations,
#'    as well as columns (\code{"var"_reporting}) for each weather variable
#'    showing the number of stations contributing to the average for that
#'    variable for each hour.
#'
#' @references For more information on this dataset and available weather and
#' flag/quality variables, see
#' \url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-format-document.pdf}.
#'
#' @examples
#' \dontrun{
#' df <- hourly_fips_df(fips = "12086", year = 1992, var = c("wind_speed",
#'                      "temperature"))
#' }
#'
#' @export
hourly_fips_df <- function(fips, year,
                           var = c("wind_direction", "wind_speed",
                                   "ceiling_height", "visibility_distance",
                                   "temperature", "temperature_dewpoint",
                                   "air_pressure"), radius = 50, coverage = NULL){

  # hourly data for multiple monitors
  data <- isd_monitors_data(fips = fips, year = year, var = var, radius =
                               radius)

  # if coverage is not null, filter stations
  if(!purrr::is_null(coverage)){
    data <- filter_hourly(hourly_data = data, coverage = coverage, var = var)
  }

  # station meta data for one county
  station_metadata <- isd_fips_stations(fips)

  # average hourly across multiple stations
  averaged <- ave_hourly(data)

  out <- list(averaged = averaged, stations = station_metadata)
  return(out)
}

#' Get station list for a particular US county
#'
#' This function serves as a wrapper to the \code{isd_stations_search} function
#' in the \code{rnoaa} package, allowing you to search by FIPS code rather than
#' having to know the latitude and longitude of the center of each county.
#'
#' @param fips A five-digit FIPS county code.
#' @param verbose TRUE / FALSE to indicate if you want the function to print
#'    out the name of the county it's processing
#' @param radius A numeric value giving the radius, in kilometers from the
#'    county's population-weighted center, within which to pull weather
#'    monitors.
#'
#' @return A dataframe of monitors within the given radius of the
#'    population-weighted center of the county specified by the FIPS code.
#'    This will have the same dataframe format as the output from the
#'    \code{isd_stations_search} function in the \code{rnoaa} package.
#'
#' @examples
#' \dontrun{
#' ids <- isd_fips_stations(fips = "12086")
#' }
isd_fips_stations <- function(fips, verbose = TRUE, radius = 50){
  census_data <- countyweather::county_centers
  loc_fips <- which(census_data$fips == fips)
  lat_fips <- census_data[loc_fips, "latitude"]
  lon_fips <- census_data[loc_fips, "longitude"]

  if(verbose) {
    print(paste0("Getting hourly weather monitors for ",
                 census_data[loc_fips, "name"]))
  }

  quiet_station_search <- purrr::quietly(rnoaa::isd_stations_search)
  stations <- quiet_station_search(lat = lat_fips, lon = lon_fips,
                                   radius = radius)$result

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
int_surface_data <- function(usaf_code, wban_code, year,
                             var = c("wind_direction", "wind_speed",
                                     "ceiling_height", "visibility_distance",
                                     "temperature", "temperature_dewpoint",
                                     "air_pressure")){
  quiet_isd <- purrr::quietly(rnoaa::isd)
  isd_df <- quiet_isd(usaf = usaf_code, wban = wban_code, year = year)
  isd_df <- isd_df$result$data

#  year <- c(1999, 2000)
#  good_st <- sapply(mult_stations, function(x) !is.null(dim(x)))
#  if(sum(good_st) > 0){
#    st_out_list <- lapply(which(good_st), function(x) mult_stations[[x]])
#    st_out_df <- dplyr::bind_rows(st_out_list)
#  } else(
#    stop("None of the stations had available data.")
#  )

# goal is to replace the last line of code (isd_df <- ) with code that binds
# multiple years together. Trying to avoid a loop but that might be the move.

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

  na_code_vars <- colnames(isd_df)[apply(isd_df, 2, max) %in%
                                 c(99.9, 999, 999.9, 9999, 9999.9, 99999, 999999)]

  for(na_var in na_code_vars){
    isd_df[[na_var]] <- as.numeric(isd_df[[na_var]])
  }

  if(length(na_code_vars) > 0){
    for(na_var in na_code_vars){
      isd_df[isd_df[ , na_var] == max(isd_df[ , na_var]), na_var] <- NA
    }
  }

  return(isd_df)
}

#' Pull hourly data for multiple monitors
#'
#' Pull all available data for all weather monitors within a certain radius of
#' the population-weighted center of a US county, based on the county's FIPS
#' code.
#'
#' @inheritParams isd_fips_stations
#' @inheritParams int_surface_data
#'
#' @examples
#' \dontrun{
#' stationdata <- isd_monitors_data(fips = "12086", year = 1992,
#'                                  var = c("wind_speed", "temperature"))
#' ggplot(stationdata, aes(x = date_time, y = wind_speed)) +
#'    geom_point(alpha = 0.5, size = 0.2) +
#'    facet_wrap(~ usaf_station, ncol = 1)
#' }
isd_monitors_data <- function(fips, year, var = c("wind_direction", "wind_speed",
                                                  "ceiling_height", "visibility_distance",
                                                  "temperature", "temperature_dewpoint",
                                                  "air_pressure"),
                              radius = 50){
  ids <- isd_fips_stations(fips, radius = radius)

  safe_int <- purrr::safely(int_surface_data)
  mult_stations <- mapply(safe_int, usaf_code = ids$usaf,
                          wban_code = ids$wban,
                          year = year, var = list(var = var))

  good_st <- sapply(mult_stations, function(x) !is.null(dim(x)))
  if(sum(good_st) > 0){
    st_out_list <- lapply(which(good_st), function(x) mult_stations[[x]])
    st_out_list <- lapply(st_out_list, function(x){
      x$usaf_station <- as.numeric(x$usaf_station)
      x$wban_station <- as.numeric(x$wban_station)
      return(x)
    })
    st_out_df <- dplyr::bind_rows(st_out_list)
  } else(
    stop("None of the stations had available data.")
  )

  # filter so ids stations match with filtered df's stations
  # out = list: data and stations
  # want to be able to access station metadata later for mapping, etc.

  return(st_out_df)
}

#' Average hourly weather data across multiple stations.
#'
#' \code{ave_hourly} returns a dataframe with hourly weather averaged across
#' stations, as well as columns showing the number of stations
#'
#' \code{ave_hourly} returns a dataframe with hourly weather averaged across
#' stations, as well as columns showing the number of stations contributing to
#' average for each variable and each hour.
#'
#' @param hourly_data A dataframe with hourly weather observations. This
#' dataframe is returned from the function \code{isd_monitors_data}.
#'
#' @importFrom dplyr %>%
ave_hourly <- function(hourly_data){

  df <- dplyr::mutate_(hourly_data, id = ~ paste0(usaf_station, wban_station))
  df <- dplyr::select_(df, .dots = c("-usaf_station", "-wban_station",
                                     "-latitude", "-longitude"))

  averaged_data <- tidyr::gather(df, key, value, -id, -date_time) %>%
    dplyr::group_by_(~ date_time, ~ key) %>%
    dplyr::summarize_(mean = ~ mean(value, na.rm = TRUE)) %>%
    tidyr::spread_(key_col = "key", value_col = "mean")

  n_reporting <- tidyr::gather(df, key, value, -id, -date_time) %>%
    dplyr::group_by_(~ date_time, ~ key) %>%
    dplyr::summarise_(n_reporting = ~ sum(!is.na(value))) %>%
    dplyr::mutate_(key = ~ paste(key, "reporting", sep = "_")) %>%
    tidyr::spread_(key_col = "key", value_col = "n_reporting")

  averaged_data <- dplyr::left_join(averaged_data, n_reporting,
                                    by = "date_time")
  return(averaged_data)
}

#' Filter NOAA ISD stations based on "coverage" requirements.
#'
#' \code{filter_hourly} filters available weather variables based on a specified
#' minimum coverage (i.e., percent non-missing hourly observations).
#'
#' @param hourly_data A \code{isd_monitors_data} dataframe
#' @param coverage A numeric value in the range of 0 to 1 that specifies the
#' desired percentage coverage for each weather variable (i.e., what percent
#' of each weather variable must be non-missing to include the data from a
#' station when calculating hourly values averaged across stations). (Optional).
#' @param var A character vector specifying desired weather variables. For
#'    example, var = c("wind_speed", "temperature"). (Optional. \code{var}
#'    includes all possible weather variables by default, which include
#'    \code{c("wind_direction", "wind_speed", "ceiling_height",
#'    "visibility_distance", "temperature", "temperature_dewpoint",
#'    "air_pressure")}. Alternatively, you can specify var = "all" to include
#'    additional flag and quality codes.
#'
#' @return a \code{dataframe} with stations that meet the specified coverage
#' requirements for weather variables included in the datafrome present in
#' this function's arguments.
#'
#' @importFrom dplyr %>%
filter_hourly <- function(hourly_data, coverage,
                          var = c("wind_direction", "wind_speed",
                                  "ceiling_height", "visibility_distance",
                                  "temperature", "temperature_dewpoint",
                                  "air_pressure")){

  df <- hourly_data %>%
    tidyr::unite_(station, usaf_station, wban_station, sep = "-") %>%
    dplyr::select_(-date_time, -latitude, -longitude) %>%
    tidyr::gather_(key, value, -station) %>%
    dplyr::group_by_(station, key) %>%
    dplyr::summarize(coverage = ~ mean(!is.na(value)))

  filtered <- hourly_data %>%
    tidyr::unite_(station, usaf_station, wban_station, sep = "-") %>%
    dplyr::select_(-latitude, -longitude) %>%
    tidyr::gather_(key, value, -station, -date_time) %>%
    dplyr::left_join(df, by = c("station", "key")) %>%
    dplyr::filter_(~ coverage > 0.80) %>%
    dplyr::group_by_(date_time, key)

  df2 <- filtered %>%
    dplyr::summarize_(n_reporting = ~ sum(!is.na(value))) %>%
    dplyr::mutate_(key = ~ paste(key, "reporting", sep = "_")) %>%
    tidyr::spread_(key_col = "key", value_col = "n_reporting")

  df3 <- filtered %>%
    dplyr::summarize(value = ~ mean(value, na.rm = TRUE)) %>%
    tidyr::spread_(key_col = "key", value_col = "value")

  out <- dplyr::full_join(df3, df2, by = "date_time")

  return(out)
}

#' Plot hourly weather stations for a particular county
#'
#' @param fips
#' @param hourly_data
#' @param point_color
#' @param point_size
#'
#' @return A plot showing points for all weather stations for a particular
#'    county satisfying the conditions present in \code{ derp
#'
#' @examples
#' \dontrun{
#' hourly_data <- hourly_fips_df <- (fips = "12086", year = 1992,
#'                                   var = c("wind_speed", "temperature"))
#' hourly_stationmap("12086", hourly_data)
#' }
#'
#' @importFrom dplyr %>%
hourly_stationmap <- function(fips, hourly_data, point_color = "firebrick",
                              point_size = 2){

  census_data <- countyweather::county_centers
  row_num <- which(grepl(fips, census_data$fips))
  choro_fips <- as.numeric(census_data[row_num, "fips"])
  title <- census_data[row_num, "name"]

  to_map <- dplyr::select_(census_data, region = ~ region) %>%
    dplyr::mutate_(value = 1)

  map <- suppressMessages(choroplethr::county_choropleth(to_map,
                              title = "", legend = "",
                              num_colors = 1, state_zoom = NULL,
                              county_zoom = choro_fips, reference_map = TRUE))

  map <- map + ggplot2::geom_point(data = hourly_data$stations,
                                   ggplot2::aes_(~ lon, ~ lat),
                        colour = point_color, size = point_size) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(title)

  return(map)

}
