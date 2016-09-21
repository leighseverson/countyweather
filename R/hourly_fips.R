#' Return average hourly weather data and a plot showing the location of weather
#' stations for a particular county.
#'
#' Given a particular county FIPS code, this function returns a list with two
#' elements: \code{data}, a dataframe of hourly average weather values, and
#' \code{plot}, a plot showing the location of weather stations contributing to
#' the average weather in \code{data}.
#'
#' @inheritParams hourly_df
#'
#' @param station_label TRUE / FALSE to indicate if you want your plot of
#'    weather station locations to include labels indicating station usaf id
#'    numbers.
#'
#' @return A list with six elements. The first element (\code{hourly_data}) is a
#'    dataframe of daily weather data averaged across multiple stations, as well
#'    as columns (\code{"var"_reporting}) for each weather variable showing the
#'    number of stations contributing to the average for that variable for that
#'    hour. The second element (\code{station_metadata} is a dataframe of station
#'    metadata for stations included in the \code{daily_data} dataframe.
#'    The third element (\code{station_map}) is a plot showing points for all
#'    weather stations for a particular county satisfying the conditions present
#'    in \code{hourly_fips}'s arguments (year, coverage, and/or var).
#'    \code{radius} is the calculated radius within which stations were pulled
#'    from the county's population-weighted center. Elements \code{lat_center}
#'    and \code{lon_center} are the latitude and longitude of the county's
#'    population-weighted center.
#'
#' @note Observation times are vased on Coordinated Universal Time Code (UTC).
#'
#'@examples
#' \dontrun{
#'
#' ex <- hourly_fips("12086", coverage = 0.90, year = c(1994, 1995),
#'                    var = "temperature")
#'
#' data <- ex$hourly_data
#' station_data <- ex$station_metadata
#' station_map <- ex$station_map
#' @export
hourly_fips <- function(fips, year, var = "all",
                        coverage = NULL, average_data = TRUE,
                        station_label = FALSE){

  weather_data <- hourly_df(fips = fips, year = year, var = var,
                                 coverage = coverage,
                                 average_data = average_data)

  station_map <- hourly_stationmap(fips = fips, hourly_data = weather_data,
                                   station_label = FALSE)

  list <- list("hourly_data" = weather_data$hourly_data,
               "station_metadata" = weather_data$station_df,
               "station_map" = station_map,
               "radius" = weather_data$radius,
               "lat_center" = weather_data$lat_center,
               "lon_center" = weather_data$lon_center)
  return(list)

}

#' Return average hourly weather data for a particular county.
#'
#' \code{hourly_df} returns a dataframe of average daily weather values
#' for a particular county, year, and/or specified "coverage."
#'
#' This function serves as a wrapper to several functions from the \code{rnoaa}
#' package, which provides weather data from all relevant stations in a county.
#' This function filters and averages across NOAA ISD/ISH stations based on
#' user-specified coverage specifications.
#'
#' @param fips A character string or vector giving the five-digit U.S. FIPS
#'    county code of the county or counties for which the user wants to pull
#'    weather data.
#' @param year a four digit number or vector of numbers indicating the year or
#'    years for which you want to pull hourly data. Values for \code{year} can
#'    be in the range from 1901 to the current year.
#' @param var A character vector specifying desired weather variables. For
#'    example, var = c("wind_speed", "temperature"). The core weather variables
#'    available include \code{c("wind_direction", "wind_speed", "ceiling_height",
#'    "visibility_distance", "temperature", "temperature_dewpoint",
#'    "air_pressure")}. Alternatively, you can specify var = "all" to include
#'    additional flag and quality codes.
#' @param average_data TRUE / FALSE to indicate if you want the function to
#'    average daily weather data across multiple monitors.
#' @param coverage A numeric value in the range of 0 to 1 that specifies
#'    the desired percentage coverage for the weather variable (i.e., what
#'    percent of each weather variable must be non-missing to include data from
#'    a monitor when calculating daily values averaged across monitors.)
#'
#' @return A list with five elements. The first element, \code{hourly_data}, is
#'    a dataframe of hourly weather data averaged across multiple stations,
#'    as well as columns (\code{"var"_reporting}) for each weather variable
#'    showing the number of stations contributing to the average for that
#'    variable for each hour. \code{station_df} is a dataframe of station
#'    metadata for each station contributing weather data. A weather station
#'    will have one row per weather variable it contributes data to. In addition
#'    to information such as usaf and wban ids and station names, this
#'    dataframe includes statistical information about weather values
#'    contributed by each station for each weather variable. These statistics
#'    include calculated coverage (\code{calc_coverage}), which is the percent
#'    of non-missing values for each station and variable for the specified
#'    date range, \code{standard_dev} (standard deviation), and \code{max} and
#'    \code{min} values for each station-weather variable combination. The
#'    element \code{radius} is the calculated radius within which stations were
#'    pulled from the county's population-weighted center. Elements
#'    \code{lat_center} and \code{lon_center} are the latitude and longitude of
#'    the county's population-weighted center.
#'
#' @note: Observation times are vased on Coordinated Universal Time Code (UTC).
#'
#' @references For more information on this dataset and available weather and
#' flag/quality variables, see
#' \url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-format-document.pdf}.
#'
#' @examples
#' \dontrun{
#' df <- hourly_df(fips = "12086", year = 1992, var = c("wind_speed",
#'                      "temperature"))
#' data <- df$hourly_data
#' station_info <- df$station_df
#' radius <- df$radius
#' }
#'
#' @export
hourly_df <- function(fips, year,
                           var = "all",
                           average_data = TRUE, coverage = NULL){

  # hourly data for multiple monitors for multiple years
  hourly_list <- lapply(year, function(x) isd_monitors_data(fips = fips,
                                                            year = x,
                                                            var = var))

  for(i in 1:length(year)){
    list <- hourly_list[[i]]
    if (i == 1){
      data <- list$df
    } else {
      data <- dplyr::bind_rows(data, list$df)
    }
  }

  # station meta data for one county (unfiltered)

  for(i in 1:length(year)){
    list <- hourly_list[[i]]
    if (i == 1){
      station_metadata <- list$ids
    } else {
      station_metadata <- dplyr::bind_rows(station_metadata, list$ids)
    }
  }

  # filter stations (if coverage is NULL, filters as if coverage = 0) and get
  # statistical info for each station/var pair
    filtered_list <- filter_hourly(hourly_data = data, coverage = coverage, var = var)
    station_stats <- filtered_list$stations

    filtered_stations <- unique(station_stats$station)

    station_metadata[station_metadata == "999999"] <- NA
    station_metadata[station_metadata == "99999"] <- NA

    station_metadata <- unique(station_metadata) %>%
      dplyr::mutate_(station = ~ paste(usaf, wban, sep = "-"))

    station_metadata <- station_metadata %>%
      dplyr::filter_(~ station %in% filtered_stations)

  # combine station_metadata and station_stats

    station_metadata <- dplyr::right_join(station_metadata, station_stats,
                                          by = "station") %>%
      select_(quote(usaf), quote(wban), quote(station), quote(station_name),
             quote(var), quote(calc_coverage), quote(standard_dev), quote(range),
             quote(ctry), quote(state), quote(elev_m), quote(begin), quote(end),
             quote(lon), quote(lat))

  # average hourly across multiple stations

  data <- data %>%
    dplyr::mutate_(station = ~ paste(usaf_station, wban_station, sep = "-")) %>%
    dplyr::filter_(~ station %in% filtered_stations) %>%
    dplyr::select_(quote(-station))

  if(average_data == TRUE){
    data <- ave_hourly(data)
  }

  radius <- hourly_list[[1]]$radius
  lat_center <- hourly_list[[1]]$lat_center
  lon_center <- hourly_list[[1]]$lon_center

  out <- list("hourly_data" = data, "station_df" = station_metadata,
              "radius" = radius,
              "lat_center" = lat_center,
              "lon_center" = lon_center)
  return(out)
}

#' Write hourly weather timeseries files for U.S. counties.
#'
#' Given a vector of U.S. county FIPS codes, this function saves lists of five
#' elements created from the function \code{hourly_fips}. Within this list,
#' the element \code{hourly_data} gives a timeseries dataframe giving: 1. the
#' values for specified weather variables, and 2. the number of weather stations
#' contributing to the average for each day within the specified date range.
#' Other elements saved include \code{station_metadata} and \code{station_map}.
#' \code{radius} gives the radius (in km) within which weather stations
#' were pulled from each county's population-weighted center, and
#' \code{lat_center} and \code{lon_center} are the latitude and longitude of the
#' county's population-weighted center.
#'
#' Given a vector of U.S. county FIPS codes, this function creates timeseries
#' dataframes giving: 1. the values for specified weather variables, and 2. the
#' number of weather stations contributing to the average for each day within the
#' specified date range.
#'
#' @return Writes out a directory with daily weather RDS files for each FIPS
#' code specified.
#'
#' @inheritParams hourly_df
#' @param out_directory The absolute or relative pathname for the directory
#' where you would like the timeseries files to be saved.
#'
#' @note If the function is unable to pull weather data for a particular county
#' given the specified percent coverage, date range, and/or weather variables,
#' \code{county_timeseries} will not produce a file for that county.
#'
#' @examples
#' \dontrun{
#' county_timeseries(fips = c("41005", "13089"), coverage = 0.90, year = 1992,
#'                   var = c("wind_speed", "temperature"),
#'                   out_directory = "~/timeseries_hourly")
#' }
#' @export
hourly_timeseries <- function(fips, coverage = NULL, year,
                              var = "all",
                              average_data = TRUE,
                              out_directory){
  if(!dir.exists(out_directory)){
    dir.create(out_directory)
  }
  for(i in 1:length(fips)) {
    possibleError <- tryCatch({
      out_list <- hourly_df(fips = fips[i], year = year, var = var,
                               coverage = coverage,
                               average_data = average_data)

      out_file <- paste0(out_directory, "/", fips[i], ".rds")
        saveRDS(out_list, file = out_file)
    }
    ,
    error = function(e) {
      e
      print(paste0("Unable to pull weather data for FIPS code ", fips[i],
                   " for the specified percent coverage, year(s), and/or",
                   " weather variables."))
    }
    )
    if(inherits(possibleError, "error")) next
  }
}

#' Write plot files for hourly weather timeseries dataframes.
#'
#' This function writes out a directory with plots for every timeseries file
#' present in the specified directory (produced by the \code{hourly_timeseries}
#' function) for a particular weather variable. These plots are meant to aid in
#' initial exploratory analysis.
#'
#' @return Writes out a directory with plots of timeseries data for a given
#' weather variable for each file present in the directory specified.
#'
#' @param var A character string (all lower-case) specifying which weather
#' variable present in the timeseries dataframe you would like to produce
#' plots for. For example, var = "wind_speed".
#' @param file_directory The absolute or relative pathname for the directory
#' where your daily timeseries dataframes (produced by \code{county_timeseries})
#' are saved.
#' @param plot_directory The absolute or relative pathname for the directory
#' where you would like the plots to be saved.
#' @param year A year or vecotr of years giving the year(s) present in the
#' timeseries dataframe.
#' @examples
#' \dontrun{
#'plot_hourly_timeseries(var = "wind_speed", year = 1992,
#'                file_directory = "~/timeseries_hourly",
#'                plot_directory = "~/timeseries_plots")
#'}
#' @importFrom dplyr %>%
#' @export
plot_hourly_timeseries <- function(var, year, file_directory,
                                  plot_directory){
  files <- list.files(file_directory)

  date_min <- paste0(min(year), "-01-01 UTC")
  date_min <- as.POSIXct(date_min, tz = "UTC")

  date_max <- paste0(max(year), "-12-31 23:00:00 UTC")
  date_max <- as.POSIXct(date_max, tz = "UTC")

  if(!dir.exists(plot_directory)){
    dir.create(plot_directory)
  }

    file_names <- gsub(".rds", "", files)

  for(i in 1:length(files)){

    setwd(file_directory)
    dat <- readRDS(files[i])
    dat <- dat$hourly_data

    # convert tibble to vector (avoiding error "'x' and 'y' lengths differ")
    y <- dat %>% dplyr::collect %>% .[[var]]

    file_name <- paste0(file_names[i], ".png")
    setwd(plot_directory)
    grDevices::png(filename = file_name)
    graphics::plot(dat$date_time, y,
                   type = "l", col = "red", main = file_names[i],
                   xlab = "date", ylab = var,
                   xlim = c(date_min, date_max)
                   )
    grDevices::dev.off()
  }
}
