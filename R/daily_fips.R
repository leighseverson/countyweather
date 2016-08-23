#' Pull average daily weather data by US county
#'
#' Given a particular county FIPS code, this function returns a list with two
#' elements: \code{daily_data}, a dataframe of daily average weather values, and
#' \code{station_map}, a map showing the location of weather stations
#' contributing to the average weather data in the \code{daily_data} dataframe.
#'
#' @inheritParams daily_df
#' @inheritParams fips_stations
#'
#' @param station_label TRUE / FALSE to indicate if you want your plot of
#'    weather station locations to include labels indicating station ids.
#'
#' @return A list with three elements. The first element (\code{daily_data}) is a
#'    dataframe of daily weather data averaged across multiple stations, as well
#'    as columns (\code{"var"_reporting}) for each weather variable showing the
#'    number of stations contributing to the average for that variable on that
#'    day. The second element (\code{station_metadata} is a dataframe of station
#'    metadata for stations included in the \code{daily_data} dataframe.
#'    The third element (\code{station_map}) is a plot showing points for all
#'    weather stations for a particular county satisfying the conditions present
#'    in \code{daily_fips}'s arguments (coverage, date_min, date_max,
#'    and/or var).
#'
#' @note Because this function uses the NOAA API to identify the weather
#'    monitors within a US county, you will need to get an access token from
#'    NOAA to use this function. Visit NOAA's token request page
#'    (\url{http://www.ncdc.noaa.gov/cdo-web/token}) to request a token by
#'    email. You then need to set that API code in your R session (e.g., using
#'    \code{options(noaakey = "your key")}, replacing "your key" with the API
#'    key you've requested from NOAA). See the package vignette for more details.
#'
#' @examples
#' \dontrun{
#' ex <- daily_fips("08031", coverage = 0.90,
#'                    date_min = "2010-01-01", date_max = "2010-02-01",
#'                    var = "PRCP")
#'
#' weather_data <- ex$daily_data
#' station_map <- ex$station_map
#'
#' mobile_ex <- weather_fips("01097", percent_coverage = 0,
#'                           date_min = "1997-07-13",
#'                           date_max = "1997-07-25",
#'                           var = "PRCP", average_data = FALSE)
#' library(ggplot2)
#' ggplot(mobile_ex$daily_weather, aes(x = date, y = prcp, color = id)) +
#'        geom_line()
#' }
#' @export
daily_fips <- function(fips, coverage = NULL,
                         date_min = NULL, date_max = NULL,
                         var = "all",
                         average_data = TRUE, station_label = FALSE){
  stations <- fips_stations(fips = fips, date_min = date_min,
                            date_max = date_max)
  weather_data <- daily_df(stations = stations,
                                  date_min = date_min,
                                  date_max = date_max,
                                  coverage = coverage,
                                  var = var,
                                  average_data = average_data)
  station_map <- stationmap_fips(fips = fips,
                                 weather_data = weather_data,
                                 station_label = station_label)
  list <- list("daily_data" = weather_data$daily_data,
               "station_metadata" = weather_data$station_df,
               "station_map" = station_map)
  return(list)
}

#' Return average daily weather data for a particular county.
#'
#' \code{daily_df} returns a list of two elements. The element
#' \code{averaged} is a dataframe of average daily weather values for a
#' particular county, date range, and/or specified "coverage." The element
#' \code{stations} is a dataframe containing metadata about stations
#' contributing to the average weather values in the \code{averaged} dataframe.
#' Columns in the \code{stations} dataframe include each station's latitude,
#' longitude, id, and name.
#'
#' This function serves as a wrapper to several functions from the \code{rnoaa}
#' package, which provides weather data from all relevant stations in a county.
#' This function filters and averages across stations based on user-specified
#' coverage specifications for weather monitors.
#'
#' @note Because this function uses the NOAA API to identify the weather
#'    monitors within a US county, you will need to get an access token from
#'    NOAA to use this function. Visit NOAA's token request page
#'    (\url{http://www.ncdc.noaa.gov/cdo-web/token}) to request a token by
#'    email. You then need to set that API code in your R session (e.g., using
#'    \code{options(noaakey = "your key")}, replacing "your key" with the API
#'    key you've requested from NOAA). See the package vignette for more details.
#'
#' @param stations A dataframe containing station metadata, returned from
#'    the function \code{fips_stations}.
#' @param coverage A numeric value in the range of 0 to 1 that specifies
#'    the desired percentage coverage for the weather variable (i.e., what
#'    percent of each weather variable must be non-missing to include data from
#'    a monitor when calculating daily values averaged across monitors. The
#'    default is to include all monitors with any available data (i.e.,
#'    \code{coverage = 0}).
#' @param var A character vector specifying desired weather variables. For
#'    example, var = c("TMIN", "TMAX", "PRCP"). The default is \code{"all"},
#'    which includes all available weather variables. For a full list of all
#'    possible variable names, see NOAA's README file for the Daily Global
#'    Historical Climatology Network (GHCN-Daily) at
#'    \url{http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt}. Many of
#'    the weather variables are available for some, but not all, monitors, so
#'    your output from this function may not include all the variables
#'    specified using this argument; if you specify a variable here but it is
#'    not included in the output dataset, it means that it was not available in
#'    the time range for any monitor in the county.
#'
#' @param average_data TRUE / FALSE to indicate if you want the function to
#'    average daily weather data across multiple monitors.
#' @inheritParams fips_stations
#'
#' @return A list with two elements. \code{daily_data} is a dataframe of daily
#'    weather data averaged across multiple monitors, as well as columns
#'    (\code{"var"_reporting}) for each weather variable showing the number of
#'    stations contributing to the average for that variable on that day.
#'    The element \code{station_df} is a vector of weather stations contributing
#'    to the average value in the \code{daily_data} dataframe.
#'
#' @examples
#' \dontrun{
#' stations <- fips_stations(fips = "12086", date_min = "2010-01-01",
#'                           date_max = "2010-02-01")
#' list <- daily_df(stations = stations, coverage = 0.90,
#'                       var = c("TMAX", "TMIN", "PRCP"),
#'                       date_min = "2010-01-01", date_max = "2010-02-01")
#' averaged_data <- list$daily_data
#' station_info <- list$stations
#' }
daily_df <- function(stations, coverage = NULL,
                            var = "all",
                            date_min = NULL, date_max = NULL,
                            average_data = TRUE){

  # get tidy full dataset for all monitors
  # meteo_pull_monitors() from helpers_ghcnd.R in ropenscilabs/rnoaa
  quiet_pull_monitors <- purrr::quietly(rnoaa::meteo_pull_monitors)
  meteo_df <- quiet_pull_monitors(monitors = stations$id,
                                  keep_flags = FALSE,
                                  date_min = date_min,
                                  date_max = date_max,
                                  var = var)$result

  # calculate coverage for each weather variable
  # meteo_coverage() from meteo_utils.R in ropenscilabs/rnoaa
  coverage_df <- rnoaa::meteo_coverage(meteo_df, verbose = FALSE)

  # filter station dataset based on specified coverage
  filtered <- filter_coverage(coverage_df,
                              coverage = coverage)
  good_monitors <- unique(filtered$id)

  # filter weather dataset based on stations with specified coverage
  filtered_data <- dplyr::filter_(meteo_df, ~ id %in% good_monitors)

  # steps to filter out erroneous data from individual stations
  # precipitation
  if("PRCP" %in% var){
    filtered_data$prcp <- filtered_data$prcp / 10
    if(max(filtered_data$prcp, na.rm = TRUE) > 1100){
      bad_prcp <- which(with(filtered_data, prcp > 1100))
      filtered_data <- filtered_data[-bad_prcp,]
    }
  }

  # snowfall
  if("SNOW" %in% var){
    if(max(filtered_data$snow, na.rm = TRUE) > 1600){
      bad_snow <- which(with(filtered_data, snow > 1600))
      filtered_data <- filtered_data[-bad_snow,]
    }
  }

  # snow depth
  if("SNWD" %in% var){
    if(max(filtered_data$snwd, na.rm = TRUE) > 11500){
      bad_snwd <- which(with(filtered_data, snwd > 11500))
      filtered_data <- filtered_data[-bad_snwd,]
    }
  }

  # tmax
  if("TMAX" %in% var){
    filtered_data$tmax <- filtered_data$tmax / 10
    if(max(filtered_data$tmax, na.rm = TRUE) > 57){
      bad_tmax <- which(with(filtered_data, tmax > 57))
      filtered_data <- filtered_data[-bad_tmax,]
    }
  }

  # tmin
  if("TMIN" %in% var){
    filtered_data$tmin <- filtered_data$tmin / 10
    if(min(filtered_data$tmin, na.rm = TRUE) < -62){
      bad_tmin <- which(with(filtered_data, tmin < -62))
      filtered_data <- filtered_data[-bad_tmin,]
    }
  }

  # average across stations, add a column for number of stations that
  # contributed to each daily average
  if(average_data == TRUE){
    filtered_data <- ave_weather(filtered_data)
  }

  stations <- dplyr::filter_(stations, ~ id %in% good_monitors)

  out <- list("daily_data" = filtered_data, "station_df" = stations)

  return(out)
}

#' Write daily weather timeseries files for U.S. counties
#'
#' Given a vector of U.S. county FIPS codes, this function creates timeseries
#' dataframes giving: 1. the values for specified weather variables, and 2. the
#' number of weather stations contributing to the average for each day within the
#' specified date range.
#'
#' @return Writes out a directory with daily weather RDS files for each FIPS
#' code specified.
#'
#' @inheritParams daily_df
#' @inheritParams fips_stations
#' @param out_directory The absolute or relative pathname for the directory
#' where you would like the timeseries files to be saved.
#'
#' @note If the function is unable to pull weather data for a particular county
#' given the specified percent coverage, date range, and/or weather variables,
#' \code{daily_timeseries} will not produce a file for that county.
#'
#' @examples
#' \dontrun{
#' daily_timeseries(fips = c("41005", "13089"), coverage = 0.90,
#'            date_min = "2000-01-01", date_max = "2000-01-10",
#'            var = c("TMAX", "TMIN", "PRCP"),
#'            out_directory = "~/timeseries_data")
#' }
#'
#' @export
daily_timeseries <- function(fips, coverage = NULL, date_min, date_max, var,
                              average_data = TRUE,
                              out_directory){

  if(!dir.exists(out_directory)){
    dir.create(out_directory)
  }
  for(i in 1:length(fips)) {
    possibleError <- tryCatch({
      stations <- fips_stations(fips = fips[i], date_min = date_min,
                                date_max = date_max)
      out_df <- daily_df(stations = stations,
                                coverage = coverage,
                                var = var, date_min = date_min,
                                date_max = date_max,
                         average_data = average_data)$daily_data

      out_file <- paste0(out_directory, "/", fips[i], ".", out_type)
        saveRDS(out_df, file = out_file)
    }
    ,
    error = function(e) {
      e
      print(paste0("Unable to pull weather data for FIPS code ", fips[i],
                   " for the specified percent coverage, date range, and/or weather variables."))
    }
    )
    if(inherits(possibleError, "error")) next

  }

}

#' Write plot files for daily weather timeseries dataframes
#'
#' This function writes out a directory with plots for every timeseries file
#' present in the specified directory (produced by the \code{daily_timeseries}
#' function) for a particular weather variable. These plots are meant to aid in
#' initial exploratory analysis.
#'
#' @return Writes out a directory with plots of timeseries data for a given
#' weather variable for each file present in the directory specified.
#'
#' @param var A character string (all lower-case) specifying which weather
#' variable present in the timeseries dataframe you would like to produce
#' plots for. For example, var = "prcp".
#' @param file_directory The absolute or relative pathname for the directory
#' where your daily timeseries dataframes (produced by \code{daily_timeseries})
#' are saved.
#' @param plot_directory The absolute or relative pathname for the directory
#' where you would like the plots to be saved.
#' @param date_min A character string giving the earliest date present in the
#' timeseries dataframe in "yyyy-mm-dd" format.
#' @param date_max  A character string giving the latest date present in the
#' timeseries dataframe in "yyyy-mm-dd" format.
#'
#' @examples
#' \dontrun{
#'plot_daily_timeseries(var = "prcp",
#'                file_directory = "~/Desktop/exposure_data/ihapps_timeseries",
#'                plot_directory = "~/Desktop/exposure_data/plots_prcp")
#'
#'plot_daily_timeseries(files = files, var = "tmax",
#'                file_directory = "~/Desktop/exposure_data/ihapps_timeseries",
#'                plot_directory = "~/Desktop/exposure_data/plots_tmax")
#'
#'plot_daily_timeseries(var = "tmin",
#'                file_directory = "~/Desktop/exposure_data/ihapps_timeseries",
#'                plot_directory = "~/Desktop/exposure_data/plots_tmin")
#' }
#' @importFrom dplyr %>%
#'
#' @export
plot_daily_timeseries <- function(var, file_directory,
                            plot_directory, date_min, date_max){

  files <- list.files(file_directory)

  if(!dir.exists(plot_directory)){
    dir.create(plot_directory)
  }

    file_names <- gsub(".rds", "", files)

  for(i in 1:length(files)){
    data <- readRDS(paste0(file_directory, "/", files[i])) %>%
      dplyr::ungroup() %>%
      as.data.frame()

    file_name <- paste0(file_names[i], ".png")
    grDevices::png(filename = paste0(plot_directory, "/", file_name))
    data$to_plot <- data[ , var]
    graphics::plot(data$date, data$to_plot,
         type = "l", col = "red", main = file_names[i],
         xlab = "date", ylab = var,
         xlim = c(as.Date(date_min), as.Date(date_max)))
    grDevices::dev.off()
  }

}
