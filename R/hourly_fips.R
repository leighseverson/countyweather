#' Return average hourly weather data and a plot showing the location of weather
#' stations for a particular county.
#'
#' Given a particular county FIPS code, this function returns a list with two
#' elements: "data", a dataframe of hourly average weather values, and "plot",
#' a plot showing the location of weather stations contributing to the average
#' weather in "data".
#'
#' @inheritParams hourly_df
#'
#' @param station_label TRUE / FALSE to indicate if you want your plot of
#'    weather station locations to include labels indicating station usaf id
#'    numbers.
#'
#' @return A list with two elements. The first element (\code{hourly_data}) is a
#'    dataframe of daily weather data averaged across multiple stations, as well
#'    as columns (\code{"var"_reporting}) for each weather variable showing the
#'    number of stations contributing to the average for that variable for that
#'    hour The second element (\code{station_map}) is a plot showing points for all
#'    weather stations for a particular county satisfying the conditions present
#'    in \code{hourly_fips}'s arguments (year, coverage, and/or var).
#'
#' @export
hourly_fips <- function(fips, year, var = c("wind_direction", "wind_speed",
                                            "ceiling_height", "visibility_distance",
                                            "temperature", "temperature_dewpoint",
                                            "air_pressure"),
                        coverage = NULL, radius = 50, average_data = TRUE,
                        station_label = FALSE){

  weather_data <- hourly_df(fips = fips, year = year, var = var,
                                 coverage = coverage, radius = radius,
                                 average_data = average_data)

  station_map <- hourly_stationmap(fips = fips, hourly_data = weather_data,
                                   station_label = FALSE)

  list <- list("hourly_data" = weather_data$hourly_data,
               "station_map" = station_map)
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
#'    example, var = c("wind_speed", "temperature"). (Optional. \code{var}
#'    includes all possible weather variables by default, which include
#'    \code{c("wind_direction", "wind_speed", "ceiling_height",
#'    "visibility_distance", "temperature", "temperature_dewpoint",
#'    "air_pressure")}. Alternatively, you can specify var = "all" to include
#'    additional flag and quality codes.
#' @param average_data TRUE / FALSE to indicate if you want the function to
#'    average daily weather data across multiple monitors.
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
#' df <- hourly_df(fips = "12086", year = 1992, var = c("wind_speed",
#'                      "temperature"))
#' data <- df$hourly_data
#' station_info <- df$station_df
#' }
#'
#' @export
hourly_df <- function(fips, year,
                           var = c("wind_direction", "wind_speed",
                                   "ceiling_height", "visibility_distance",
                                   "temperature", "temperature_dewpoint",
                                   "air_pressure"),
                           average_data = TRUE, radius = 50, coverage = NULL){

  # hourly data for multiple monitors
  hourly_list <- lapply(year, function(x) isd_monitors_data(fips = fips,
                                                              year = x,
                                                              var = var,
                                                              radius = radius))
  data <- dplyr::bind_rows(hourly_list)

  # if coverage is not null, filter stations
  if(!purrr::is_null(coverage)){
    data <- filter_hourly(hourly_data = data, coverage = coverage, var = var)
  }

  # station meta data for one county
  station_metadata <- isd_fips_stations(fips)

  # average hourly across multiple stations
  if(average_data == TRUE){
    data <- ave_hourly(data)
  }

  out <- list("hourly_data" = data, "station_df" = station_metadata)
  return(out)
}

#' Write hourly weather timeseries files for U.S. counties
#'
#' Given a vector of U.S. county FIPS codes, this function creates timeseries
#' dataframes giving: 1. the values for specified weather variables, and 2. the
#' number of weather stations contributing to the average for each day within the
#' specified date range.
#'
#' @return Writes out a directory with daily weather files for each FIPS code
#' specified.
#'
#' @inheritParams hourly_df
#' @param out_directory The absolute or relative pathname for the directory
#' where you would like the timeseries files to be saved.
#' @param out_type A character string indicating that you would like either .rds
#' files (out_type = "rds") or .csv files (out_type = ".csv"). This option
#' defaults to .rds files.
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
                              var = c("wind_direction", "wind_speed",
                                      "ceiling_height", "visibility_distance",
                                      "temperature", "temperature_dewpoint",
                                      "air_pressure"), radius = 50,
                              average_data = TRUE,
                              out_directory, out_type = "rds"){
  if(!dir.exists(out_directory)){
    dir.create(out_directory)
  }
  for(i in 1:length(fips)) {
    possibleError <- tryCatch({
      out_df <- hourly_df(fips = fips[i], year = year, var = var,
                               coverage = coverage,
                               average_data = average_data)$hourly_data
      out_file <- paste0(out_directory, "/", fips[i], ".", out_type)
      if(out_type == "rds"){
        saveRDS(out_df, file = out_file)
      } else if (out_type == "csv"){
        utils::write.csv(out_df, file = out_file, row.names = FALSE)
      }
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

#' Write plot files for hourly weather timeseries dataframes
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
#' @param file_type A character string indicating the type of timeseries files
#' you would like to produce plots for (either "rds" or "csv"). This option
#' defaults to .rds files.
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
#' @export
plot_hourly_timeseries <- function(var, year, file_directory, file_type = "rds",
                                  plot_directory){
  files <- list.files(file_directory)

  date_min <- paste0(min(year), "-01-01 00:00:00")
  date_max <- paste0(max(year), "-12-31 23:00:00")

  if(!dir.exists(plot_directory)){
    dir.create(plot_directory)
  }

  if(file_type == "rds"){
    file_names <- gsub(".rds", "", files)
  } else if (file_type == "csv"){
    file_names <- gsub(".csv", "", files)
  }

  for(i in 1:length(files)){

    setwd(file_directory)
    data <- readRDS(files[i])

    file_name <- paste0(file_names[i], ".png")
    setwd(plot_directory)
    grDevices::png(filename = file_name)
    graphics::plot(data$date_time, data[,var],
                   type = "l", col = "red", main = file_names[i],
                   xlab = "date", ylab = var,
                   xlim = c(as.Date(date_min), as.Date(date_max)))
    grDevices::dev.off()
  }
}
