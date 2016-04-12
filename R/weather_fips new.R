#' Return average daily weather data for a particular county.
#'
#' \code{weather_fips} returns a data.frame of average daily precipitation,
#' maximum and minimum temperature values for a particular county, date range,
#' and specified "coverage."
#'
#' This function serves as a wrapper to several functions from the
#' \code{rnoaa} package, which provide weather data from all relevant
#' monitors in a county, and then this function filters and averages
#' across monitors based on user-specified coverage specifications.
#'
#' @note Because this function uses the NOAA API to identify the weather
#'    monitors within a US county, you will need to get an access token from
#'    NOAA to use this function. Visit NOAA's token request page
#'    (\url{http://www.ncdc.noaa.gov/cdo-web/token}) to request a token by
#'    email, and then use the code
#'    \code{options("noaakey" = "<key the NOAA emails you>")} to set up your
#'    API access.
#'
#' @param fips A character string giving the five-digit U.S. FIPS county code
#'    of the county for which the user wants to pull weather data.
#' @param coverage_val A numeric value in the range of 0 to 1 that specifies the
#'    desired percentage coverage for the weather variable (i.e., what percent
#'    of each weather variable must be non-missing to include data from a
#'    monitor when calculating daily values averaged across monitors. The
#'    default is 0.90 (90% non-missing observations required to include a
#'    monitor in averaging).
#' @param start_date A character string giving the earliest date you want
#'    in your dataset in "yyyy-mm-dd" format. -
#' \code{start_date}.
#' @param end_date A character string giving the latest date you want
#'    in your dataset in "yyyy-mm-dd" format. -
#'
#' @return A dataframe with
#'
#' @examples
#' \dontrun{
#' df <- weather_fips(fips = "06037", coverage_val = 0.90,
#'                   min_date = "1999-01-01", max_date = "2012-12-31")
#' }
#'
#' @export
weather_fips <- function(fips, coverage_val, min_date, max_date){

  # get stations for 1 fips
  # fips_stations() from weather_fips function.R in countyweather
  stations <- fips_stations(fips, data_coverage = coverage_val)

  # get tidy full dataset for all monitors
  # clean_daily() and meteo_pull_monitors() from helpers_ghcnd.R in
  # openscilabs/rnoaa
  monitors <- meteo_pull_monitors(monitors = stations,
                                  date_min = min_date,
                                  date_max = max_date,
                                  var = c("tmin", "tmax", "prcp"))

  # calculate coverage for each variable (prcp, tmax, tmin)
  # meteo_coverage() from meteo_utils.R in rnoaaopenscilabs
  coverage <- meteo_coverage(monitors, verbose = FALSE)

  # filter station dataset based on specified coverage
  filtered <- filter_coverage(coverage, coverage_val)

  # filter weather dataset based on stations w/ specified coverage
  filtered_data <- filter(monitors, id %in% filtered$id)

  # get rid of unwanted weather variables
  selected_variables <- select(filtered_data, id, date, prcp, tmax, tmin)

  # average across stations, add a column for number of stations that contributed
  # to each daily average
  averaged <- average_weather(selected_variables, min_date, max_date)

  return(averaged)
}

#' Filter stations based on "coverage" requirements
#'
#' \code{filter_coverage} filters precipitation, maximum and minumum temperature
#' based on a specified value of coverage.
#'
#' @param coverage_df a \code{meteo_coverage} data.frame
#' @param percent_coverage a number ranging from 0 to 1. Resulting coverage for
#' precipitation, maximum and minimum temperatures will be equal to or greater
#' than the specified value.
#' @return a \code{data.frame} with stations that meet the specified coverage
#' requirements for \code{prcp}, \code{tmax}, and \code{tmin}.
filter_coverage <- function(coverage_df, percent_coverage){
  filtered <- filter(coverage_df, prcp >= percent_coverage &
                       tmax >= percent_coverage &
                       tmin >= percent_coverage)
  return(filtered)
}

#' Average weather data across multiple stations
#'
#' \code{average_weather} returns a data.frame with daily values for
#' precipitation, maximum and minimum temperature averaged across multiple
#' stations. The resulting data.frame has a column \code{n_reporing} showing the
#' number of stations contributing to the average values for each day. Values for
#' this column can range from 1 to the number of unique stations in your
#' \code{meteo_pull_monitors} data.frame.
#'
#' If a station has missing data for either \code{prcp}, \code{tmax}, or
#' \code{tmin} on a particular day, it will not contribute to the average value
#' for any of those weather variables for that day.
#'
#' @param weather_data a \code{meteo_pull_monitors} data.frame
#' @param start_date a date in "yyyy-mm-dd" format - the earliest date you want
#' in your dataset.
#' \code{start_date}.
#' @param end_date a date in "yyyy-mm-dd" format - the lastest date you want in
#' your dataset.
#' @export
#' @examples
#' \dontrun{
#' monitors <- c("ASN00095063", "ASN00024025", "ASN00040112")
#' obs <- meteo_pull_monitors(monitors)
#' avg <- average_weather(obs, "1999-01-01", "2013-01-01")
#' }
average_weather <- function(weather_data, start_date = NULL, end_date = NULL){
  df <- na.omit(weather_data)
  df <- sqldf::sqldf("select date, avg(prcp) as avg_prcp,
                     avg(tmax) as avg_tmax, avg(tmin) as avg_tmin,
                     count(prcp) as n_reporting
                     from df
                     group by date")

  start_date <- format(as.POSIXct(start_date, format = '%Y-%m-%d'), format =
                                         '%Y-%m-%d')
  end_date <- format(as.POSIXct(end_date, format = '%Y-%m-%d'), format =
                         '%Y-%m-%d')

  if(is.null(start_date)){
    start_date <- min(df$date)
  }
  if(is.null(end_date)){
    end_date <- max(df$date)
  }

    df <- subset(df, date >= as.Date(start_date))
    df <- subset(df, date <= as.Date(end_date))
    return(df)
}
