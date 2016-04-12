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
#' @param percent_coverage A numeric value in the range of 0 to 1 that specifies the
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
#' df <- weather_fips(fips = "06037", percent_coverage = 0.90,
#'                   min_date = "1999-01-01", max_date = "2012-12-31")
#' }
#'
#' @export
weather_fips <- function(fips, percent_coverage, min_date, max_date){

  # get stations for 1 fips
  # fips_stations() from weather_fips function.R in countyweather
  stations <- fips_stations(fips, date_min = min_date, date_max = max_date)

  # get tidy full dataset for all monitors
  # clean_daily() and meteo_pull_monitors() from helpers_ghcnd.R in
  # openscilabs/rnoaa
  monitors <- meteo_pull_monitors(monitors = stations,
                                  date_min = min_date,
                                  date_max = max_date,
                                  var = c("tmin", "tmax", "prcp"))

  # calculate coverage for each variable (prcp, tmax, tmin)
  # meteo_coverage() from meteo_utils.R in rnoaaopenscilabs
  coverage_df <- meteo_coverage(monitors, verbose = FALSE)

  # filter station dataset based on specified coverage
  filtered <- filter_coverage(coverage_df, percent_coverage)
  good_monitors <- unique(filtered$id)

  # filter weather dataset based on stations w/ specified coverage
  filtered_data <- gather(monitors, key, value, -id, -date) %>%
    left_join(filtered, by = c("id", "key")) %>%
    filter(id %in% good_monitors) %>%
    mutate(value = value * covered) %>%
    select(-covered) %>%
    spread(key = key, value = value)

  # average across stations, add a column for number of stations that contributed
  # to each daily average
  # averaged <- average_weather(filtered_data, min_date, max_date)
  averaged <- ave_weather(filtered_data)

  return(averaged)
}

ave_weather <- function(filtered_data){
  averaged_data <- gather(filtered_data, key, value, -id, -date) %>%
    ddply(c("date", "key"), summarize,
          mean = mean(value, na.rm = TRUE)) %>%
    spread(key = key, value = mean)
  n_reporting <- gather(filtered_data, key, value, -id, -date) %>%
    ddply(c("date", "key"), summarize,
          n_reporting = sum(!is.na(value))) %>%
    mutate(key = paste(key, "reporting", sep = "_")) %>%
    spread(key = key, value = n_reporting)
  averaged_data <- left_join(averaged_data, n_reporting,
                             by = "date")
  return(averaged_data)
}

#' Filter stations based on "coverage" requirements
#'
#' \code{filter_coverage} filters available weather variables
#' based on a specified required minimum coverage (i.e., percent non-missing
#' daily observations).
#'
#' @param coverage_df a \code{meteo_coverage} data.frame
#' @inheritParams weather_fips
#'
#' @return a \code{data.frame} with stations that meet the specified coverage
#' requirements for \code{prcp}, \code{tmax}, and \code{tmin}.
filter_coverage <- function(coverage_df, percent_coverage){
  filtered <- select(coverage_df, -start_date, -end_date, -total_obs) %>%
    gather(key, covered, -id)  %>%
    filter(covered >= percent_coverage) %>%
    mutate(covered = 1) %>%
    group_by(id) %>%
    mutate(good_monitor = sum(!is.na(covered)) > 0) %>%
    ungroup() %>%
    filter(good_monitor) %>%
    select(-good_monitor)
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
#' @param end_date a date in "yyyy-mm-dd" format - the lastest date you want in
#' your dataset.
#' @export
#' @examples
#' \dontrun{
#' monitors <- c("ASN00095063", "ASN00024025", "ASN00040112")
#' obs <- meteo_pull_monitors(monitors)
#' avg <- average_weather(obs, "1999-01-01", "2012-12-31")
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
