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
#' @param date_min A character string giving the earliest date you want
#'    in your dataset in "yyyy-mm-dd" format. -
#' \code{date_min}.
#' @param date_max A character string giving the latest date you want
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
weather_fips <- function(fips, percent_coverage, date_min, date_max){

  # get stations for 1 fips
  # fips_stations() from weather_fips function.R in countyweather
  stations <- fips_stations(fips, date_min, date_max)

  # get tidy full dataset for all monitors
  # clean_daily() and meteo_pull_monitors() from helpers_ghcnd.R in
  # openscilabs/rnoaa
  monitors <- meteo_pull_monitors(monitors = stations,
                                  date_min,
                                  date_max,
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

#' Average weather data across multiple stations
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


#' Plot of stations for a particular FIPS
#'
#'
#'
#' @examples
#' \dontrun{
#' stationmap_fips("08001", 0.90, "1990-01-01", "1990-12-31")
#' }
stationmap_fips <- function(fips, percent_coverage, date_min, date_max){
  stations <- fips_stations(fips, date_min, date_max)
  monitors <- meteo_pull_monitors(monitors = stations,
                                  date_min,
                                  date_max,
                                  var = c("tmax", "tmin", "prcp"))
  coverage_df <- meteo_coverage(monitors, verbose = FALSE)
  filtered <- filter_coverage(coverage_df, percent_coverage)
  good_monitors <- unique(filtered$id)

  df <- latlong_df(good_monitors)

  station_info <- filter(monitors, monitors$id %in% df$id)

  perc_missing <- gather(station_info, key, value, -id, -date) %>%
    ddply(c("id", "key"), summarize,
          percent_missing = sum(is.na(value)) / length(value)) %>%
    mutate(key = paste(key, "percent_missing", sep = "_")) %>%
    spread(key = key, value = percent_missing)

  df <- left_join(df, perc_missing, by = "id")

  map <- ggmap::get_map(location = c(lon = df$lon[1], lat = df$lat[1]),
                        zoom = 9)
  map <- ggmap::ggmap(map) +
    geom_point(data = df, aes(x = lon, y = lat, color = prcp_percent_missing),
               size = 3)
  # prcp_percent_missing for example - prob want to be able to specify what
  # weather variable you want here
  return(map)
}


mapping <- function(station){
  slist <- gsub("^", "GHCND:", station)
  latlong <- ncdc_stations(stationid = slist)$data
  df <- select(latlong, longitude, latitude, id)
  colnames(df) <- c("lon", "lat", "id")
  df$id <- gsub("GHCND:", "", df$id)
  return(df)
}

latlong_df <- function(station_list){
  for(i in 1:length(station_list)){
    data <- mapping(station_list[i])
    if(i == 1){
      df <- data
    } else {
      df <- rbind(df, data)
    }
  }
  return(df)
}
