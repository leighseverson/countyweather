#' NOAA NCDC station IDs per county.
#'
#' \code{fips_stations} returns a dataframe showing NOAA NCDC station IDs for
#' a single U.S. county. This function has options to filter stations based on
#' start and end date of available data, as well as percent of data coverage.
#'
#' A NOAA Token is required to use this function, which interacts with the NCDC
#' API. Request a Token from here: \url{http://www.ncdc.noaa.gov/cdo-web/token}.
#' Then run the code \code{options(noaakey = "your key")} before using this
#' function.
#'
#' @param fips A five-digit U.S. FIPS code in numeric or factor format.
#' @param date_min Accepts date in character, ISO format ("yyyy-mm-dd"). The
#' dataframe returned will include only stations that have data for dates
#' including and after the specified date.
#' @param date_max Accepts date in character, ISO format ("yyyy-mm-dd"). The
#' dataframe returned will include only stations that have data for dates
#' including and before the specified date.
#'
#' @examples
#' \dontrun{
#' ex <- fips_stations("36005")
#' ex2 <- fips_stations("12086", date_min = "1999-01-01",
#'                               date_max = "2012-12-31")
#' }
#'
#' @importFrom dplyr %>%
fips_stations <- function(fips, date_min = NULL, date_max = NULL){
  FIPS <- paste0('FIPS:', fips)
  station_ids <- rnoaa::ncdc_stations(datasetid = 'GHCND', locationid = FIPS,
                                      limit = 10)
  station_df <- station_ids$data
  if(station_ids$meta$totalCount > 10){
    how_many_more <- station_ids$meta$totalCount - 10
    more_stations <- rnoaa::ncdc_stations(datasetid = 'GHCND',
                                          locationid = FIPS,
                                          limit = how_many_more,
                                          offset = 10 + 1)
    station_df <- rbind(df, more_stations$data)
  }

  # If either `min_date` or `max_date` option was null, set to a date that
  # will keep all monitors in the filtering.
  if(is.null(date_max)){
    date_max <- min(df$maxdate)
  }
  if(is.null(date_min)){
    date_min <- max(df$mindate)
  }

  date_max <- lubridate::ymd(date_max)
  date_min <- lubridate::ymd(date_min)

  tot_df <- dplyr::mutate_(station_df,
                           mindate = ~ lubridate::ymd(mindate),
                           maxdate = ~ lubridate::ymd(maxdate)) %>%
    dplyr::filter_(~ maxdate >= date_min & mindate <= date_max) %>%
    dplyr::select_(.dots = c("id", "latitude", "longitude", "name")) %>%
    dplyr::mutate_(id = ~ gsub("GHCND:", "", id))

  # vec <- as.vector(tot_df$id)
  return(tot_df)
}

#' Average weather data across multiple stations.
#'
#' \code{ave_weather} returns a dataframe with daily weather averaged across
#'    stations, as well as columns showing the number of stations contributing
#'    to the average for each variable and each day.
#'
#' @param weather_data A dataframe with daily weather observations. This
#'    dataframe is returned from the function \code{meteo_pull_monitors}.
#'
#' @importFrom dplyr %>%
#'
ave_weather <- function(weather_data){

  all_cols <- colnames(weather_data)
  not_vars <- c("id", "date")
  g_cols <- all_cols[!all_cols %in% not_vars]

  #not sure about -id -date cols - how to do NSE here
  averaged_data <- tidyr::gather_(weather_data, key_col = "key",
                                  value_col = "value",
                                  gather_cols = g_cols) %>%
    dplyr::group_by_(.dots = c("date", "key")) %>%
    dplyr::summarize_(mean = ~ mean(value, na.rm = TRUE)) %>%
    tidyr::spread_(key_col = "key", value_col = "mean")

  n_reporting <- tidyr::gather_(weather_data, key_col = "key",
                                value_col = "value",
                                gather_cols = g_cols) %>%
    dplyr::group_by_(.dots = c("date", "key")) %>%
    dplyr::summarize_(n_reporting = ~ sum(!is.na(value))) %>%
    dplyr::mutate_(key = ~ paste(key, "reporting", sep = "_")) %>%
    tidyr::spread_(key_col = "key", value_col = "n_reporting")

  averaged_data <- dplyr::left_join(averaged_data, n_reporting,
                                    by = "date")
  return(averaged_data)
}

#' Filter stations based on "coverage" requirements.
#'
#' \code{filter_coverage} filters available weather variables
#' based on a specified required minimum coverage (i.e., percent non-missing
#' daily observations).
#'
#' @param coverage_df A \code{meteo_coverage} dataframe
#' @param coverage A numeric value in the range of 0 to 1 that specifies
#'    the desired percentage coverage for the weather variable (i.e., what
#'    percent of each weather variable must be non-missing to include data from
#'    a monitor when calculating daily values averaged across monitors.
#'    (Optional.)
#'
#' @return a \code{dataframe} with stations that meet the specified coverage
#'    requirements for weather variables included in the dataframe present in
#'    this function's arguments.
#'
#' @importFrom dplyr %>%
#'
filter_coverage <- function(coverage_df, coverage = NULL){

  if (is.null(coverage)){
    coverage <- 0
  }

  filtered <- dplyr::select_(coverage_df,
                             .dots = list("-start_date", "-end_date",
                                          "-total_obs")) %>%
    tidyr::gather(key, covered, -id)  %>%
    dplyr::filter_(~ covered >= coverage) %>%
    dplyr::mutate_(covered = ~ 1) %>%
    dplyr::group_by_(.dots = list("id")) %>%
    dplyr::mutate_(good_monitor = ~ sum(!is.na(covered)) > 0) %>%
    dplyr::ungroup() %>%
    dplyr::filter_(~ good_monitor) %>%
    dplyr::select_(.dots = list("-good_monitor"))
  return(filtered)
}

#' Plot daily weather stations for a particular county
#'
#' @param fips A character string giving the five-digit U.S. FIPS county code
#'    of the county for which the user wants to pull weather data.
#' @param weather_data An object returned from \code{daily_df}.
#' @param point_color The specified \code{ggplot2} color for each point
#'    representing the location of a station. (Optional. This argument defaults
#'    to "firebrick.")
#' @param point_size The specified \code{ggplot2} size for each point
#'    representing the location of a station. (Optional. The default size is 2).
#'
#' @return A plot showing points for all weather stations for a particular
#'    county satisfying the conditions present in \code{weather_fip_df}'s
#'    arguments (coverage, date_min, date_max, and/or var).
#'    (\code{stationmap_fips} takes the resulting weather dataframe from this
#'    function.)
#'
#' @examples
#' \dontrun{
#' all_stations <- fips_stations(fips = "12086", date_min = "1999-08-01",
#'                           date_max = "1999-08-31")
#' weather_data <- daily_df(stations = all_stations, coverage =
#'                                 0.90, var = "PRCP", date_min = "1999-08-01",
#'                                 date_max = "1999-08-31")$daily_data
#' stationmap_fips(fips = "12086", weather_data = weather_data)
#' }
#'
#' @importFrom dplyr %>%
#'
stationmap_fips <- function(fips, weather_data, point_color = "firebrick",
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

  map <- map + ggplot2::geom_point(data = weather_data$station_df,
                                   ggplot2::aes_(~ longitude, ~ latitude),
                                   colour = point_color, size = point_size) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(title)

  return(map)
}
