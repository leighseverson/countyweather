#' Get station list for a particular U.S. county.
#'
#' This function serves as a wrapper to the \code{isd_stations_search} function
#' in the \code{rnoaa} package, allowing you to search by FIPS code rather than
#' having to know the latitude and longitude of the center of each county.
#' \code{isd_stations_search} requires a radius within which to search for
#' stations. This radius is estimated from 2010 U.S. Census Land Area data.
#'
#' @param fips A five-digit FIPS county code.
#' @param verbose TRUE / FALSE to indicate if you want the function to print
#'    out the name of the county it's processing
#'
#' @return A list with four elements. The first element, \code{stations}, is a
#'    dataframe of monitors within a calculated radius of the
#'    population-weighted center of the county specified by the FIPS code.
#'    This will have the same dataframe format as the output from the
#'    \code{isd_stations_search} function in the \code{rnoaa} package. The
#'    second element, \code{radius}, gives the radius (in km) within which
#'    stations were pulled from the county's population-weighted center.
#'    Elements \code{lat_center} and \code{lon_center} are the latitude and
#'    longitude of the county's population-weighted center.
#'
#' @examples
#' \dontrun{
#' ids <- isd_fips_stations(fips = "12086")$stations
#' }
isd_fips_stations <- function(fips, verbose = TRUE){

  # population-weighted center for specified county
  census_data <- countyweather::county_centers
  loc_fips <- which(census_data$fips == fips)
  lat_fips <- census_data[loc_fips, "latitude"]
  lon_fips <- census_data[loc_fips, "longitude"]

  # radius data for specified county
  radius_data <- countyweather::county_radius
  loc_rad <- which(radius_data == fips)
  radius <- radius_data[loc_rad, "county_radius"]

  if(verbose) {
    print(paste0("Getting hourly weather monitors for ",
                 census_data[loc_fips, "name"]))
  }

  quiet_station_search <- purrr::quietly(rnoaa::isd_stations_search)
  stations <- quiet_station_search(lat = lat_fips, lon = lon_fips,
                                   radius = radius)$result

  list <- list("stations" = stations,
               "radius" = radius,
               "lat_center" = lat_fips,
               "lon_center" = lon_fips)

  return(list)
}

#' Get hourly data for a single monitor.
#'
#' This function wraps the \code{isd} function from the \code{rnoaa} package.
#'
#' @param usaf_code A character string with a six-digit usaf code for the
#'    monitor.
#' @param wban_code A character string with a five-digit wban code for the
#'    monitor.
#' @param year A four-digit numeric giving the year for which to pull data.
#' @param var A character vector listing the weather variables to pull. Choices
#'    include "wind_direction", "wind_speed", "ceiling_height",
#'    "visibility_distance", "temperature", "temperature_dewpoint", and
#'    "air_pressure."
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
#' ids <- isd_fips_stations(fips = "12086")$stations
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
                             var = "all"){
  quiet_isd <- purrr::quietly(rnoaa::isd)
  isd_df <- quiet_isd(usaf = usaf_code, wban = wban_code, year = year)
  isd_df <- isd_df$result

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

#' Pull hourly data for multiple monitors.
#'
#' Pull all available data for all weather monitors within a calculated radius of
#' the population-weighted center of a U.S. county, based on the county's FIPS
#' code. The radius for each county is calculated using 2010 U.S. Census Land Area
#' data.
#'
#' @param fips A five-digit FIPS county code.
#' @param year A four-digit numeric giving the year for which to pull data.
#' @param var A character vector listing the weather variables to pull. The main
#'    available weather variables include \code{wind_direction}, \code{wind_speed},
#'    \code{ceiling_height}, \code{visibility_distance}, \code{temperature},
#'    \code{temperature_dewpoint} and \code{air_pressure}.
#'
#' @return A list with five elements: \code{ids} is a dataframe of station
#'    metadata for all avaiable stations in the given fips code. \code{df} is a
#'    data frame with hourly weather data for the given variable(s) and date
#'    range. \code{radius} is the calculated radius within which stations
#'    were pulled from the county's population-weighted center. Elements
#'    \code{lat_center} and \code{lon_center} are the latitude and longitude
#'    of the county's population-weighted center.
#'
#' @examples
#' \dontrun{
#' stationdata <- isd_monitors_data(fips = "12086", year = 1992,
#'                                  var = c("wind_speed", "temperature"))$df
#' ggplot(stationdata, aes(x = date_time, y = wind_speed)) +
#'    geom_point(alpha = 0.5, size = 0.2) +
#'    facet_wrap(~ usaf_station, ncol = 1)
#' }
isd_monitors_data <- function(fips, year, var = "all"){

  list <- isd_fips_stations(fips)
  ids <- list$stations
  radius <- list$radius
  lat_center <- list$lat_center
  lon_center <- list$lon_center

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

      cols <- colnames(st_out_list[[1]])

      if("wind_direction" %in% cols){
        x$wind_direction <- as.numeric(x$wind_direction)
      }
      if("ceiling_height" %in% cols){
        x$ceiling_height <- as.numeric(x$ceiling_height)
      }
      if("visibility_distance" %in% cols){
        x$visibility_distance <- as.numeric(x$visibility_distance)
      }
      if("temperature" %in% cols){
        x$temperature <- as.numeric(x$temperature)
      }
      if("temperature_dewpoint" %in% cols){
        x$temperature_dewpoint <- as.numeric(x$temperature_dewpoint)
      }
      if("air_pressure" %in% cols){
        x$air_pressure <- as.numeric(x$air_pressure)
      }
      if("GF1_lowest_cloud_base_height" %in% cols){
        x$GF1_lowest_cloud_base_height <- as.numeric(x$GF1_lowest_cloud_base_height)
      }

      return(x)
    })
    st_out_df <- dplyr::bind_rows(st_out_list)
  } else(
    stop("None of the stations had available data.")
  )

  # filter so ids stations match with filtered df's stations
  # out = list: data and stations
  # want to be able to access station metadata later for mapping, etc.

  list <- list("df" = st_out_df,
               "ids" = ids,
               "radius" = radius,
               "lat_center" = lat_center,
               "lon_center" = lon_center)
  return(list)
}

#' Average hourly weather data across multiple stations.
#'
#' \code{ave_hourly} returns a dataframe with hourly weather averaged across
#' stations, as well as columns showing the number of stations contributing to
#' average for each variable and each hour.
#'
#' @param hourly_data A dataframe with hourly weather observations. This
#'    dataframe is returned from the "df" element of the function
#'    \code{isd_monitors_data}.
#'
#' @importFrom dplyr %>%
ave_hourly <- function(hourly_data){

  df <- dplyr::mutate_(hourly_data, id = ~ paste0(usaf_station, wban_station))
  df <- dplyr::select_(df, .dots = c("-usaf_station", "-wban_station",
                                     "-latitude", "-longitude"))

  all_cols <- colnames(df)
  not_vars <- c("date_time", "id")
  g_cols <- all_cols[!all_cols %in% not_vars]

  averaged_data <- tidyr::gather_(data = df, key_col = "key",
                                  value_col = "value", gather_cols = g_cols) %>%
    dplyr::group_by_(~ date_time, ~ key) %>%
    dplyr::summarize_(mean = ~ mean(value, na.rm = TRUE)) %>%
    tidyr::spread_(key_col = "key", value_col = "mean")

  n_reporting <- tidyr::gather_(data = df, key_col = "key", value_col = "value",
                                gather_cols = g_cols) %>%
    dplyr::group_by_(~ date_time, ~ key) %>%
    dplyr::summarise_(n_reporting = ~ sum(!is.na(value))) %>%
    dplyr::mutate_(key = ~ paste(key, "reporting", sep = "_")) %>%
    tidyr::spread_(key_col = "key", value_col = "n_reporting")

  averaged_data <- dplyr::left_join(averaged_data, n_reporting,
                                    by = "date_time")

  averaged_data <- dplyr::ungroup(averaged_data, ~ date_time)
  averaged_data <- as.data.frame(averaged_data)

  return(averaged_data)
}

#' Filter NOAA ISD stations based on "coverage" requirements.
#'
#' \code{filter_hourly} filters available weather variables based on a specified
#' minimum coverage (i.e., percent non-missing hourly observations).
#'
#' @param hourly_data A \code{isd_monitors_data} dataframe (The "df" element of
#'    a \code{isd_monitors_data} list)
#' @param coverage A numeric value in the range of 0 to 1 that specifies the
#'    desired percentage coverage for each weather variable (i.e., what percent
#'    of each weather variable must be non-missing to include the data from a
#'    station when calculating hourly values averaged across stations).
#' @param var A character vector specifying desired weather variables. For
#'    example, var = c("wind_speed", "temperature"). The core available weather
#'    variables include \code{wind_direction}, \code{wind_speed},
#'    \code{ceiling_height}, \code{visibility_distance}, \code{temperature},
#'    \code{temperature_dewpoint} and \code{air_pressure}. Alternatively,
#'    you can specify var = "all" to include additional flag and quality codes.
#'
#' @return A list with two elements: \code{stations} is a dataframe giving
#'    statistical information for stations that meet the specified coverage
#'    requirements. The column \code{station} gives the station id (usaf and
#'    wban identification numbers pasted together, separated by "-"). Note: one
#'    of these identification ids is sometimes missing. For example, value in
#'    \code{station} might be \code{722029-NA}. The column \code{var}
#'    gives the weather variable associated with the row of statistical values
#'    for each station and variable combination. \code{calc_covearge} gives the
#'    percentage coverage for each weather variable and station. These values
#'    will all be greater than or equal to the specified \code{coverage} value.
#'    \code{standard_dev} gives the standard deviation of values for each station
#'    and weather variable. \code{max} and \code{min} give the minimum and
#'    maximum values for the values in each station and weather variable.
#'
#' @importFrom dplyr %>%
filter_hourly <- function(hourly_data, coverage = NULL,
                          var = "all"){

  if(purrr::is_null(coverage)){
   coverage <- 0
  }

  all_cols <- colnames(hourly_data)
  not_vars <- c("usaf_station", "wban_station", "date_time", "latitude",
                "longitude")
  g_cols <- all_cols[!all_cols %in% not_vars]
  group_cols <- c("station", "key")

  # calc_coverage for each station (combination of usaf and wban ids)

  df <- hourly_data %>%
    tidyr::unite_(col = "station", from = c("usaf_station", "wban_station"),
                  sep = "-") %>%
    dplyr::select_(quote(-date_time), quote(-latitude), quote(-longitude)) %>%
    tidyr::gather_(key_col = "key", value_col = "value", gather_cols = g_cols) %>%
    dplyr::group_by_(.dots = group_cols) %>%
    dplyr::summarize_(calc_coverage = ~ mean(!is.na(value)),
                      standard_dev = ~ sd(value, na.rm = TRUE),
                      range = ~ max(value, na.rm = TRUE) -
                        min(value, na.rm = TRUE))

  group_cols <- c("date_time", "key")

  filtered <- hourly_data %>%
    tidyr::unite_(col = "station", from = c("usaf_station", "wban_station"),
                  sep = "-") %>%
    dplyr::select_(quote(-latitude), quote(-longitude)) %>%
    tidyr::gather_(key_col = "key", value_col = "value", gather_cols = g_cols) %>%
    dplyr::left_join(df, by = c("station", "key")) %>%
    dplyr::filter_(~ calc_coverage >= coverage) %>%
    dplyr::group_by_(.dots = group_cols)

  stations <- filtered %>%
    dplyr::ungroup() %>%
    dplyr::select_(quote(-date_time), quote(-value)) %>%
    dplyr::distinct()

  colnames(stations)[2] <- "var"

  df2 <- filtered %>%
    dplyr::summarize_(n_reporting = ~ sum(!is.na(value))) %>%
    dplyr::mutate_(key = ~ paste(key, "reporting", sep = "_")) %>%
    tidyr::spread_(key_col = "key", value_col = "n_reporting")

  df3 <- filtered %>%
    dplyr::summarize_(value = ~ mean(value, na.rm = TRUE)) %>%
    tidyr::spread_(key_col = "key", value_col = "value")

  out <- dplyr::full_join(df3, df2, by = "date_time")

  list <- list("df" = out,
               "stations" = stations)

  return(list)
}

#' Plot hourly weather stations for a particular county.
#'
#' This function produces a map with points indicating stations that contribute
#' to the weather data in the argument \code{hourly_data}.
#'
#' @param fips A five-digit FIPS county code.
#' @param hourly_data A list returned from the function \code{hourly_df}.
#' @param point_color point_color The specified \code{ggplot2} color for each point
#'    representing the location of a station.
#' @param point_size The specified \code{ggplot2} size for each point
#'    representing the location of a station.
#' @param station_label TRUE / FALSE If TRUE, includes labels giving the id for
#'    monitor on the map. The default is FALSE.
#'
#' @return A plot showing points for all weather stations for a particular
#'    county satisfying the conditions present in \code{hourly_df}'s
#'    arguments (year(s) and/or var). Because hourly data is pulled by radius
#'    from each county's population-weighted center, this plot inlcudes the
#'    calculated radius from which stations are pulled. This radius is calculated
#'    for each county using 2010 U.S. Census Land Area data. 2010 U.S. Census
#'    cartographic boundary shapefiles are used to proved county outlines,
#'    included on this plot as well. Note: because stations are pulled within
#'    a radius from the county's population-weighted center, stations from
#'    outside of the county's boundaries may sometimes be providing data for that
#'    county.
#'
#' @examples
#' \dontrun{
#' hourly_data <- hourly_df(fips = "12086", year = 1992,
#'                          var = c("wind_speed", "temperature"))
#' hourly_stationmap("12086", hourly_data)
#' }
#'
#' @importFrom dplyr %>%
hourly_stationmap <- function(fips, hourly_data, point_color = "firebrick",
                              point_size = 2, station_label = FALSE){

  census_data <- countyweather::county_centers
  row_num <- which(grepl(fips, census_data$fips))
  title <- census_data[row_num, "name"]

  county_outlines <- countyweather::county_outlines
  colnames(county_outlines)[3] <- "fips_codes"
  outline_df <- county_outlines %>%
    dplyr::filter_( ~ fips_codes == fips)

  county <- suppressMessages(get_map(c(hourly_data$lon_center,
                                       hourly_data$lat_center), zoom = 9,
                                     color = "bw"))

  map <- ggmap(county) + geom_path(aes(lon, lat), data = outline_df,
                                   inherit.aes = FALSE)

  r <- hourly_data$radius
  r_lat <- r / 110.574
  r_lon <- r / 111.320*(cos(r_lat))

  x_c <- hourly_data$lon_center
  y_c <- hourly_data$lat_center

  x_v <- sapply(0:360, function(x) r_lon*cos(x) + x_c)
  y_v <- sapply(0:360, function(y) r_lat*sin(y) + y_c)

  df <- cbind(x_v, y_v)
  df <- as.data.frame(df)

  station_df <- subset(hourly_data$station_df, !duplicated(station))

  if(station_label == TRUE){
    map_out <- map + geom_polygon(aes(x_v, y_v), data = df, inherit.aes = FALSE,
                              fill = "#9999CC", alpha = 0.25) +
      geom_point(data = station_df,
                 aes(longitude, latitude), colour = point_color,
                 size = point_size) +
      theme(legend.position = "none") +
      ggtitle(title) + geom_text(data = station_df,
                                 aes(longitude, latitude, label = station),
                                 vjust = 1.3,
                                 fontface = "bold",
                                 inherit.aes = F)
  } else {
    map_out <- map + geom_polygon(aes(x_v, y_v), data = df, inherit.aes = FALSE,
                              fill = "#9999CC", alpha = 0.25) +
      geom_point(data = station_df,
                 aes(longitude, latitude), colour = point_color,
                 size = point_size) +
      theme(legend.position = "none") +
      ggtitle(title)
  }

  return(map_out)

}
