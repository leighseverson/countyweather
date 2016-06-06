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
#'    example, var = c("wind_speed", "temperature"). (Optional.)
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
#' @references For more information on this dataset and available weather
#' variables, see
#' \url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-format-document.pdf}.
#'
#' @examples
#' \dontrun{
#' df <- hourly_fips_df(fips = "12086", year = 1992, var = c("wind_speed",
#'                      "temperature"))
#' }
#'
#' @export
hourly_fips_df <- function(fips, year, var = "all", radius = 50,
                           coverage = NULL){
  data <- isd_monitors_data(fips = fips, year = year, var = var, radius =
                               radius)
  if(!purrr::is_null(coverage)){
    data <- filter_hourly(hourly_data = data, coverage = coverage, var = var)
  }
  averaged <- ave_hourly(data)
  return(averaged)
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
#'
#' @export
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
                                 c(99, 999, 999.9, 9999, 9999.9, 99999, 99999.9)]
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
#'
#' @export
isd_monitors_data <- function(fips, year, var = "all", radius = 50){
  ids <- isd_fips_stations(fips, verbose = FALSE, radius = radius)

  safe_int <- purrr::safely(int_surface_data)
  mult_stations <- mapply(safe_int, usaf_code = ids$usaf,
                          wban_code = ids$wban,
                          year = year, var = list(var = var))

  good_st <- sapply(mult_stations, function(x) !is.null(dim(x)))
  if(sum(good_st) > 0){
    st_out_list <- lapply(which(good_st), function(x) mult_stations[[x]])
    st_out_df <- dplyr::bind_rows(st_out_list)
  } else(
    stop("None of the stations had available data.")
  )

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
#'
#' @export
ave_hourly <- function(hourly_data){

  df <- dplyr::mutate_(hourly_data, id = ~ paste0(usaf_station, wban_station))
  df <- dplyr::select_(df, .dots = c("-usaf_station", "-wban_station",
                                     "-latitude", "-longitude"))

  averaged_data <- tidyr::gather(df, key, value, -id, -date_time) %>%
    dplyr::group_by_(~ date_time, ~ key) %>%
    dplyr::summarize_(mean = ~ mean(value, na.rm = TRUE)) %>%
    tidyr::spread(key = key, value = mean)

  n_reporting <- tidyr::gather(df, key, value, -id, -date_time) %>%
    dplyr::group_by_(~ date_time, ~ key) %>%
    dplyr::summarize_(n_reporting = ~ sum(!is.na(value))) %>%
    dplyr::mutate_(key = ~ paste(key, "reporting", sep = "_")) %>%
    tidyr::spread(key = key, value = n_reporting)

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
#'
#' @return a \code{dataframe} with stations that meet the specified coverage
#' requirements for weather variables included in the datafrome present in
#' this function's arguments.
#'
#' @export
filter_hourly <- function(hourly_data, coverage, var){

  ex <- hourly_data %>%
    unite(station, usaf_station, wban_station, sep = "-") %>%
    select(-date_time, -latitude, -longitude) %>%
    gather(key, value, -station) %>%
    group_by(station, key) %>%
    summarize(coverage = mean(!is.na(value)))
  ex2 <- hourly_data %>%
    unite(station, usaf_station, wban_station, sep = "-") %>%
    select(-latitude, -longitude) %>%
    gather(key, value, -station, -date_time) %>%
    left_join(ex, by = c("station", "key")) %>%
    filter(coverage > 0.80) %>%
    group_by(date_time, key) %>%
    summarize(value = mean(value, na.rm = TRUE)) %>%
    spread(key = key, value = value)

  df <- hourly_data
  # add a single identifier for each station
  df <- dplyr::mutate_(df, id = ~ paste0(usaf_station, wban_station))

  # calculate number of missing observations for each station and each variable
  dplyr::group_by_(df, ~id) %>%
    dplyr::do({
      n_missing <- as.data.frame(do.call("rbind",
                                         sapply(var,
                                                FUN = function(i) sum(is.na(df[,i])),
                                                         simplify = FALSE)))
    }) -> m_df

  for(i in 1:length(var)){
    indexes <- seq(i, length(m_df$id), length(var))
    if(i == 1){
      ind_out <- indexes
    } else {
      ind_out <- rbind(ind_out, indexes)
    }
    ind_out <- t(ind_out)
  }
  colnames(ind_out) <- var

  m_df <- dplyr::add_rownames(m_df)
  m_df <- dplyr::mutate_(m_df, variable = ~ NA)

  for(i in 1:length(var)){
    a <- dplyr::filter_(m_df, ~ rowname %in% ind_out[,i])
    a$variable <- var[i]
    if(i == 1){
      missing_out <- a
    } else {
      missing_out <- rbind(missing_out, a)
    }
  }
  colnames(missing_out) <- c("rowname", "id", "missing", "variable")

  # calculate total number of observations for each station and weather variable
  dplyr::group_by_(df, ~id) %>%
    dplyr::do({
      n_total <- as.data.frame(do.call("rbind",
                                       sapply(var, FUN = function(i) nrow(df[,i]),
                                                       simplify = FALSE)))
    }) -> t_df

  t_df <- dplyr::add_rownames(t_df)
  t_df <- dplyr::mutate_(t_df, variable = ~ NA)

  for(i in 1:length(var)){
    b <- dplyr::filter_(t_df, ~ rowname %in% ind_out[,i])
    b$variable <- var[i]
    if(i == 1){
      total_out <- b
    } else {
      total_out <- rbind(total_out, b)
    }
  }
  colnames(total_out) <- c("rowname", "id", "total", "variable")
  total_out <- dplyr::select_(total_out, .dots = c("rowname", "total"))

  # calculate percent coverage
  coverage_data <- dplyr::full_join(missing_out, total_out, by = "rowname")
  coverage_data <- dplyr::mutate_(coverage_data,
                                  covered = ~ 1 - (missing/total))
  coverage_data$covered <- as.numeric(coverage_data$covered)

  coverage_data <- dplyr::mutate_(coverage_data, uniqueid = ~ paste0(id, variable))
  coverage_df <- dplyr::select_(coverage_data,
                                .dots = c("covered", "uniqueid", "id", "variable"))

  # only filter if minimum covered value is < specified coverage
  if(min(coverage_df$covered) < coverage){
    df_g <- tidyr::gather(df, key, value, -usaf_station,
                          -wban_station, -date_time, -id,
                          -latitude, -longitude)
    df_g <- dplyr::mutate_(df_g, uniqueid = ~ paste0(id, key))

    df_c <- dplyr::full_join(coverage_df, df_g, by = "uniqueid")
    df_filtered <- dplyr::filter_(df_c, ~ covered >= coverage)

    df_filtered <- dplyr::select_(df_filtered, .dots = c("-uniqueid"))

    df_s <- tidyr::spread(df_filtered, key, value)
    df_out <- dplyr::select_(df_s, .dots = c("-covered"))

    low <- coverage_df[!coverage_df$covered >= coverage, ]
    low <- unique(low)

    a <- dplyr::select_(df, .dots = c("id", "usaf_station", "wban_station"))
    ids <- unique(a$id)

    for(i in 1:length(ids)){
        b <- dplyr::filter_(a, ~ id == ids[i])
        b <- b[1,]
        if(i == 1){
          out <- b
        } else {
          out <- rbind(out, b)
        }
    }

    removed <- dplyr::left_join(low, out, by = "id")

    message <- paste0("The following variables from the following NOAA",
                        " ISD weather stations were not included:")
    print(message)
      for(i in 1:nrow(removed)){
        info <- paste0(removed$variable[i], " from NOAA ISD station with USAF number ",
                       removed$usaf_station[i], " and WBAN number ", removed$wban_station[i],
                       " due to low coverage of ", removed$covered[i])
        print(info)
      }

  } else {
    df_out <- hourly_data
  }

  return(df_out)
}
