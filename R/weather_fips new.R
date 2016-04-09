# 1. get station list for 1 FIPS

# this is the fips_station function from the weather_fips function.R

county_stations <- function(fips, date_min = NULL, date_max = NULL,
                            data_coverage = 0){

  FIPS <- paste0('FIPS:', fips)
  station_ids <- rnoaa::ncdc_stations(datasetid = 'GHCND', locationid = FIPS,
                                      limit = 10)
  df <- station_ids$data
  if(station_ids$meta$totalCount > 10){
    how_many_more <- station_ids$meta$totalCount - 10
    more_stations <- rnoaa::ncdc_stations(datasetid = 'GHCND',
                                          locationid = FIPS,
                                          limit = how_many_more,
                                          offset = 10 + 1)
    df <- rbind(df, more_stations$data)
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

  tot_df <- dplyr::mutate(df,
                          mindate = lubridate::ymd(mindate),
                          maxdate = lubridate::ymd(maxdate)) %>%
    dplyr::filter(maxdate >= date_max & mindate <= date_min) %>%
    dplyr::filter(datacoverage >= data_coverage) %>%
    dplyr::select(id) %>%
    dplyr::mutate(id = gsub("GHCND:", "", id))

  vec <- as.vector(tot_df$id)
  return(vec)
}

# 2. use meteo_pull_monitor - get tidy full dataset for all stations

# clean_daily and meteo_pull_monitors functions from helpers_ghcnd.R

clean_daily <- function(ghcnd_data, keep_flags = FALSE){
  if(keep_flags){
    cleaned_df <- dplyr::filter(ghcnd_data$data,
                                element %in% c("TAVG", "TMAX", "TMIN", "PRCP",
                                               "SNOW", "SNWD", "AWND",
                                               "WSFG")) %>%
      tidyr::gather(what, value, -id, -year, -month, -element) %>%
      dplyr::mutate(day = as.numeric(gsub("[A-Z]", "", what)),
                    what = gsub("[0-9]", "", what),
                    what = paste(tolower(element), tolower(what), sep = "_"),
                    what = gsub("_value", "", what),
                    value = ifelse(value == -9999, NA, as.character(value))) %>%
      dplyr::mutate(date = suppressWarnings(
        lubridate::ymd(paste0(year, sprintf("%02s", month),
                              sprintf("%02s", day))))) %>%
      dplyr::filter(!is.na(date)) %>%
      dplyr::select(id, date, what, value) %>%
      tidyr::spread(what, value) %>%
      dplyr::arrange(date)
  } else {
    cleaned_df <- dplyr::filter(ghcnd_data$data,
                                element %in% c("TAVG", "TMAX", "TMIN", "PRCP",
                                               "SNOW", "SNWD", "AWND",
                                               "WSFG")) %>%
      dplyr::select(-matches("FLAG")) %>%
      tidyr::gather(what, value, -id, -year, -month, -element) %>%
      dplyr::mutate(day = as.numeric(gsub("[A-Z]", "", what)),
                    what = gsub("[0-9]", "", what),
                    what = paste(tolower(element), tolower(what), sep = "_"),
                    what = gsub("_value", "", what),
                    value = ifelse(value == -9999, NA, as.character(value))) %>%
      dplyr::mutate(date = suppressWarnings(
        lubridate::ymd(paste0(year, sprintf("%02s", month),
                              sprintf("%02s", day))))) %>%
      dplyr::filter(!is.na(date)) %>%
      dplyr::select(id, date, what, value) %>%
      tidyr::spread(what, value) %>%
      dplyr::arrange(date)
  }
  which_weather_vars <- which(colnames(cleaned_df) %in%
                                c("prcp", "tavg", "tmax", "tmin", "awnd",
                                  "wsfg"))
  cleaned_df <- tbl_df(cleaned_df)
  # All these variables are in tenths of units
  cleaned_df[, which_weather_vars] <- vapply(cleaned_df[, which_weather_vars],
                                             FUN.VALUE = numeric(nrow(cleaned_df)),
                                             FUN = function(x) as.numeric(x) / 10)
  which_snow_vars <- which(colnames(cleaned_df) %in%
                             c("snow", "snwd"))
  cleaned_df[, which_weather_vars] <- vapply(cleaned_df[, which_weather_vars],
                                             FUN.VALUE = numeric(nrow(cleaned_df)),
                                             FUN = function(x) as.numeric(x))
  return(cleaned_df)
}


meteo_pull_monitors <- function(monitors, keep_flags = FALSE){

  monitors <- unique(monitors)
  safe_ghcnd <- purrr::safely(ghcnd)
  all_monitors_ghcnd <- lapply(monitors, safe_ghcnd)

  check_station <- sapply(all_monitors_ghcnd, function(x) is.null(x$result))
  bad_stations <- monitors[check_station]
  if(length(bad_stations) > 0){
    warning(paste("The following stations could not be pulled from",
                  "the GHCN ftp:\n", paste(bad_stations, collapse = ", "),
                  "\nAll other monitors were successfully pulled from GHCN."))
  }

  all_monitors_ghcnd <- lapply(all_monitors_ghcnd[!check_station],
                               function(x) x$result)
  all_monitors_clean <- lapply(all_monitors_ghcnd, clean_daily,
                               keep_flags = keep_flags)
  all_monitors_clean <- suppressWarnings(dplyr::bind_rows(all_monitors_clean))
  return(all_monitors_clean)
}

#3. "coverage" to calculate coverage

# meteo_coverage from meteo_utils.R

meteo_coverage <- function(meteo_df,
                           obs_start_date=NULL,
                           obs_end_date=NULL,
                           verbose=TRUE) {

  if (!is.null(obs_start_date)) {
    meteo_df <- dplyr::filter(meteo_df, as.Date(date) >= obs_start_date)
  }

  if (!is.null(obs_end_date)) {
    meteo_df <- dplyr::filter(meteo_df, as.Date(date) <= obs_end_date)
  }

  dplyr::group_by(meteo_df, id) %>%
    do({
      rng <- range(.$date)
      dat <- data.frame(start_date = rng[1],
                        end_date = rng[2],
                        total_obs = nrow(.), stringsAsFactors=FALSE)
      if (verbose) cat(sprintf("Station Id: %s\n", .$id[1]))
      if (verbose) cat(sprintf("\n  Date range for observations: %s\n\n",
                               paste0(as.character(rng), sep="", collapse=" to ")))
      if (verbose) cat(sprintf("  Total number of observations: %s\n\n", comma(nrow(.))))
      meteo_cols <- setdiff(colnames(.), c("id", "date"))
      col_cov <- lapply(meteo_cols, function(x, n) {
        if (verbose) cat(sprintf("  Column %s completeness: %5s\n",
                                 formatC(sprintf("'%s'", x), width = (n+2)),
                                 percent(sum(!is.na(.[,x])) / nrow(.))))
        sum(!is.na(.[,x])) / nrow(.)
      }, max(vapply(colnames(.), nchar, numeric(1), USE.NAMES=FALSE)))
      if (verbose) cat("\n")
      col_cov <- setNames(cbind.data.frame(col_cov, stringsAsFactors=FALSE), meteo_cols)
      dplyr::bind_cols(dat, col_cov)
    }) -> out
  class(out) <- c("meteo_coverage", class(out))
  if (verbose) return(invisible(out))
  out
}

# 4. filter monitors based on coverage

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


# 5. Averaging by day to get single daily value for FIPS
# SQL - removing rows with missing data

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
#' @param start_date a date in "yyyy-mm-dd" format that is one day before the
#' desired starting date of your dataset. For example, if you wanted your
#' earliest day to be "1999-01-01", you would input "1998-12-31" for
#' \code{start_date}.
#' @param end_date a date in "yyyy-mm-dd" formate that is one day after the
#' desired ending date of your dataset. For example, if you wanted your latest
#' day to be "2012-12-31", you would input "2013-01-01" for \code{start_date}.
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
  df$date <- format(as.POSIXct(df$date, format = '%Y-%m-%d'), format =
                      '%Y-%m-%d')
  df <- subset(df, date > as.Date(start_date))
  df <- subset(df, date < as.Date(end_date))
  return(df)
}

# 6. wrapper

#' Return average daily weather data for a particular county.
#'
#' \code{weather_fips} returns a data.frame of average daily precipitation,
#' maximum and minimum temperature values for a particular county, date range,
#' and specified "coverage."
#'
#' @param fips a U.S. FIPS code in character format
#' @param coverage_val a number in the range of 0 to 1 that specifies the desired
#' coverage.
#' @param min_date a date in "yyyy-mm-dd" format that is one day before the
#' desired starting date of your dataset. For example, if you wanted your
#' earliest day to be "1999-01-01", you would input "1998-12-31" for
#' \code{start_date}.
#' @param end_date a date in "yyyy-mm-dd" formate that is one day after the
#' desired ending date of your dataset. For example, if you wanted your latest
#' day to be "2012-12-31", you would input "2013-01-01" for \code{start_date}.
#' @export
#' @examples
#' \dontrun{
#' df <- weather_fips("06037", 0.90, "1998-12-31", "2013-01-01")
#' }
weather_fips <- function(fips, coverage_val, min_date, max_date){

  # get stations for 1 fips
  stations <- county_stations(fips)

  # get titdy full dataset for all monitors
  monitors <- meteo_pull_monitors(stations)

  # calculate coverage for each variable (prcp, tmax, tmin)
  coverage <- meteo_coverage(monitors, verbose = FALSE)

  # filter station dataset based on specified coverage
  filtered <- filter_coverage(coverage, coverage_val)

  # filter weather dataset based on stations w/ specified coverage
  filtered_data <- filter(monitors, id %in% filtered$id)

  # get rid of unwanted weather variables
  selected_variables <- select(filtered_data, id, date, prcp, tmax, tmin)

  # average across stations, add a column for number of stations that contributed
  # to each daily average
  averaged <- average_weather(selected_variables, "1998-12-31", "2013-01-01")

  return(averaged)
}


# probs
# 1. using tavg tends to kick out all of the stations (based on coverage
# requirements)
# 2. verbose = false
# 3. using SQL - is this sketchy?
# 4. filtering dates: > vs. =>

