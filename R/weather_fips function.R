#' NOAA NCDC station IDs per county.
#'
#' \code{fips_stations} returns a dataframe showing NOAA NCDC station IDs for
#' each U.S. county present in its arguments. This function has options to
#' filter stations based on start and end date of available data, as well as
#' percent of data coverage.
#'
#' A NOAA Token is required to use this function, which interacts with the NCDC
#' API. Request a Token from here: \url{http://www.ncdc.noaa.gov/cdo-web/token}.
#' Then run the code \code{options(noaakey = "your key")} before using this
#' function.
#'
#' @param fips A vector of U.S. FIPS codes in numeric or factor format.
#' @param date_min Accepts date in character, ISO format ("yyyy-mm-dd"). The
#' dataframe returned will include only stations that have data for dates
#' including and after the specified date.
#' @param date_max Accepts date in character, ISO format ("yyyy-mm-dd"). The
#' dataframe returned will include only stations that have data for dates
#' including and before the specified date.
#' @param data_coverage A numerical value ranging from 0 to 1. The dataframe
#' returned will include only stations that have data coverage equal to or
#' greater than the specified fraction.
#'
#' @examples
#' \dontrun{
#'  fips <- c(36081, 36085, 36087, 36119, 40017)
#'  ex_df <- fips_stations(fips)
#'
#' fips_stations(c("01073", "01089", "01097"),
#'    date_min = "1999-01-01", date_max = "2012-12-31",
#'    data_coverage = 0.9)
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
fips_stations <- function(fips, date_min = NULL, date_max = NULL,
                         data_coverage = 0){

  vec <- as.data.frame(fips) %>%
    dplyr::mutate(FIPS = paste0("FIPS:", fips))

  # Create a dataframe that joins all the dataframes from `rnoaa` calls for
  # different fips together
  for (i in 1:length(fips)) {
    fip_stations <- rnoaa::ncdc_stations(datasetid = 'GHCND',
                                         locationid = vec$FIPS[i],
                               limit = 10)
    df <- fip_stations$data
    if(fip_stations$meta$totalCount > 10){
      how_many_more <- fip_stations$meta$totalCount - 10
      more_stations <- rnoaa::ncdc_stations(datasetid = 'GHCND',
                                            locationid = vec$FIPS[i],
                                            limit = how_many_more,
                                            offset = 10 + 1)
      df <- rbind(df, more_stations$data)
    }
    df <- dplyr::mutate(df, fips = fips[i])
    if (i == 1) {
      tot_df <- df
    } else {
      tot_df <- rbind(tot_df, df)
    }
  }

  # If either `min_date` or `max_date` option was null, set to a date that
  # will keep all monitors in the filtering.
  if(is.null(date_max)){
    date_max <- min(tot_df$maxdate)
  }
  if(is.null(date_min)){
    date_min <- max(tot_df$mindate)
  }

  date_max <- lubridate::ymd(date_max)
  date_min <- lubridate::ymd(date_min)

  tot_df <- dplyr::mutate(tot_df,
                          mindate = lubridate::ymd(mindate),
                          maxdate = lubridate::ymd(maxdate)) %>%
    dplyr::filter(maxdate >= date_max & mindate <= date_min) %>%
    dplyr::filter(datacoverage >= data_coverage) %>%
    dplyr::select(id, fips) %>%
    dplyr::mutate(id = gsub("GHCND:", "", id))

  return(tot_df)
}

#' Daily precipiation, maximum and minimum temperatures per county.
#'
#' \code{weather_fips} returns a dataframe showing GHCN-Daily weather data
#' (precipitation in mm, and maximum and minumum temperatures in fahrenheit and
#' celsius), given a dataframe of NOAA NCDC station IDs and corresponding U.S.
#' county fips codes. (This dataframe could be obtained with the
#' \code{fips_stations} function.)
#'
#' A NOAA Token is required to use this function, which interacts with the NCDC
#' API. Request a Token from here: \url{http://www.ncdc.noaa.gov/cdo-web/token}.
#' Then run the code \code{options(noaakey = "your key")} before using this
#' function.
#'
#' @param station_fips_df A dataframe containing NCDC station IDs in character
#' format ("USC00010764", for example) and corresponding fips codes in factor
#' format. This dataframe coulld be obtained for a given vector of fips codes
#' with the \code{fips_stations} function.
#'
#' @examples
#' \dontrun{
#' ex_df <- fips_stations(c("01073", "01089", "01097"),
#'          date_min = "1999-01-01",
#'          date_max = "2012-12-31", data_coverage = 0.9)
#' weather_data <- weather_fips(ex_df)
#' head(weather_data)
#'}
weather_fips <- function(station_fips_df){
  tot_df <- station_fips_df
  for (i in 1:length(tot_df$id)) {
    # get weather info for each station
    dat <- rnoaa::ghcnd(stationid = tot_df$id[i])$data
    # relevant dates
    dat <- dplyr::filter(dat, year >= lubridate::year(min_date) &
                           year <= lubridate::year(max_date))
    # combine different stations into one df
    if (i == 1) {
      tot_dat <- dat
    } else {
      tot_dat <- rbind(tot_dat, dat)
    }
  }

  # add in fips codes
  tot_dat <- dplyr::full_join(tot_dat, tot_df, by = "id")

  # reshape df
  # gather all variables expect for id, year, month, element, and fips, with
  # key variable = col and value variable = measurement
  ### Can we use `select` for all of this instead of gathering and spreading?
  tot_dat <- tot_dat %>% tidyr::gather(col, measurement, -id, -year, -month,
                                       -element, -fips)
  # filter to isolate only relevant weather values
  tot_dat <- tot_dat %>% dplyr::filter(element == "PRCP" | element == "TMAX" |
                                         element == "TMIN")

  # move PRCP, TMAX, TMIN to be column headings
  # this step moves around the order of fips codes in tot_dat - unclear why
  #
  # tried defining fips as ordered factor (makes no differnce)

  # tot_dat$fips <- factor(tot_dat$fips, levels = fips, ordered = TRUE)

  # for c(36081, 36085, 36087, 36119, 40017):
  # weather info for 36119 are at the head and tail of the df, but rows
  # for 36081 and 40017 are still in there
  # (no stations for 36085 or 36087)
  tot_dat <- tot_dat %>% tidyr::spread(element, measurement)

  tot_dat <- dplyr::select(tot_dat, fips, id:month, col:TMIN)

  # isolate col = VALUE only (remove MFLAG, QFLAG, and SFLAG)
  # change col from "VALUE1" to "VALUE 1"
  tot_dat$col <- gsub('(\\D+)(\\d+)', '\\1 \\2', tot_dat$col)
  # change "VALUE 1" to two columns
  tot_dat <- dplyr::mutate(tot_dat, col2 = col)
  # remove digit and space at end
  tot_dat$col2 <- gsub('\\d', "", tot_dat$col2)
  tot_dat$col2 <- gsub('\\s', "", tot_dat$col2)
  tot_dat <- dplyr::mutate(tot_dat, day = col)
  # remove letters -> day of month
  tot_dat$day <- gsub('\\D', "", tot_dat$day)

  tot_dat <- subset(tot_dat, col2 == "VALUE")
  tot_dat <- dplyr::select(tot_dat, fips, id, year, month, day, PRCP, TMAX,
                           TMIN)

  tot_dat$PRCP <- as.integer(tot_dat$PRCP)
  tot_dat$TMAX <- as.integer(tot_dat$TMAX)
  tot_dat$TMIN <- as.integer(tot_dat$TMIN)

  # change missing values coded as -9999 to NA
  tot_dat[tot_dat == -9999] <- NA

  # TMAX is in "tenths of a degree C"
  tot_dat <- dplyr::mutate(tot_dat, TMAX_C = (tot_dat$TMAX)/10)

  tot_dat <- dplyr::mutate(tot_dat,
                           TMAX_F = weathermetrics::celsius.to.fahrenheit(
                             T.celsius = tot_dat$TMAX_C, round = 0))
  # same with TMIN
  tot_dat <- dplyr::mutate(tot_dat, TMIN_C = (tot_dat$TMIN)/10)
  tot_dat <- dplyr::mutate(tot_dat,
                           TMIN_F = weathermetrics::celsius.to.fahrenheit(
                             T.celsius = tot_dat$TMIN_C, round = 0))

  # PRCP is in 10ths of mm
  tot_dat <- dplyr::mutate(tot_dat, PRCP_mm = (tot_dat$PRCP)/10)

  tot_dat <- dplyr::select(tot_dat, fips, id, year, month, day, PRCP_mm,
                           TMAX_C, TMAX_F, TMIN_C, TMIN_F)

  # retain date order
  tot_dat$year <- factor(tot_dat$year, levels = c(1999:2012), ordered = TRUE)
  tot_dat$month <- factor(tot_dat$month, levels = c(1:12), ordered = TRUE)
  tot_dat$day <- factor(tot_dat$day, levels = c(1:31), ordered = TRUE)

  return(tot_dat)
}
