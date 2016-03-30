#' Create county-specific weather datasets for US counties
#'
#' This function pulls daily precipiation, maximum and minimum temperatures for
#' each county requested.
#'
#' \code{fips_stations} returns a dataframe showing daily maximum and minimum
#' temperatures from 1999-2012 for each U.S. county present in its arguments.
#'
#' A NOAA Token is required to use this function, which interacts with the NCDC
#' API. Request a Token from here: \url{http://www.ncdc.noaa.gov/cdo-web/token}.
#' Then run the code \code{options(noaakey = "your key")} before using this
#' function.
#'
#' @param fips A vector of U.S. FIPS codes in numeric or factor format.
#'
#' @examples
#' \dontrun{
#'  ex <- c(36081, 36085, 36087, 36119, 40017)
#'  ex_df <- weather_fips(ex)
#' }
#'
#' @export
weather_fips <- function(fips){
  vec <- as.data.frame(fips)
  # add column with fips codes in 'FIPS:#####' format for ncdc_stations function
  vec <- dplyr::mutate(vec, FIPS = paste0("FIPS:", fips))

  # Create a dataframe that joins all the dataframes from `rnoaa` calls for
  # different fips together
  for (i in 1:length(fips)) {
    # the max daily limit of 1000 for this function is a potential prob.
    df <- rnoaa::ncdc_stations(datasetid = 'GHCND', locationid = vec$FIPS[i],
                               limit = 1000)$data %>%
      dplyr::mutate(fips = fips[i])
    if (i == 1) {
      tot_df <- df
    } else {
      tot_df <- rbind(tot_df, df)
    }
  }

  # changing mindate and maxdate columns in station dataframe to date format
  tot_df <- dplyr::mutate(tot_df,
                          mindate = lubridate::ymd(mindate),
                          maxdate = lubridate::ymd(maxdate)) %>%
    dplyr::filter(maxdate >= "2012-12-31" & mindate <= "1999-01-01") %>%
    dplyr::filter(datacoverage >= 0.90)

  tot_df$fips <- as.factor(tot_df$fips)

  tot_df <- dplyr::select(tot_df, id, fips)

  # remove "GHCND:" from station id
  tot_df$id <- gsub("GHCND:", "", tot_df$id)

  for (i in 1:length(tot_df$id)) {
    # get weather info for each station
    dat <- rnoaa::ghcnd(stationid = tot_df$id[i])$data
    # relevant dates
    dat <- dplyr::filter(dat, year >= 1999 & year <= 2012)
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
  tot_dat <- tot_dat %>% tidyr::gather(col, measurement, -id, -year, -month,
                                       -element, -fips)
  # filter to isolate only relevant weather values
  tot_dat <- tot_dat %>% dplyr::filter(element == "PRCP" | element == "TMAX" |
                                         element == "TMIN")

  # move PRCP, TMAX, TMIN to be column headings
  # this step moves around the order of fips codes in tot_dat - unclear why
  #
  # tried defining fips as ordered factor (makes no differnce)

  tot_dat$fips <- factor(tot_dat$fips, levels = fips, ordered = TRUE)

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

#' Determine percent missing data for a FIPs code
#'
#' Given a particular fips code, this returns the percent of rows with missing
#' data in the corresponding weather data frame
#'
#' @inheritParams weather_fips
#'
#' @examples
#' \dontrun{
#' x <- na_fips("01073")
#' }
#'
#' @export
na_fips <- function(fips){
  a <- weather_fips(fips)
  b <- na.omit(a)
  percent <- (nrow(b) / nrow(a))
  out <- data.frame("FIPS" = fips,
                    "Percent_NA" = 1 - percent)
  return(out)
}

#' Given a particular fips, this returns the percent of rows with missing data
#' in the corresponding weather data frame per weather station
#'
#' @inheritParams weather_fips
#'
#' @note For 01073, three of the six stations have 100% of their rows with
#'    missing data. We either want to average the remaining three, or choose one
#'    of them.
#'
#' @examples
#' \dontrun{
#' y <- na_stations("01073")
#'
#' # averaging:
#' test <- weather_fips("01073")
#' na_stations("01073")
#' test_st <- filter(test, id == "USC00010764" | id == "USC00016478" | id ==
#'                     "USW00013876")
#'
#' test_avg <- plyr::ddply(test_st, .(fips, year, month, day),
#'                         colwise(mean, .(PRCP_mm,
#'                                         TMAX_C, TMAX_F, TMIN_C, TMIN_F)))
#' }
#'
#' @export
na_stations <- function(fips){
  a <- weather_fips(fips)
  a_st <- c(unique(a$id))
  for(i in 1:length(a_st)){
    perc_st <- (nrow(na.omit(subset(a, id == a_st[i]))) /
                  nrow(subset(a, id == a_st[i])))
    out <- data.frame("FIPS" = fips, "id" = a_st[i], "Percent_NA" = 1-perc_st)
    if(i == 1){
      dat <- out
    } else {
      dat <- rbind(dat, out)
    }
    if(length(dat$id) == length(a_st)){
      dat_final <- dat
    }
  }
  return(dat_final)
}

#'
#' This function performs the same task as \code{\link{na_fips}} but for a
#' vector of counties (identified with FIPS codes) rather than a single county.
#'
#' @param fvec A numeric or character vector of FIPS codes for US counties.
#'
#' @examples
#' \dontrun{
#'  fvec <- c("01073", "01089", "01097", "01101", "02020", "04013")
#'  ok <- na_fips_fun(fvec)
#' }
#'
#' @export
na_fips_fun <- function(fvec){
  for(i in 1:length(fvec)){
    missing <- na_fips(fvec[i])
    if(i == 1){
      df <- missing
    } else {
      df <- rbind(df, missing)
    }
    if(length(df$FIPS) == length(fvec)){
      df_final <- df
    }
  }
  return(df_final)
}

#' This function does the same thing as \code{\link{na_stations}}, but for a
#' vector counties (listed using their FIPS codes) rather than a single county.
#'
#' @examples
#' \dontrun{
#'  ok2 <- na_st_fun(fvec)
#' }
na_st_fun <- function(fvec){
  for(i in 1:length(fvec)){
    missing <- na_stations(fvec[i])
    if(i == 1){
      df <- missing
    } else {
      df <- rbind(df, missing)
    }
  }
  return(df)
}

