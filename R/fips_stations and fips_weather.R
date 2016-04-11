#' List of weather stations per county.
#'
#' \code{fips_stations} returns a dataframe with the names of GHCND weather
#' stations for each U.S. county present in its arguments.
#'
#' A NOAA Token is required to use this function, which interacts with the NCDC
#' API. Request a Token from here: \url{http://www.ncdc.noaa.gov/cdo-web/token}.
#' Then run the code \code{options(noaakey = "your key")} before using this
#' function.
#'
#' @param fips A vector of U.S. FIPS codes in numeric or factor format.
#'
#' @note A fips code could get excluded from the final dataframe because its weather
#' stations:
#' \itemize{
#' \item don't exist,
#' \item don't start after Jan 1, 1999,
#' \item don't start before Dec. 31, 2012, and/or
#' \item don't have a \% coverage of at least 0.9.
#' }

fips_stations <- function(fips){
  vec <- as.data.frame(fips)
  # add column with fips codes in 'FIPS:#####' format for ncdc_stations function
  vec <- dplyr::mutate(vec, FIPS = paste0("FIPS:", fips))

  # Create a dataframe that joins all the dataframes from `rnoaa` calls for
  # different fips together
  for(i in 1:length(fips)){
    # the max daily limit of 1000 for this function is a potential prob.
    df <- rnoaa::ncdc_stations(datasetid = 'GHCND', locationid = vec$FIPS[i],
                               limit = 1000)$data %>%
      dplyr::mutate(fips = fips[i])
    if(i == 1){
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
  return(tot_df)
}

# NOTES: This is just the stations_per_county function cut short - used for
# fips_weather function. Maybe should combine this function into that one...


#' Daily precipiation, maximum and minimum temperatures per county.
#'
#' \code{fips_stations} returns a dataframe showing daily maximum and minimum
#' temperatures from 1999-2012 for each U.S. county present in its arguments.
#'
#' A NOAA Token is required to use this function, which interacts with the NCDC
#' API. Request a Token from here: \url{http://www.ncdc.noaa.gov/cdo-web/token}.
#' Then run the code \code{options(noaakey = "your key")} before using this
#' function.
#'
#' @param fips_df A dataframe with two columns: U.S. FIPS codes and
#' corresponding GHCND weather stations. This dataframe can be obtained with the
#' \code{fips_stations} function.
#'
#' @examples
#' ex_df <- data.frame(id = c("GHCND:USW00003856", "GHCND:USC00012172",
#'                            "GHCND:USW00013894"), fips = c("01089", "01097",
#'                                                           "01097"))
#' weather_ex <- weather_fips(ex_df)
#' head(weather_ex)
weather_fips <- function(fips_df){
  # remove "GHCND:" from station id
  fips_df$id <- gsub("GHCND:", "", fips_df$id)

  for(i in 1:length(fips_df$id)){
    # get weather info for each station
    dat <- rnoaa::ghcnd(stationid = fips_df$id[i])$data
    # relevant dates
    dat <- dplyr::filter(dat, year >= 1999 & year <= 2012)
    # combine different stations into one df
    if(i == 1){
      tot_dat <- dat
    } else {
      tot_dat <- rbind(tot_dat, dat)
    }
  }
  # reshape df
  tot_dat <- tidyr::gather(tot_dat, col, value, -id, -year, -month, -element)
  tot_dat <- tidyr::spread(tot_dat, element, value)
  tot_dat <- dplyr::select(tot_dat, id, year, month, col, PRCP, TMAX, TMIN)

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
  # subset to isolate only weather values
  tot_dat <- subset(tot_dat, col2 == "VALUE")
  tot_dat <- dplyr::select(tot_dat, id, year, month, day, PRCP, TMAX, TMIN)

  tot_dat$PRCP <- as.integer(tot_dat$PRCP)
  tot_dat$TMAX <- as.integer(tot_dat$TMAX)
  tot_dat$TMIN <- as.integer(tot_dat$TMIN)

  # TMAX is in "tenths of a degree C"
  tot_dat <- dplyr::mutate(tot_dat, TMAX_C = (tot_dat$TMAX)/10)
  tot_dat <- dplyr::mutate(tot_dat, TMAX_F = celsius.to.fahrenheit(T.celsius =
                                                                     tot_dat$TMAX_C,
                                                                   round = 0))
  # same with TMIN
  tot_dat <- dplyr::mutate(tot_dat, TMIN_C = (tot_dat$TMIN)/10)
  tot_dat <- dplyr::mutate(tot_dat, TMIN_F = celsius.to.fahrenheit(T.celsius =
                                                                     tot_dat$TMIN_C,
                                                                   round = 0))
  # PRCP is in 10ths of mm
  tot_dat <- dplyr::mutate(tot_dat, PRCP_mm = (tot_dat$PRCP)/10)

  # add in fips codes
  tot_dat <- dplyr::full_join(tot_dat, fips_df, by = "id")

  # reorder columns
  tot_dat <- tot_dat[ , c(13, 1, 2, 3, 4, 5, 12, 6, 8, 9, 7, 10, 11)]
  tot_dat <- dplyr::select(tot_dat, fips, id, year, month, day, PRCP_mm,
                           TMAX_C, TMAX_F, TMIN_C, TMIN_F)
  return(tot_dat)
}

test <- weather_fips(fips_df)

# QUESTIONS:
# 1. how to reference two packages for one line of code
# tot_dat <- dplyr::mutate(tot_dat, TMAX_F = celsius.to.fahrenheit(T.celsius =
#                                                             tot_dat$TMAX_C,
#                                                           round = 0))
# uses both dplyr and weathermetrics
#
# 2. we have daily info for tmax, tmin, and precip - do we want more averaged
# info? (avg yearly rainfall?) - do this by hand?
