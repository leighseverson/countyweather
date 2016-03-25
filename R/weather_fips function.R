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
#' @param fips A vector of U.S. FIPS codes in numeric or factor format.
#'
#' @examples
#'ex <- c(36081, 36085, 36087, 36119, 40017)
#'ex_df <- weather_fips(ex)

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

  tot_dat <- dplyr::select(tot_dat, fips, id, year, month, day, PRCP_mm,
                           TMAX_C, TMAX_F, TMIN_C, TMIN_F)
  return(tot_dat)
}

# QUESTIONS/NOTES:
# 1. how to reference two packages for one line of code
# for example,
# tot_dat <- dplyr::mutate(tot_dat, TMAX_F = celsius.to.fahrenheit(T.celsius =
#                                                             tot_dat$TMAX_C,
#                                                           round = 0))
# uses both dplyr and weathermetrics
#
# 2. we have daily info for tmax, tmin, and precip - do we want more averaged
# info? (avg yearly rainfall?) - do this by hand?
#
# 3. testing this function with fips <- c(36081, 36085, 36087, 36119, 40017) -
# values are missing for 40017 - this stations doesn't have precipitation
# info? - Need to see how many of our fips codes will have missing values w/
# this function.
#
# 4. potential problem with spread() step - why is this reordering the fips
# codes
#
# 5. "No encoding supplied: defaulting to UTP-8."
#
# 6. stations_per_county() worked with all 222 medicare fips, this function
# did not
#
# 7. want weather info for each county in a separate file? (vs. one huge
# dataframe)

# functions to check out how many rows have missing data - maybe not useful ???

# Given a particular fips code, this returns the percent of rows with missing
# data in the corresponding weather data frame
na_fips <- function(fips){
  a <- weather_fips(fips)
  b <- na.omit(a)
  percent <- (nrow(b)/nrow(a))
  out <- data.frame("FIPS" = fips,
                    "Percent_NA" = 1-percent)
  return(out)
}

x <- na_fips("01073")

# Given a particular fips, this returns the percent of rows with missing data
# in the corresponding weather data frame per weather station
na_stations <- function(fips){
  a <- weather_fips(fips)
  a_st <- c(unique(a$id))
  for(i in 1:length(a_st)){
    perc_st <- (nrow(na.omit(subset(a, id == a_st[i]))) /
                  nrow(subset(a, id == a_st[i])))
    st_col <- paste0("FIPS:", fips, " IDs")
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

y <- na_stations("01073")

# same as na_fips() but for a vector of fips
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

fvec <- c("01073", "01089", "01097", "01101", "02020", "04013")
ok <- na_fips_fun(fvec)

# same as na_stations() but for a vector of fips
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

ok2 <- na_st_fun(fvec)
