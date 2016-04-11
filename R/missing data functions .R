# these need to be re-worked b/c of re-worked fips_stations() and
# weather_fips() functions.

# functions to check out how many rows have missing data - maybe not useful ???
# BUT maybe helpful to figure out what to do about fips with multiple stations
# (average vs. choose one)
# ALSO notes/code about averaging across stations for a particular fips

# Given a particular fips code, this returns the percent of rows with missing
# data in the corresponding weather data frame
na_fips <- function(fips){
  a <- weather_fips(fips)
  b <- na.omit(a)
  percent <- (nrow(b)/nrow(a))
  out <- data.frame("FIPS" = fips,
                    "Percent_NA" = (1-percent)*100)
  return(out)
}



# Given a particular fips, this returns the percent of rows with missing data
# in the corresponding weather data frame per weather station
na_stations <- function(fips){
  a <- weather_fips(fips)
  a_st <- c(unique(a$id))
  for(i in 1:length(a_st)){
    perc_st <- (nrow(na.omit(subset(a, id == a_st[i]))) /
                  nrow(subset(a, id == a_st[i])))
    out <- data.frame("FIPS" = fips, "id" = a_st[i], "Percent_NA" =
                        (1-perc_st)*100)
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



#cutpoint ~ 5%

# for 01073, three of the six stations have 100% of their rows with missing
# data. We either want to average the remaining three, or choose one of them.

# averaging:

#test <- weather_fips("01073")
#na_stations("01073")
#test_st <- filter(test, id == "USC00010764" | id == "USC00016478" | id ==
#                    "USW00013876")

#test_avg <- ddply(test_st, .(fips, year, month, day), colwise(mean, .(PRCP_mm,
#              TMAX_C, TMAX_F, TMIN_C, TMIN_F)))


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


