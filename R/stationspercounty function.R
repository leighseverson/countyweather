

#' Count of weather stations per county.
#'
#' \code{stationspercounty} returns a plot showing the number of GHCND weather
#' stations for each U.S. county present in its arguments.
#'
#' A NOAA Token is required to use this function, which interacts with the NCDC
#' API. Request a Token from here: \url{http://www.ncdc.noaa.gov/cdo-web/token}.
#' Then run the code \code{options(noaakey = "your key")} before using this
#' function.
#'
#' @param yourvector A vector of U.S. FIPS codes in numberic or factor format.
#' @examples
#'  vec7 <- c(36081, 36085, 36087, 36119, 40017)
#'  stationspercounty(vec7)
stationspercounty <- function(yourvector){
  vec <- as.data.frame(yourvector)
  # add column with fips codes in 'FIPS:#####' format for ncdc_stations function
  vec <- dplyr::mutate(vec, FIPS = "FIPS:")
  for(i in 1:length(vec$FIPS)){
    vec$FIPS[i] <- gsub("$", vec$yourvector[i], vec$FIPS[i])

    # the max daily limit of 1000 for this function is a potential prob.
    df <- rnoaa::ncdc_stations(datasetid = 'GHCND', locationid = vec$FIPS[i],
                               limit = 1000)

    # This if else statement allows the function to keep going even if there's
    # a fips code without any stations
    if(length(df$data[i]) == 0){
      # I want this 'missing' dataframe to contain fips codes without a corresponding
      # station...the code I have isn't doing this
      missing <- yourvector$yourvector[i]
    } else {
      df <- df$data
    }

    # changing mindate and maxdate columns in station dataframe to date format
    df$mindate <- as.factor(df$mindate)
    df$mindate <- as.Date(df$mindate)
    df$maxdate <- as.factor(df$maxdate)
    df$maxdate <- as.Date(df$maxdate)
    max <- subset(df, maxdate < "2012-12-31")
    min <- subset(df, mindate > "1999-01-01")

    # similar goal with the next 3 if else statements. The 'missing' df isn't
    # happening, but the function will keep going if there are fips codes that
    # don't meet the date/station coverage requirements.
    if(length(df[!(df$id %in% max$id), ]) == 0){
      missing_maxdate <- yourvector$yourvector[i]
    } else {
      df <- df[!(df$id %in% max$id), ]
    }

    if(length(df[!(df$id %in% min$id), ]) == 0){
      missing_mindate <- yourvector$yourvector[i]
    } else {
      df <- df[!(df$id %in% min$id), ]
    }

    coverage <- subset(df, datacoverage < 0.9)
    if(length(df[!(df$id %in% coverage$id), ]) == 0){
      missing_coverage <- yourvector$yourvector[i]
    } else {
      df <- df[!(df$id %in% coverage$id), ]
    }

    df <- as.data.frame(df)
    df <- dplyr::mutate(df, fips_code = vec$FIPS[i])

    # new dataframe with only fips code and # of corresponding weather stations
    stationinfo <- data.frame(fips_code = df$fips_code, nstations = nrow(df))
    print(unique(stationinfo))
  }
}

# right now, this function is printing a separate dataframe (one row, two
# columns) for each fips code. I tried to get them in one dataframe with the
# commented out code below but the function was unhappy about it.
#  if(i == 1){
#    stationinfo_df <- stationinfo
#  } else {
#    stationinfo_df <- rbind(stationinfo_df, stationinfo)
#  }

# Examples

vec <- c(36081, 36085, 36087, 36119, 40017)
stationspercounty(vec)
# returns an error - none of these fips had relevant stations

vec2 <- c("01073", "01089", "01097", "01101", "02020", "04013", "04019",
          "05119", "06001", "06013", "06019", "06029", "06037", "06065",
          "06067", "06071", "06073", "06075", "06077", "06081", "06085",
          "06099", "06111", "08001", "08005", "08031", "08041", "09001",
          "09003", "09009", "10003", "11001", "12001", "12011", "12031",
          "12033", "12057", "12071", "12073", "12081", "12086", "12095",
          "12099", "12103", "12117", "12127", "13021", "13051", "13063",
          "13067", "13089", "13121", "13215", "13245")
stationspercounty(vec2)

# A fips code could get kicked out of the final dataframe because its weather
# stations:
# A. don't exist,
# B. don't start after Jan 1, 1999,
# C. don't start before Dec. 31, 2012, and/or
# D. don't have a % coverage of at least 0.9.

# problems:
# 1. Trying to get fips codes without relevant stations into a separate df
# 2. Returning an empty dataframe instead of an error when the fips vector
#    doesn't have any fips with relevant stations
# 3. Getting the function to return a single dataframe (insead of a separate df
#    for each fips code)
