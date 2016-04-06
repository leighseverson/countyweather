#' Count of weather stations per county.
#'
#' \code{stations_per_county} returns a plot showing the number of GHCND weather
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
#'
#' @examples
#' \dontrun{
#' fips <- c(36081, 36085, 36087, 36119, 40017)
#' stations_per_county(vec7)
#' }
#'
#' returns an error - none of these fips had relevant stations
#' \donttest{
#' vec2 <- c("01073", "01089", "01097", "01101", "02020", "04013", "04019",
#'          "05119", "06001", "06013", "06019", "06029", "06037", "06065",
#'          "06067", "06071", "06073", "06075", "06077", "06081", "06085",
#'          "06099", "06111", "08001", "08005", "08031", "08041", "09001",
#'          "09003", "09009", "10003", "11001", "12001", "12011", "12031",
#'          "12033", "12057", "12071", "12073", "12081", "12086", "12095",
#'          "12099", "12103", "12117", "12127", "13021", "13051", "13063",
#'          "13067", "13089", "13121", "13215", "13245")
#' stations_per_county(vec2)
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
stations_per_county <- function(fips){
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

    # new dataframe with only fips code and # of corresponding weather stations
    out <- dplyr::group_by(tot_df, fips) %>%
      dplyr::summarize(nstations = n()) %>%
      dplyr::right_join(vec, by = "fips") %>%
      dplyr::select(-FIPS) %>%
      dplyr::mutate(nstations = ifelse(is.na(nstations), 0, nstations))
    return(out)

}




# problems:
# 1. Trying to get fips codes without relevant stations into a separate df
# If the output gives these with `nstations` of `0`, then we can get this by
# just subsetting that output.
# 2. Returning an empty dataframe instead of an error when the fips vector
#    doesn't have any fips with relevant stations
# I haven't checked yet if the revised function will do that in that case. We
# should check.
# 3. Getting the function to return a single dataframe (insead of a separate df
#    for each fips code)
# This should be working now.
