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
#' fips_stations("36005")
#' fips_stations("12086", date_min = "1999-01-01", date_max = "2012-12-31")
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
fips_stations <- function(fips, date_min = NULL, date_max = NULL){
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
    station_df <- rbind(df, more_stations$data); assign("station_df",
                                                        station_df, .GlobalEnv)
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

  tot_df <- dplyr::mutate_(station_df,
                          mindate = ~ lubridate::ymd(mindate),
                          maxdate = ~ lubridate::ymd(maxdate)) %>%
    dplyr::filter_(~ maxdate >= date_min & mindate <= date_max) %>%
    dplyr::select_(~ id) %>%
    dplyr::mutate_(id = ~ gsub("GHCND:", "", id))

  vec <- as.vector(tot_df$id)
  return(vec)
}

