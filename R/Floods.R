#' USGS Daily Streamflow data per US counties
#'
#' \code{streamstations} returns a vector of U.S. Geological Survey (USGS)
#' station identification numbers for a given US county, date range, and percent
#' coverage.
#'
#' This function interacts with the USGS Site web service to import daily
#' hydrological data. Options at the USGS Site Web Service URL Generation Tool
#' (\url{http://waterservices.usgs.gov/rest/Site-Test-Tool.html}) are set to
#' the following settings:
#' \itemize{
#'  \item Major Filters = Counties
#'  \item Optional Arguments: Show period of record information about these
#'  data types = Daily values
#'  \item Optional Filters = Show only sites operational between a start date
#'  and an end date
#'  \item Optional Filters: Show only sites serving these data types = Daily
#'  values
#'  \item Optional Filters: Sites serving parameter codes = 00060
#' }
#'
#' @param fips A character string or character vector giving the five-digit
#'  U.S. FIPS county code(s) of the county for which the user wants to pull
#'  hydrologic data.
#' @param date_min A character string giving the earliest date you want to
#'  pull data for in "yyyy-mm-dd" format.
#' @param date_max A character string giving the latest date you want to pull
#'  data for in "yyyy-mm-dd" format.
#' @param fraction_coverage A numeric value between 0 and 1. The dataframe
#'  returned will include only stations that have data coverage equal to or
#'  greater than the specified fraction. (Optional.)
#'
#' @examples
#' ex <- streamstations("47157", "2011-01-01", "2011-12-31")
#'
#' @export
streamstations <- function(fips, date_min, date_max, fraction_coverage = NULL){
  fips <- paste(fips, collapse = ",")
  url <- paste0("http://waterservices.usgs.gov/nwis/site/?format=rdb&countyCd=",
                fips, "&startDT=", date_min, "&endDT=", date_max,
                "&outputDataTypeCd=dv&parameterCd=00060&siteType=ST")

  df <- read.table(url, sep = "\t", comment.char = "#", header = TRUE)
  df <- df[-1, ]

  df$begin_date <- lubridate::ymd(df$begin_date)
  df$end_date <- lubridate::ymd(df$end_date)

  df <- mutate(df, date_range = df$end_date - df$begin_date)
  df$date_range <- as.numeric(df$date_range)
  df$count_nu <- as.numeric(df$count_nu)

  df <- mutate(df, perc_coverage = (count_nu/date_range))
  if(is.null(percent_coverage)){
    percent_coverage <- min(df$perc_coverage)
  }

  df <- filter(df, perc_coverage >= percent_coverage)

  vec <- as.character(unique(df$site_no))
  return(vec)
}

#' Return average daily streamflow data for a particular county and date range.
#'
#' \code{streamdata} returns a data.frame showing USGS station IDs, mean daily
#' streamflow values, dates, and a quantification code. This function applies
#' the \code{importDVs} function from the waterData package for all of the USGS
#' stations for a given FIPS code.
#'
#' @param fips A character string or character vector giving the five-digit
#'  U.S. FIPS county code(s) of the county for which the user wants to pull
#'  hydrologic data.
#' @param date_min A character string giving the earliest date you want to
#'  pull data for in "yyyy-mm-dd" format.
#' @param date_max A character string giving the latest date you want to pull
#'  data for in "yyyy-mm-dd" format.
#' @param stat_code A 5-number character string specifying a USGS statistics
#' code to uniquely identify specific statistics. Possible codes can be found
#' here:
#' \url{http://help.waterdata.usgs.gov/code/stat_cd_nm_query?stat_nm_cd=%25&fmt=html}.
#' Not all statistics are available at every gage. The default for stat_code in
#' \code{streamdata} is set to "00003", which gives daily mean values.
#'
#' @examples
#' ex <- streamdata("47157", "2011-01-01", "2011-12-31")
#'
#' @export
streamdata <- function(fips, date_min, date_max, stat_code = "00003"){
  ids <- streamstations(fips, date_min, date_max)

  test <- sapply(ids, FUN = waterData::importDVs, stat = stat_code,
                 sdate = date_min,
                 edate = date_max)
  df <- do.call(rbind, lapply(ids, function(ids) waterData::importDVs(
    staid = ids, stat = stat_code, sdate = date_min,
    edate = date_max)))

  df_clean <- cleanUp(df, task = "fix")
  return(df_clean)
}

#' Return average daily streamflow data for a particular county and date range.
#'
#' (This function is meant to return a data.frame with a "flood" column, showing
#' "1" if mean daily streamflow was greater than or equal to the 90th percentile
#' for that station, and "0" otherwise. This function could be useful if we can
#' figure out stations that have values for stat = "01900", which gives daily
#' 90th percentile streamflow values).
#'
#'
#' @export
streamfloods <- function(fips, date_min, date_max, stat_code){
  ids <- streamstations(fips, date_min, date_max)

  test <- sapply(ids, FUN = waterData::importDVs, stat = stat_code,
                 sdate = date_min,
                 edate = date_max)
  df <- do.call(rbind, lapply(ids, function(ids) waterData::importDVs(
    staid = ids, stat = stat_code, sdate = date_min,
    edate = date_max)))

  df_clean <- cleanUp(df, task = "fix")


  percentile90 <- purrr::safely(importDVs(ids[1], stat = "01900",
                                          sdate = date_min,
                                          edate = date_max))
  colnames(percentile90) <- c("staid", "percentileval", "dates", "qualcode")
  df <- inner_join(df_clean, percentile90)

  df <- mutate(df, flood = NA)

  for(i in 1:length(df$val)){
    if(df$val[i] >= df$percentileval[i]){
      df$flood[i] = 1
    } else {
      df$flood[i] = 0
    }
  }
  return(df)
}







