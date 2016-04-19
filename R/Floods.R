#'USGS Daily Streamflow data per US counties
#'
#'\code{}
#'
#' @examples
#' fipsvec <- c(51059, 51061)
#' test <- streamflow(fipsvec, "2000-04-12", "2005-04-13")
streamflow <- function(fips, date_min, date_max){
  fips <- paste(fips, collapse = ",")
  url <- paste0("http://waterservices.usgs.gov/nwis/site/?format=rdb&countyCd=",
                fips, "&startDT=", date_min, "&endDT=", date_max,
                "&outputDataTypeCd=dv&parameterCd=00060&siteType=ST")
  df <- read.table(url, sep = "\t", comment.char = "#", header = TRUE)
  df <- df[-1, ]
  df <- as.character(unique(df$site_no))
  return(df)
}


# ex <- importDVs(test[1], stat = "00003", sdate = date_min, edate = date_max)
# ex2 <- cleanUp(ex, task = "fix")





