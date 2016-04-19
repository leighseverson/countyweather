#'USGS Daily Streamflow data per US counties
#'
#'\code{}

library(rvest)

streamflow <- function(fips, date_min, date_max){
  fips <- paste(fips, collapse = ",")
  url <- paste0("http://waterservices.usgs.gov/nwis/site/?format=rdb&countyCd=",
                fips, "&startDT=", date_min, "&endDT=", date_max,
                "&outputDataTypeCd=dv&parameterCd=00060&siteType=ST")
  df <- read.table(url, sep = "\t", comment.char = "#", header = TRUE)
  df <- test1[-1, ]
  return(df)
}

fipsvec <- c(51059, 51061)

test <- streamflow(fipsvec, "2000-04-12", "2005-04-13")



