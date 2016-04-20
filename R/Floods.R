#'USGS Daily Streamflow data per US counties
#'
#'\code{streamstations} returns a vector of USGS station identification numbers
#'
#'http://waterservices.usgs.gov/rest/Site-Test-Tool.html
#'
#'
#' @examples
#' fipsvec <- c(51059, 51061)
#' test <- streamflow(fipsvec, "2000-04-12", "2005-04-13")
streamstations <- function(fips, date_min, date_max, percent_coverage = NULL){

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


streamdata <- function(fips, date_min, date_max, stat_code){
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

test <- streamdata("47157", "2011-01-01", "2011-12-31", "00003")

streamdata <- function(fips, date_min, date_max, stat_code){
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


}


x <- data.frame(id = c(1, 2, 3), val = c(100, 200, 300),
                dates = c(10, 20, 30))
y <- data.frame(id = c(1, 2, 3), percval = c(150, 150, 300),
                dates = c(10, 20, 30))

dft <- inner_join(x, y)

dft <- mutate(dft, flood = NA)

for(i in 1:length(dft$val)){



}
if(dft$val >= dft$percval){
  dft$flood = 1
} else {
  dft$flood = 0
}





# http://help.waterdata.usgs.gov/code/stat_cd_nm_query?stat_nm_cd=%25&fmt=html

# ex <- importDVs(test[1], stat = "00003", sdate = date_min, edate = date_max)
# ex2 <- cleanUp(ex, task = "fix")





