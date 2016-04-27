#' Return average daily weather data for a particular county.
#'
#' \code{weather_fips} returns a data.frame of average daily precipitation,
#' maximum and minimum temperature values for a particular county, radius,
#' date range, and specified "coverage."
#'
#' This function serves as a wrapper to several functions from the
#' \code{rnoaa} package, which provide weather data from all relevant
#' monitors in a county, and then this function filters and averages
#' across monitors based on user-specified coverage specifications.
#'
#' @note Because this function uses the NOAA API to identify the weather
#'    monitors within a US county, you will need to get an access token from
#'    NOAA to use this function. Visit NOAA's token request page
#'    (\url{http://www.ncdc.noaa.gov/cdo-web/token}) to request a token by
#'    email, and then use the code
#'    \code{options("noaakey" = "<key NOAA emails you>")} to set up your
#'    API access.
#'
#' @param fips A character string giving the five-digit U.S. FIPS county code
#'    of the county for which the user wants to pull weather data.
#' @param radius A numeric vector giving a radius (in kilometers) within which
#'    to search for monitors. (Optional.)
#' @param percent_coverage A numeric value in the range of 0 to 1 that specifies the
#'    desired percentage coverage for the weather variable (i.e., what percent
#'    of each weather variable must be non-missing to include data from a
#'    monitor when calculating daily values averaged across monitors. (Optional.)
#' @param date_min A character string giving the earliest date you want
#'    in your dataset in "yyyy-mm-dd" format. (Optional.)
#' \code{date_min}.
#' @param date_max A character string giving the latest date you want
#'    in your dataset in "yyyy-mm-dd" format. (Optional.)
#' @param var A character vector specifying desired weather variables. For
#'    example, var = c("TMIN", "TMAX", "PRCP"). (Optional.)
#'
#' @return A dataframe of daily weather data averaged across multiple monitors,
#'    as well as columns (\code{"var"_reporting} for each weather variable
#'    showing the number of stations contributing to the average for that
#'    variable for that day.
#'
#' @examples
#' \dontrun{
#' df <- weather_fips(fips = "12086", radius = 10,
#'                    percent_coverage = 0.90, date_min = "2010-01-01",
#'                    date_max = "2010-02-01", var = c("TMAX", "TMIN", "PRCP"))
#' }
#'
#' @export
weather_fips <- function(fips, radius = NULL, percent_coverage = NULL,
                         date_min = NULL, date_max = NULL, var = "all"){
  #browser()
  # get stations for 1 fips
  if (is.null(radius)){
    stations <- fips_stations(fips, date_min = date_min, date_max = date_max)
  } else {
    stations <- station_radius(fips = fips, radius = radius)
  }
  # want radius to be the default? Like not optional?

  # get tidy full dataset for all monitors
  # clean_daily() and meteo_pull_monitors() from helpers_ghcnd.R in
  # openscilabs/rnoaa
  meteo_df <- meteo_pull_monitors(monitors = stations,
                                  keep_flags = FALSE,
                                  date_min = date_min,
                                  date_max = date_max,
                                  var = var)

  # calculate coverage for each variable (prcp, tmax, tmin)
  # meteo_coverage() from meteo_utils.R in rnoaaopenscilabs
  coverage_df <- meteo_coverage(meteo_df, verbose = FALSE)

  # filter station dataset based on specified coverage
  filtered <- filter_coverage(coverage_df, percent_coverage)
  good_monitors <- unique(filtered$id)

  # filter weather dataset based on stations w/ specified coverage
  filtered_data <- filter(meteo_df, id %in% good_monitors)

  # average across stations, add a column for number of stations that contributed
  # to each daily average
  averaged <- ave_weather(filtered_data)
  # omg - this step took ~48 minutes s

  return(averaged)
}

#' Average weather data across multiple stations.
#'
#' \code{ave_weather} returns a dataframe with daily weather averaged across
#'    stations, as well as columns showing the number of stations contributing
#'    to the average for each variable and each day.
#'
#' @param weather_data A dataframe with daily weather observations. This
#'    dataframe is returned from \code{meteo_pull_monitors}.
#'
#' @export
ave_weather <- function(weather_data){
  averaged_data <- gather(weather_data, key, value, -id, -date) %>%

    ddply(c("date", "key"), summarize,
          mean = mean(value, na.rm = TRUE)) %>%
    #this step takes > 22 minutes to run!

    spread(key = key, value = mean)
  n_reporting <- gather(weather_data, key, value, -id, -date) %>%
    ddply(c("date", "key"), summarize,
          n_reporting = sum(!is.na(value))) %>%
    mutate(key = paste(key, "reporting", sep = "_")) %>%
    spread(key = key, value = n_reporting)
  averaged_data <- left_join(averaged_data, n_reporting,
                             by = "date")
  return(averaged_data)
}

#' Filter stations based on "coverage" requirements.
#'
#' \code{filter_coverage} filters available weather variables
#' based on a specified required minimum coverage (i.e., percent non-missing
#' daily observations).
#'
#' @param coverage_df a \code{meteo_coverage} dataframe
#' @inheritParams weather_fips
#'
#' @return a \code{dataframe} with stations that meet the specified coverage
#'    requirements for weather variables included in the dataframe present in
#'    this function's arguments.
#'
#' @export
filter_coverage <- function(coverage_df, percent_coverage = NULL){

  if (is.null(percent_coverage)){
    percent_coverage <- 0
  }

  filtered <- select(coverage_df, -start_date, -end_date, -total_obs) %>%
    gather(key, covered, -id)  %>%
    filter(covered >= percent_coverage) %>%
    mutate(covered = 1) %>%
    group_by(id) %>%
    mutate(good_monitor = sum(!is.na(covered)) > 0) %>%
    ungroup() %>%
    filter(good_monitor) %>%
    select(-good_monitor)
  return(filtered)
}

#' Search for stations located within a specified radius for a particular
#' U.S. county.
#'
#' @return A character vector listing station ids which are located within
#'    the specified radius centered in the specified county.
#'
#' @export
station_radius <- function(fips, radius){
  url <- paste0("http://www2.census.gov/geo/docs/reference/",
                "codes/files/national_county.txt")
  county_names <- read.csv(url, header = FALSE, colClasses = "character")
  colnames(county_names) <- c("state", "state_fips", "county_fips", "county",
                              "fips_class")
  non_fifty <- c("VI", "UM", "PR", "MP", "GU", "AS")
  county_names <- county_names[!county_names$state %in% non_fifty, ]
  county_names$state <- state.name[match(county_names$state,state.abb)]
  county_names <- transform(county_names, fips_code = paste(state_fips,
                                                            county_fips,
                                                            sep = ""),
                            name = paste(county, state, sep = ", "))
  county_names <- select(county_names, fips_code, county, state, name)

  fipsname <- filter(county_names, fips_code == fips)$name
  fipsname <- as.character(fipsname)

  lonlat <- ggmap::geocode(location = fipsname, output = c("latlon"))


  FIPS <- paste0('FIPS:', fips)
  station_df <- ncdc_stations(datasetid = 'GHCND', locationid = FIPS)$data;
      assign("station_df", station_df, .GlobalEnv)


  station_df <- meteo_distance(station_data = station_df,
                                 lat = lonlat$lat,
                                 long = lonlat$lon,
                                 radius = radius)
  stations <- unique(station_list$id)
  stations <- stations[!is.na(stations)]
  stations <- gsub("GHCND:", "", stations)
  return(stations)
}

#' Plot of stations for a particular FIPS
#'
#'
#'
#' @examples
#' \dontrun{
#' ex <- stationmap_fips("08031", 0.90, "2000-01-01", "2010-12-31")
#' }
#'
#' @export
stationmap_fips <- function(fips, radius = NULL, percent_coverage = NULL,
                            date_min = NULL, date_max = NULL, var = "all"){
  # filter stations
  if (is.null(radius)){
    stations <- fips_stations(fips, date_min = date_min, date_max = date_max)
  } else {
    stations <- station_radius(fips = fips, radius = radius)
  }
  meteo_df <- meteo_pull_monitors(monitors = stations,
                                  keep_flags = FALSE,
                                  date_min = date_min,
                                  date_max = date_max,
                                  var = var)
  coverage_df <- meteo_coverage(meteo_df, verbose = FALSE)
  filtered <- filter_coverage(coverage_df, percent_coverage)
  good_monitors <- unique(filtered$id)

  df <- mapping(station_df)

  station_latlong <- filter(df, df$id %in% good_monitors)

  meteo_df_filtered <- filter(meteo_df, meteo_df$id %in% good_monitors)

  perc_missing <- gather(meteo_df_filtered, key, value, -id, -date) %>%
    ddply(c("id", "key"), summarize,
          percent_missing = sum(is.na(value)) / length(value)) %>%
    mutate(key = paste(key, "percent_missing", sep = "_")) %>%
    spread(key = key, value = percent_missing)

  final_df <- left_join(station_latlong, perc_missing, by = "id")

  map <- ggmap::get_map(location = c(lon = final_df$lon[1],
                                     lat = final_df$lat[1]),
                        zoom = 9, maptype = "roadmap")
  map <- ggmap(map)
    # geom_point(data = final_df, aes(x = lon, y = lat, color = prcp_percent_missing),
    #           size = 4)

  map_overlay <- map + geom_polygon(aes(x = longitude, y = latitude),
                                    data = polygon_df)
  # LOL


  # prcp_percent_missing for example - prob want to be able to specify what
  # weather variable you want here
  return(map)
}

#' Mapping function
#' @export
mapping <- function(ncdcdf){
  df <- select(ncdcdf, longitude, latitude, id)
  colnames(df) <- c("lon", "lat", "id")
  df$id <- gsub("GHCND:", "", df$id)
  return(df)
}


library(maps)
county.fips$fips <- sprintf("%05d", county.fips$fips)
polyname <- filter(county.fips, fips == "12086")$polyname

polygon_list <- map("county", region = polyname)
polygon_list$x <- na.omit(polygon_list$x)
polygon_list$y <- na.omit(polygon_list$y)

names(polygon_list)[1] <- "longitude"
names(polygon_list)[2] <- "latitude"

long <- unlist(polygon_list[1], use.names = FALSE)
lat <- unlist(polygon_list[2], use.names = FALSE)

polygon_df <- data.frame(longitude = long, latitude = lat)

fips <- "12086"
radius <- 10
percent_coverage <- 0.90
date_min <- "2010-01-01"
date_max <- "2010-02-01"
var <- "PRCP"
