#' Return average daily weather data and a plot showing the location of weather
#' stations for a particular county.
#'
#' Given a particular county FIPS code, this function returns a list with two
#' elements: "data", a dataframe of daily average weather values, and "plot",
#' a plot showing the location of weather stations contributing to the average
#' weather in "data".
#'
#' @inheritParams weather_fips_df
#' @param shapefile_dir This is the absolute or relative pathname for the
#'    directory where the U.S. Census Cartographic Boundary Shapefile (Counties)
#'    is saved. The cb_2014_us_county_5m.zip file can be downloaded from
#'    \url{https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html}.
#'    (Optional.)
#'
#' @return A list with two elements. The first element ("data") is a dataframe
#'    of daily weather data averaged across multiple stations, as well as
#'    columns (\code{"var"_reporting}) for each weather variable showing the
#'    number of stations contributing to the average for that variable on that
#'    day. The second element ("plot") is a plot showing points for all weather
#'    stations for a particular county satisfying the conditions present in
#'    \code{weather_fips}'s arguments (radius, percent_coverage, date_min,
#'    date_max, and/or var). If you have downloaded the U.S. Census Cartographic
#'    Boundary Shapefile (cb_2014_us_county_5m.zip) and include the
#'    \code{shapefile_dir} argument, the resulting plot will include an outline
#'    of the county indicated in the function's \code{fips} argument.
#'
#' @example
#' \dontrun{
#' ex <- weather_fips("08031", radius = 15, percent_coverage = 0.90,
#' date_min = "2010-01-01", date_max = "2010-02-01", var = "PRCP",
#' shapefile_dir = "/Users/rachelseverson1/Desktop/LD/cb_2014_us_county_5m")
#'
#' data <- ex$data
#' plot <- ex$plot
#' }
#' @export
weather_fips <- function(fips, radius = NULL, percent_coverage = NULL,
                         date_min = NULL, date_max = NULL, var = "all",
                         shapefile_dir = NULL){
  data <- weather_fips_df(fips, radius, percent_coverage, date_min, date_max,
                          var)
  plot <- stationmap_fips(fips, radius, percent_coverage,
                          date_min, date_max, var, shapefile_dir)
  list <- list("data" = data, "plot" = plot)
  return(list)
}

#' Return average daily weather data for a particular county.
#'
#' \code{weather_fips_df} returns a dataframe of average daily weather values
#' for a particular county, radius, date range, and/or specified "coverage."
#'
#' This function serves as a wrapper to several functions from the \code{rnoaa}
#' package, which provides weather data from all relevant stations in a county.
#' This function filters and averages across stations based on user-specified
#' coverage specifications.
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
#'    to search for monitors. (Optional.) If this argument is NULL,
#'    \code{weather_fips_df} will begin its search with all stations within the
#'    specified county.
#' @param percent_coverage A numeric value in the range of 0 to 1 that specifies
#'    the desired percentage coverage for the weather variable (i.e., what
#'    percent of each weather variable must be non-missing to include data from
#'    a monitor when calculating daily values averaged across monitors.
#'    (Optional.)
#' @param date_min A character string giving the earliest date you want
#'    in your dataset in "yyyy-mm-dd" format. (Optional.)
#' \code{date_min}.
#' @param date_max A character string giving the latest date you want
#'    in your dataset in "yyyy-mm-dd" format. (Optional.)
#' @param var A character vector specifying desired weather variables. For
#'    example, var = c("TMIN", "TMAX", "PRCP"). (Optional.)
#'
#' @return A dataframe of daily weather data averaged across multiple monitors,
#'    as well as columns (\code{"var"_reporting}) for each weather variable
#'    showing the number of stations contributing to the average for that
#'    variable on that day.
#'
#' @examples
#' \dontrun{
#' df <- weather_fips_df(fips = "12086", radius = 10,
#'                    percent_coverage = 0.90, date_min = "2010-01-01",
#'                    date_max = "2010-02-01", var = c("TMAX", "TMIN", "PRCP"))
#' }
#' @export
weather_fips_df <- function(fips, radius = NULL, percent_coverage = NULL,
                         date_min = NULL, date_max = NULL, var = "all"){

  # get stations for 1 fips
  if (is.null(radius)){
    stations <- fips_stations(fips, date_min = date_min, date_max = date_max)
  } else {
    stations <- station_radius(fips = fips, radius = radius)
  }

  # get tidy full dataset for all monitors
  # meteo_pull_monitors() from helpers_ghcnd.R in ropenscilabs/rnoaa
  meteo_df <- meteo_pull_monitors(monitors = stations,
                                  keep_flags = FALSE,
                                  date_min = date_min,
                                  date_max = date_max,
                                  var = var)

  # calculate coverage for each weather variable
  # meteo_coverage() from meteo_utils.R in ropenscilabs/rnoaa
  coverage_df <- meteo_coverage(meteo_df, verbose = FALSE)

  # filter station dataset based on specified coverage
  filtered <- filter_coverage(coverage_df, percent_coverage)
  good_monitors <- unique(filtered$id)

  # filter weather dataset based on stations with specified coverage
  filtered_data <- filter(meteo_df, id %in% good_monitors)

  # average across stations, add a column for number of stations that
  # contributed to each daily average
  averaged <- ave_weather(filtered_data)

  return(averaged)
}

#' Average weather data across multiple stations.
#'
#' \code{ave_weather} returns a dataframe with daily weather averaged across
#'    stations, as well as columns showing the number of stations contributing
#'    to the average for each variable and each day.
#'
#' @param weather_data A dataframe with daily weather observations. This
#'    dataframe is returned from the function \code{meteo_pull_monitors}.
#'
#' @export
ave_weather <- function(weather_data){

  averaged_data <- gather(weather_data, key, value, -id, -date) %>%
    ddply(c("date", "key"), summarize,
          mean = mean(value, na.rm = TRUE)) %>%
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
#' @param percent_coverage A numeric value in the range of 0 to 1 that specifies
#'    the desired percentage coverage for the weather variable (i.e., what
#'    percent of each weather variable must be non-missing to include data from
#'    a monitor when calculating daily values averaged across monitors.
#'    (Optional.)
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
station_radius <- function(fips, radius = NULL){
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

  central_latlong <- ggmap::geocode(location = fipsname, output = c("latlon"));
  assign("central_latlong", central_latlong, .GlobalEnv)


  FIPS <- paste0('FIPS:', fips)
  station_df <- rnoaa::ncdc_stations(datasetid = 'GHCND',
                                     locationid = FIPS)$data;
      assign("station_df", station_df, .GlobalEnv)

  # meteo_distance from meteo_distance.R in ropenscilabs/rnoaa
  station_df <- meteo_distance(station_data = station_df,
                                 lat = central_latlong$lat,
                                 long = central_latlong$lon,
                                 radius = radius)
  stations <- unique(station_df$id)
  stations <- stations[!is.na(stations)]
  stations <- gsub("GHCND:", "", stations)
  return(stations)
}

#' Plot weather stations for a particular county
#'
#' @param shapefile_dir This is the absolute or relative pathname for the
#'    directory where the U.S. Census Cartographic Boundary Shapefile (Counties)
#'    is saved. The cb_2014_us_county_5m.zip file can be downloaded from
#'    \url{https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html}.
#'    (Optional.)
#' @inheritParams weather_fips_df
#'
#' @return A plot showing points for all weather stations for a particular
#'    county satisfying the conditions present in \code{stationmap_fips}'s
#'    arguments (radius, percent_coverage, date_min, date_max, and/or var). If
#'    you have downloaded the U.S. Census Cartographic Boundary Shapefile
#'    cb_2014_us_county_5m.zip and include the \code{shapefile_dir} argument,
#'    the resulting plot will include an outline of the county indicated in the
#'    function's \code{fips} argument.
#'
#' @examples
#' \dontrun{
#' ex <- stationmap_fips(fips = "08031", radius = 0.20, percent_coverage = 0.90,
#' date_min = "2010-01-01", date_max = "2010-02-01", var = "PRCP")
#' }
#' @export
stationmap_fips <- function(fips, radius = NULL, percent_coverage = NULL,
                            date_min = NULL, date_max = NULL, var = "all",
                            shapefile_dir = NULL){
  # pull stations
  if (is.null(radius)){
    stations <- fips_stations(fips, date_min = date_min, date_max = date_max)
  } else {
    stations <- station_radius(fips = fips, radius = radius)
  }
  # meteo_pull_monitors() from helpers_ghcnd.R in ropenscilabs/rnoaa
  meteo_df <- meteo_pull_monitors(monitors = stations,
                                  keep_flags = FALSE,
                                  date_min = date_min,
                                  date_max = date_max,
                                  var = var)
  coverage_df <- meteo_coverage(meteo_df, verbose = FALSE)
  filtered <- filter_coverage(coverage_df, percent_coverage)
  good_monitors <- unique(filtered$id)

  # in case radius is not NULL, still need to run fips_stations() to
  # get station_df (which is in the global environment after running the
  # fips_stations() function)
  fips_stations(fips, date_min = date_min, date_max = date_max)
  df <- mapping(station_df)

  station_latlong <- filter(df, df$id %in% good_monitors)

  meteo_df_filtered <- filter(meteo_df, meteo_df$id %in% good_monitors)

  # not currently using this perc_missing df for anything
  perc_missing <- gather(meteo_df_filtered, key, value, -id, -date) %>%
    ddply(c("id", "key"), summarize,
          percent_missing = sum(is.na(value)) / length(value)) %>%
    mutate(key = paste(key, "percent_missing", sep = "_")) %>%
    spread(key = key, value = percent_missing)

  final_df <- left_join(station_latlong, perc_missing, by = "id")

  # run station_radius() to get central_latlong (in global environment after
  # running station_radius())
  station_radius(fips = fips, radius = radius)

  # this initial zoom = 9 will later be cropped based on the county polygon
  # or station locations.
  getmap <- ggmap::get_map(location = c(lon = central_latlong$lon,
                                     lat = central_latlong$lat),
                        zoom = 9, maptype = "roadmap", color = "bw")
  map <- ggmap(getmap)

  if(!is.null(shapefile_dir)){

    shp <- rgdal::readOGR(shapefile_dir, "cb_2014_us_county_5m")
    shp@data$GEOID <- as.character(shp@data$GEOID)

    dat <- shp@data
    dat <- mutate(dat, id = c(0:3232))
    dat <- select(dat, GEOID, id)
    dat$id <- as.character(dat$id)
    shp_df <- broom::tidy(shp)
    shp <- full_join(shp_df, dat, by = "id")

    county_pol <- filter(shp, GEOID == fips)

    xlim_min <- min(county_pol$long) - 0.1
    xlim_max <- max(county_pol$long) + 0.1
    ylim_min <- min(county_pol$lat) - 0.1
    ylim_max <- max(county_pol$lat) + 0.1

    map_overlay <- map + geom_polygon(data = county_pol,
                                      aes(x = long, y = lat),
                                      fill = "white", colour = "black",
                                      alpha = 0, size = 1) +
      geom_point(data = final_df, aes(x = lon, y = lat), colour = "blue4",
                 size = 5)

    # map with county polygon overlaid
    map_cropped <- map_overlay + scale_x_continuous(limits = c(xlim_min,
                                                               xlim_max),
                                                    expand = c(0, 0)) +
                                 scale_y_continuous(limits = c(ylim_min,
                                                               ylim_max),
                                                    expand = c(0, 0))
  } else {

    xlim_min <- min(final_df$lon) - 0.1
    xlim_max <- max(final_df$lon) + 0.1
    ylim_min <- min(final_df$lat) - 0.1
    ylim_max <- max(final_df$lat) + 0.1

    # map without county polygon overlaid
    map_cropped <- map + geom_point(data = final_df, aes(x = lon, y = lat),
                                    colour = "blue4", size = 5) +
      scale_x_continuous(limits = c(xlim_min, xlim_max),
                         expand = c(0, 0)) +
      scale_y_continuous(limits = c(ylim_min, ylim_max),
                         expand = c(0, 0))
  }

  return(map_cropped)

}

#' Return a dataframe with station IDs, and longitude and latitude for each
#' station.
#'
#' @param ncdcdf A dataframe obtained using the \code{ncdc_stations} function
#'    in the rnoaa package, with the \code{datasetid} argument set to 'GHCND',
#'    the \code{locationid} set to a U.S. county FIPS code (in the format
#'    'FIPS:08031', for example). This dataframe can be obtained using the
#'    \code{station_fips} function. After running \code{station_fips}, the
#'    \code{station_df} will be in your global environment.
#'
#' @export
mapping <- function(station_df){
  df <- select(station_df, longitude, latitude, id)
  colnames(df) <- c("lon", "lat", "id")
  df$id <- gsub("GHCND:", "", df$id)
  return(df)
}
