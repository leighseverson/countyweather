#' Return average daily weather data and a plot showing the location of weather
#' stations for a particular county.
#'
#' Given a particular county FIPS code, this function returns a list with two
#' elements: "data", a dataframe of daily average weather values, and "plot",
#' a plot showing the location of weather stations contributing to the average
#' weather in "data".
#'
#' @inheritParams weather_fips_df
#'
#' @return A list with two elements. The first element ("data") is a dataframe
#'    of daily weather data averaged across multiple stations, as well as
#'    columns (\code{"var"_reporting}) for each weather variable showing the
#'    number of stations contributing to the average for that variable on that
#'    day. The second element ("plot") is a plot showing points for all weather
#'    stations for a particular county satisfying the conditions present in
#'    \code{weather_fips}'s arguments (percent_coverage, date_min,
#'    date_max, and/or var).
#'
#' @note You must have a NOAA API to use this function, and you need to set
#'    that API code in your R session (e.g., using \code{options("noaakey" = }).
#'
#' @examples
#' \dontrun{
#' ex <- weather_fips("08031", percent_coverage = 0.90,
#' date_min = "2010-01-01", date_max = "2010-02-01", var = "PRCP")
#'
#' weather_data <- ex$weather_data
#' station_map <- ex$station_map
#' }
#' @export
weather_fips <- function(fips, percent_coverage = NULL,
                         date_min = NULL, date_max = NULL, var = "all"){
  weather_data <- weather_fips_df(fips = fips,
                                  percent_coverage = percent_coverage,
                                  date_min = date_min, date_max = date_max,
                                  var = var)
  station_map <- stationmap_fips(fips = fips,
                                 percent_coverage = percent_coverage,
                                 date_min = date_min, date_max = date_max,
                                 var = var)
  list <- list("weather_data" = weather_data, "station_map" = station_map)
  return(list)
}

#' Return average daily weather data for a particular county.
#'
#' \code{weather_fips_df} returns a dataframe of average daily weather values
#' for a particular county, date range, and/or specified "coverage."
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
#' df <- weather_fips_df(fips = "12086",
#'                    percent_coverage = 0.90, date_min = "2010-01-01",
#'                    date_max = "2010-02-01", var = c("TMAX", "TMIN", "PRCP"))
#' }
#' @export
weather_fips_df <- function(fips, percent_coverage = NULL,
                         date_min = NULL, date_max = NULL, var = "all"){

  # get stations for 1 fips
    stations <- fips_stations(fips, date_min = date_min, date_max = date_max)

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
#'    requirements for weather variablezs included in the dataframe present in
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
#' @inheritParams weather_fips_df
#'
#' @return A plot showing points for all weather stations for a particular
#'    county satisfying the conditions present in \code{stationmap_fips}'s
#'    arguments (percent_coverage, date_min, date_max, and/or var).
#'
#' @examples
#' \dontrun{
#' ex <- stationmap_fips(fips = "08031", percent_coverage = 0.90,
#'                       date_min = "2010-01-01", date_max = "2010-02-01",
#'                       var = "PRCP")
#' }
#' @export
stationmap_fips <- function(fips, percent_coverage = NULL,
                            date_min = NULL, date_max = NULL, var = "all"){
  # pull stations
    stations <- fips_stations(fips, date_min = date_min, date_max = date_max)

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

  data("df_pop_county")

  census_csv <- paste0("http://www2.census.gov/geo/docs/reference/cenpop2010/",
                       "county/CenPop2010_Mean_CO.txt")
  census_data <- read.csv(census_csv)
  census_data$COUNTYFP <- sprintf("%03d", census_data$COUNTYFP)
  census_data <- mutate(census_data, choro_fips = paste0(census_data$STATEFP,
                                                         census_data$COUNTYFP))

  census_data$state_long <- sprintf("%02d", census_data$STATEFP)
  census_data$fips <- paste0(census_data$state_long, census_data$COUNTYFP)

  row_num <- which(grepl(fips, census_data$fips))

  census_data$cname <- paste0(census_data$COUNAME, " County, ")
  census_data$name <- paste0(census_data$cname, census_data$STNAME)
  census_data <- select(census_data, -cname, -state_long)

  choro_fips <- census_data[row_num, 8]
  title <- census_data[row_num, 10]

  map <- choroplethr::county_choropleth(df_pop_county, title = "", legend = "",
                           num_colors = 1, state_zoom = NULL,
                           county_zoom = choro_fips, reference_map = TRUE)

  map <- map + ggplot2::geom_point(data = final_df, aes(x = lon, y = lat),
                          colour = "black", size = 5) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(title)

  return(map)
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

#' Write daily weather timeseries files for U.S. counties
#'
#' Given a vector of U.S. county FIPS codes, this function creates timeseries
#' dataframes giving: 1. the values for specified weather variables, and 2. the
#' number of weather stations contributing to the average for each day within the
#' specified date range.
#'
#' @return Writes out a directory with daily weather files for each FIPS code
#' specified.
#'
#' @inheritParams weather_fips_df
#' @param out_directory The absolute or relative pathname for the directory
#' where you would like the timeseries files to be saved.
#' @param out_type A character string indicating that you would like either .rds
#' files (out_type = "rds") or .csv files (out_type = ".csv"). This option
#' defaults to .rds files.
#'
#' @note If the function is unable to pull weather data for a particular county
#' given the specified percent coverage, date range, and/or weather variables,
#' \code{county_timeseries} will not produce a file for that county.
#'
#' @examples
#' county_timeseries(fips = c("41005", "13089"), percent_coverage = 0.90,
#'            date_min = "2000-01-01", date_max = "2000-01-10",
#'            var = c("TMAX", "TMIN", "PRCP"),
#'            out_directory = "~/timeseries_data")
#'
#' @export
county_timeseries <- function(fips, percent_coverage, date_min, date_max, var,
                              out_directory, out_type = "rds"){

  if(!dir.exists(out_directory)){
    dir.create(out_directory)
  }
  for(i in 1:length(fips)) {
    possibleError <- tryCatch({
      out_df <- weather_fips_df(fips = fips[i], percent_coverage = percent_coverage, date_min =
                                  date_min, date_max = date_max,
                                var = var)
      out_file <- paste0(out_directory, "/", fips[i], ".", out_type)
      if(out_type == "rds"){
        saveRDS(out_df, file = out_file)
      } else if (out_type == "csv"){
        utils::write.csv(out_df, file = out_file, row.names = FALSE)
      }
    }
    ,
    error = function(e) {
      e
      print(paste0("Unable to pull weather data for FIPS code ", fips[i],
                   " for the specified percent coverage, date range, and/or weather variables."))
    }
    )
    if(inherits(possibleError, "error")) next

  }

}

#' Write plot files for daily weather timeseries dataframes
#'
#' This function writes out a directory with plots for every timeseries file
#' present in the specified directory (produced by the \code{county_timeseries}
#' function) for a particular weather variable. These plots are meant to aid in
#' initial exploratory analysis.
#'
#' @return Writes out a directory with plots of timeseries data for a given
#' weather variable for each file present in the directory specified.
#'
#' @param var A character string (all lower-case) specifying which weather
#' variable present in the timeseries dataframe you would like to produce
#' plots for. For example, var = "prcp".
#' @param file_directory The absolute or relative pathname for the directory
#' where your daily timeseries dataframes (produced by \code{county_timeseries})
#' are saved.
#' @param file_type A character string indicating the type of timeseries files
#' you would like to produce plots for (either "rds" or "csv"). This option
#' defaults to .rds files.
#' @param plot_directory The absolute or relative pathname for the directory
#' where you would like the plots to be saved.
#'
#' @examples
#'plot_timeseries(var = "prcp",
#'                file_directory = "~/Desktop/exposure_data/ihapps_timeseries",
#'                plot_directory = "~/Desktop/exposure_data/plots_prcp")
#'
#'plot_timeseries(files = files, var = "tmax",
#'                file_directory = "~/Desktop/exposure_data/ihapps_timeseries",
#'                plot_directory = "~/Desktop/exposure_data/plots_tmax")
#'
#'plot_timeseries(var = "tmin",
#'                file_directory = "~/Desktop/exposure_data/ihapps_timeseries",
#'                plot_directory = "~/Desktop/exposure_data/plots_tmin")
#'
#' @export
plot_timeseries <- function(var, file_directory, file_type = "rds",
                            plot_directory){

  files <- list.files(file_directory)

  if(!dir.exists(plot_directory)){
    dir.create(plot_directory)
  }

  if(file_type == "rds"){
    file_names <- gsub(".rds", "", files)
  } else if (file_type == "csv"){
    file_names <- gsub(".csv", "", files)
  }

  for(i in 1:length(files)){

    setwd(file_directory)
    data <- readRDS(files[i])

    file_name <- paste0(file_names[i], ".png")
    setwd(plot_directory)
    png(filename = file_name)
    plot(data$date, data[,var],
         type = "l", col = "red", main = file_names[i],
         xlab = "date", ylab = var,
         xlim = c(as.Date("1987-01-01"), as.Date("2005-12-31")))
    dev.off()
  }

}
