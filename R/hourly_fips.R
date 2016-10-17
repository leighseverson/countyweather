#' Return average hourly weather data and a plot showing the location of weather
#' stations for a particular county.
#'
#' Given a particular county FIPS code, this function returns a list with two
#' elements: \code{data}, a dataframe of hourly average weather values, and
#' \code{plot}, a plot showing the location of weather stations contributing to
#' the average weather in \code{data}.
#'
#' @inheritParams hourly_df
#'
#' @param station_label TRUE / FALSE to indicate if you want your plot of
#'    weather station locations to include labels indicating station usaf id
#'    numbers.
#' @param verbose TRUE / FALSE to indicate if you want the function to print
#'    out the name of the county it's processing.
#'
#' @return A list with six elements. The first element (\code{hourly_data}) is a
#'    dataframe of daily weather data averaged across multiple stations, as well
#'    as columns (\code{"var"_reporting}) for each weather variable showing the
#'    number of stations contributing to the average for that variable for that
#'    hour. The second element (\code{station_metadata} is a dataframe of station
#'    metadata for stations included in the \code{daily_data} dataframe, as
#'    well as statistical information about the values contriuted to each
#'    weather variable by each station. The third element (\code{station_map})
#'    is a plot showing points for all weather stations for a particular county
#'    satisfying the conditions present in \code{hourly_fips}'s arguments
#'    (year, coverage, and/or var). \code{radius} is the calculated radius
#'    within which stations were pulled from the county's center. Elements
#'    \code{lat_center} and \code{lon_center} are the latitude and longitude of
#'    the county's center.
#'
#' @note Observation times are vased on Coordinated Universal Time Code (UTC).
#'
#' @examples
#' \dontrun{
#'
#' ex <- hourly_fips("12086", coverage = 0.90, year = c(1994, 1995),
#'                    var = "temperature")
#'
#' data <- ex$hourly_data
#' station_data <- ex$station_metadata
#' station_map <- ex$station_map
#' @export
hourly_fips <- function(fips, year, var = "all",
                        coverage = NULL, average_data = TRUE,
                        station_label = FALSE, verbose = TRUE){

  census_data <- countyweather::county_centers
  loc_fips <- which(census_data$fips == fips)

  if(verbose) {
    message(paste0("Getting hourly weather data for ",
                   census_data[loc_fips, "name"], ".",
                   " This may take a while."))
  }

  weather_data <- hourly_df(fips = fips, year = year, var = var,
                                 coverage = coverage,
                                 average_data = average_data)

  station_map <- hourly_stationmap(fips = fips, hourly_data = weather_data,
                                   station_label = station_label)

  list <- list("hourly_data" = weather_data$hourly_data,
               "station_metadata" = weather_data$station_df,
               "station_map" = station_map,
               "radius" = weather_data$radius,
               "lat_center" = weather_data$lat_center,
               "lon_center" = weather_data$lon_center)
  return(list)

}

#' Return average hourly weather data for a particular county.
#'
#' \code{hourly_df} returns a dataframe of average daily weather values
#' for a particular county, year, and/or specified "coverage."
#'
#' This function serves as a wrapper to several functions from the \code{rnoaa}
#' package, which provides weather data from all relevant stations in a county.
#' This function filters and averages across NOAA ISD/ISH stations based on
#' user-specified coverage specifications.
#'
#' @param fips A string with the five-digit U.S. FIPS code of a county
#'    in numeric, character, or factor format.
#' @param year a four digit number or vector of numbers indicating the year or
#'    years for which you want to pull hourly data. Values for \code{year} can
#'    be in the range from 1901 to the current year.
#' @param var A character vector specifying desired weather variables. For
#'    example, var = c("wind_speed", "temperature"). The core weather variables
#'    available include \code{c("wind_direction", "wind_speed", "ceiling_height",
#'    "visibility_distance", "temperature", "temperature_dewpoint",
#'    "air_pressure")}. Alternatively, you can specify var = "all" to include
#'    additional flag and quality codes.
#' @param average_data TRUE / FALSE to indicate if you want the function to
#'    average daily weather data across multiple monitors.
#' @param coverage A numeric value in the range of 0 to 1 that specifies
#'    the desired percentage coverage for the weather variable (i.e., what
#'    percent of each weather variable must be non-missing to include data from
#'    a monitor when calculating daily values averaged across monitors.)
#'
#' @return A list with five elements. The first element, \code{hourly_data}, is
#'    a dataframe of hourly weather data averaged across multiple stations,
#'    as well as columns (\code{"var"_reporting}) for each weather variable
#'    showing the number of stations contributing to the average for that
#'    variable for each hour. \code{station_df} is a dataframe of station
#'    metadata for each station contributing weather data. A weather station
#'    will have one row per weather variable it contributes data to. In addition
#'    to information such as usaf and wban ids and station names, this
#'    dataframe includes statistical information about weather values
#'    contributed by each station for each weather variable. These statistics
#'    include calculated coverage (\code{calc_coverage}), which is the percent
#'    of non-missing values for each station and variable for the specified
#'    date range, \code{standard_dev} (standard deviation), \code{max},
#'    \code{min}, and \code{range} values for each station-weather variable
#'    combination. The element \code{radius} is the calculated radius within
#'    which stations were pulled from the county's center. Elements
#'    \code{lat_center} and \code{lon_center} are the latitude and longitude of
#'    the county's center.
#'
#' @note: Observation times are vased on Coordinated Universal Time Code (UTC).
#'
#' @references For more information on this dataset and available weather and
#' flag/quality variables, see
#' \url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-format-document.pdf}.
#'
#' @examples
#' \dontrun{
#' df <- hourly_df(fips = "12086", year = 1992, var = c("wind_speed",
#'                      "temperature"))
#' data <- df$hourly_data
#' station_info <- df$station_df
#' radius <- df$radius
#' }
#'
#' @export
hourly_df <- function(fips, year,
                           var = "all",
                           average_data = TRUE, coverage = NULL){

  # hourly data for multiple monitors for multiple years
  hourly_list <- lapply(year, function(x) isd_monitors_data(fips = fips,
                                                            year = x,
                                                            var = var))

  for(i in 1:length(year)){
    list <- hourly_list[[i]]
    if (i == 1){
      data <- list$df
    } else {
      data <- dplyr::bind_rows(data, list$df)
    }
  }

  # station meta data for one county (unfiltered)

  for(i in 1:length(year)){
    list <- hourly_list[[i]]
    if (i == 1){
      station_metadata <- list$ids
    } else {
      station_metadata <- dplyr::bind_rows(station_metadata, list$ids)
    }
  }

  # filter stations (if coverage is NULL, filters as if coverage = 0) and get
  # statistical info for each station/var pair
    filtered_list <- filter_hourly(fips = fips, hourly_data = data,
                                   coverage = coverage)
    station_stats <- filtered_list$stations

    filtered_stations <- unique(station_stats$station)

    station_metadata[station_metadata == "999999"] <- NA
    station_metadata[station_metadata == "99999"] <- NA

    station_metadata <- unique(station_metadata) %>%
      dplyr::mutate_(station = ~ paste(usaf, wban, sep = "-"))

    station_metadata <- station_metadata %>%
      dplyr::filter_(~ station %in% filtered_stations)

  # combine station_metadata and station_stats

    station_metadata <- dplyr::right_join(station_metadata, station_stats,
                                          by = "station") %>%
      dplyr::select_(quote(usaf), quote(wban), quote(station), quote(station_name),
             quote(var), quote(calc_coverage), quote(standard_dev), quote(range),
             quote(ctry), quote(state), quote(elev_m), quote(begin), quote(end),
             quote(longitude), quote(latitude))

  # average hourly across multiple stations

  data <- data %>%
    dplyr::mutate_(station = ~ paste(usaf_station, wban_station, sep = "-")) %>%
    dplyr::filter_(~ station %in% filtered_stations) %>%
    dplyr::select_(quote(-station))

  if(average_data == TRUE){
    data <- ave_hourly(data)
  }

  data <- tibble::as_tibble(data)

  radius <- hourly_list[[1]]$radius
  lat_center <- hourly_list[[1]]$lat_center
  lon_center <- hourly_list[[1]]$lon_center

  out <- list("hourly_data" = data, "station_df" = station_metadata,
              "radius" = radius,
              "lat_center" = lat_center,
              "lon_center" = lon_center)
  return(out)
}

#' Write hourly weather timeseries files for U.S. counties.
#'
#' Given a vector of U.S. county FIPS codes, this function saves each element of
#' the lists created from the function \code{daily_fips} to a separate folder
#' within a fiven directory. The dataframe \code{daily_data} is saved to a
#' subdirectory of the given directory called "data." This timeseries dataframe
#' gives 1. the values for specified weather variables, and 2. the number of
#' weather stations contributing to the average for each day within the
#' specified year(s). Metadata information about the weather stations and
#' county are saved in a list with four elements in a subdirectory called
#' "metadata." These elements include \code{station_metadata} (station metadata
#' for stations contributing to the time series dataframe), \code{radius}
#' (the radius (in km) within which weather stations were pulled from each
#' county's center), \code{lat_center}, and \code{lon_center} (the latitude
#' and longitude of the county's center). If the user specifies "csv" ouput for
#' the \code{metadata_type}, argument, \code{radius}, \code{lat_center}, and
#' \code{lon_center} are added to the \code{station_metadata} dataframe as three
#' additional columns.
#'
#' @return Writes out three subirectories of a fiven directory with hourly
#' weather files saved in "data", station and county metadata saved in
#' "metadata", and a map of weather station locations saved in "maps" for each
#' FIPS code specified. The user can specify either .rds or .csv files for the
#' data and metadatafiles, using the arguments \code{data_type} and
#' \code{metadata_type}, respectively. Maps are saved as .png files.
#'
#' @inheritParams hourly_df
#' @param out_directory The absolute or relative pathname for the directory
#'    where you would like the timeseries files to be saved.
#' @param data_type A character strign indicating that you would like either
#'    .rds files (data_type = "rds") or .csv files (data_type = "csv") for the
#'    timeseries output. This option defaults to .rds files.
#' @param metadata_type A character string indicating that you would like either
#'    .rds files (metadata_type = "rds") or .csv files (metadata_type = "csv")
#'    for the station and county metadata output. This option defaults to .rds
#'    files, in which case a list of four elements is saved
#'    (\code{station_metadata}, \code{radius}, \code{lat_center}, and
#'    \code{lon_center}). If the user specified "csv" output, \code{radius},
#'    \code{lat_center}, and \code{lon_center} are added to the
#'    \code{station_metadata} dataframe as additional columns.
#' @param keep_map TRUE / FALSE indicating if a map of the stations should
#'    be included. The map can substantially increase the size of the files. If
#'    FALSE, the "maps" subdirectory will not be created.
#' @param verbose TRUE / FALSE to indicate if you want the function to print
#'    out the county or vector of counties it's saving files for.
#'
#' @note If the function is unable to pull weather data for a particular county
#' given the specified percent coverage, date range, and/or weather variables,
#' \code{county_timeseries} will not produce a file for that county.
#'
#' @examples
#' \dontrun{
#' county_timeseries(fips = c("41005", "13089"), coverage = 0.90, year = 1992,
#'                   var = c("wind_speed", "temperature"),
#'                   out_directory = "~/timeseries_hourly")
#' }
#' @export
write_hourly_timeseries <- function(fips, coverage = NULL, year, var = "all",
                                    out_directory, data_type = "rds",
                                    meatadata_type = "rds", average_data = TRUE,
                                    station_label = FALSE, keep_map = TRUE,
                                    verbose = TRUE){

  if(verbose) {

    for(i in 1:length(fips)){
      if(i == 1){
        codes <- (paste0(fips[i], ", "))
      } else if (i == length(fips)) {
        codes <- paste0(codes, "and ", fips[i])
      } else {
        codes <- paste0(codes, fips[i], ", ")
      }
    }
    message(paste0("Saving daily weather files for FIPS codes ", codes,
                   " in the directory ", out_directory, ".", " This may take ",
                   "a while."))

  }

  if(!dir.exists(out_directory)){
    dir.create(out_directory)
  }

  if(!dir.exists(paste0(out_directory, "/data"))){
    dir.create(paste0(out_directory, "/data"))
  }

  if(!dir.exists(paste0(out_directory, "/metadata"))){
    dir.create(paste0(out_directory, "/metadata"))
  }

  for(i in 1:length(fips)) {
    possibleError <- tryCatch({

      out_list <- hourly_fips(fips = fips[i], year = year, var = var,
                               coverage = coverage,
                               average_data = average_data,
                              station_label = station_label)

      out_data <- out_list$hourly_data

      meta <- c(2, 4:6)
      out_metadata <- out_list[meta]

      if(data_type == "rds"){

        data_file <- paste0(out_directory, "/data", "/", fips[i], ".rds")
        saveRDS(out_data, file = data_file)

      } else if (data_type == "csv"){

        data_file <- paste0(out_directory, "/data", "/", fips[i], ".csv")
        utils::write.csv(out_data, file = data_file, row.names = FALSE)

      }

      if (metadata_type == "rds"){

        metadata_file <- paste0(out_directory, "/metadata", "/", fips[i],
                                ".rds")
        saveRDS(out_metadata, file = metadata_file)

      } else if (metadata_type == "csv"){

        out_metadata[[1]]$radius <- out_metadata[[2]]
        out_metadata[[1]]$lat_center <- out_metadata[[3]]
        out_metadata[[1]]$lon_center <- out_metadata[[4]]

        out_metadata <- out_metadata[[1]]

        metadata_file <- paste0(out_directory, "/metadata", "/", fips[i],
                                ".csv")
        utils::write.csv(out_metadata, file = metadata_file, row.names = FALSE)

      }


      if (keep_map == TRUE){

        if (!dir.exists(paste0(out_directory, "/maps"))){
          dir.create(paste0(out_directory, "/maps"))
        }

        out_map <- out_list$station_map

        map_file <- paste0(out_directory, "/maps")
        map_name <- paste0(fips[i], ".png")
        suppressMessages(ggplot2::ggsave(file = map_name, path = map_file,
                                         plot = out_map))

      }

    }
    ,
    error = function(e) {
      e
      message(paste0("Unable to pull weather data for FIPS code ", fips[i],
                   " for the specified percent coverage, year(s), and/or",
                   " weather variables."))
    }
    )
    if (inherits(possibleError, "error")) next

  }

}

#' Write plot files for hourly weather timeseries dataframes.
#'
#' This function writes out a directory with plots for every timeseries file
#' present in the specified directory (produced by the \code{write_hourly_timeseries}
#' function) for a particular weather variable. These plots are meant to aid in
#' initial exploratory analysis.
#'
#' @return Writes out a directory with plots of timeseries data for a given
#' weather variable for each file present in the directory specified.
#'
#' @param var A character string (all lower-case) specifying which weather
#'    variable present in the timeseries dataframe you would like to produce
#'    plots for. For example, var = "wind_speed".
#' @param data_directory The absolute or relative pathname for the directory
#'    where your daily timeseries dataframes (produced by \code{county_timeseries})
#'    are saved.
#' @param plot_directory The absolute or relative pathname for the directory
#'    where you would like the plots to be saved.
#' @param year A year or vector of years giving the year(s) present in the
#'    timeseries dataframe.
#' @param data_type A character string indicating the type of timeseries files
#'    you would like to produce plots for (either "rds" or "csv"). This option
#'    defaults to .rds files.
#'
#' @examples
#' \dontrun{
#'plot_hourly_timeseries(var = "wind_speed", year = 1992,
#'                data_directory = "~/timeseries_hourly",
#'                plot_directory = "~/timeseries_plots")
#'}
#' @importFrom dplyr %>%
#' @export
plot_hourly_timeseries <- function(var, year, data_directory,
                                  plot_directory, data_type = "rds"){

  files <- list.files(data_directory)

  date_min <- paste0(min(year), "-01-01 UTC")
  date_min <- as.POSIXct(date_min, tz = "UTC")

  date_max <- paste0(max(year), "-12-31 23:00:00 UTC")
  date_max <- as.POSIXct(date_max, tz = "UTC")

  if(!dir.exists(plot_directory)){
    dir.create(plot_directory)
  }

  if(data_type == "rds"){
    file_names <- gsub(".rds", "", files)
  } else if (data_type == "csv"){
    file_names <- gsub(".csv", files)
  }


    current_wd <- getwd()

  for(i in 1:length(files)){

    setwd(data_directory)
    dat <- readRDS(files[i])

    # convert tibble to vector (avoiding error "'x' and 'y' lengths differ")
    y <- dat %>% dplyr::collect() %>% .[[var]]

    file_name <- paste0(file_names[i], ".png")
    setwd(plot_directory)
    grDevices::png(filename = file_name)
    graphics::plot(dat$date_time, y,
                   type = "l", col = "red", main = file_names[i],
                   xlab = "date", ylab = var,
                   xlim = c(date_min, date_max)
                   )
    grDevices::dev.off()
  }

    setwd(current_wd)

}
