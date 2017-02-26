#' Pull average daily weather data by U.S. county.
#'
#' Given a particular county FIPS code, this function returns data and meta-data
#' for weather data, either for all available dates or for dates within a
#' requested date range.
#'
#' @inheritParams daily_df
#' @inheritParams daily_stations
#'
#' @param station_label TRUE / FALSE to indicate if you want your plot of
#'    weather station locations to include labels with station ids.
#' @param verbose TRUE / FALSE to indicate if you want the function to print
#'    out the name of the county it's processing.
#'
#' @return A list with three elements. The first element (\code{daily_data}) is a
#'    dataframe of daily weather data averaged across multiple stations, as well
#'    as columns (\code{"var"_reporting}) for each weather variable showing the
#'    number of stations contributing to the average for that variable on that
#'    day. The second element (\code{station_metadata}) is a dataframe of station
#'    metadata for stations included in the \code{daily_data} dataframe, as well
#'    as statistical information about these values. Columns
#'    include \code{id}, \code{name}, \code{var}, \code{latitude},
#'    \code{longitude}, \code{calc_coverage}, \code{standard_dev}, \code{min},
#'    \code{max}, and \code{range}. The third element (\code{station_map})
#'    is a plot showing locations of all weather stations for a particular county
#'    satisfying the conditions present in \code{daily_fips}'s arguments
#'    (\code{coverage}, \code{date_min}, \code{date_max}, and/or \code{var}).
#'
#' @note Because this function uses the NOAA API to identify the weather
#'    monitors within a U.S. county, you will need to get an access token from
#'    NOAA to use this function. Visit NOAA's token request page
#'    (\url{http://www.ncdc.noaa.gov/cdo-web/token}) to request a token by
#'    email. You then need to set that API code in your R session (e.g., using
#'    \code{options(noaakey = "your key")}, replacing "your key" with the API
#'    key you've requested from NOAA). See the package vignette for more details.
#'
#' @examples
#' \dontrun{
#' denver_ex <- daily_fips("08031", coverage = 0.90, date_min = "2010-01-01",
#'                  date_max = "2010-02-01", var = "prcp")
#'
#' head(denver_ex$daily_data)
#' denver_ex$station_map
#'
#' mobile_ex <- daily_fips("01097", date_min = "1997-07-13",
#'                         date_max = "1997-07-25", var = "prcp",
#'                         average_data = FALSE)
#' library(ggplot2)
#' ggplot(mobile_ex$daily_data, aes(x = date, y = prcp, color = id)) +
#'    geom_line()
#' }
#' @export
daily_fips <- function(fips, coverage = NULL, date_min = NULL, date_max = NULL,
                       var = "all", average_data = TRUE, station_label = FALSE,
                       verbose = TRUE) {
  census_data <- countyweather::county_centers
  loc_fips <- which(census_data$fips == fips)

  if (verbose) {

    message(paste0("Getting daily weather data for ",
                   census_data[loc_fips, "name"], ".",
                   " This may take a while."))

  }

  stations <- daily_stations(fips = fips, date_min = date_min,
                             date_max = date_max)
  weather_data <- daily_df(stations = stations,
                           var = var,
                           date_min = date_min,
                           date_max = date_max,
                           coverage = coverage,
                           average_data = average_data)
  station_map <- daily_stationmap(fips = fips,
                                  daily_data = weather_data,
                                  station_label = station_label)
  list <- list("daily_data" = weather_data$daily_data,
               "station_metadata" = weather_data$station_df,
               "station_map" = station_map)
  return(list)

}

#' Return average daily weather data for a particular county.
#'
#' Returns a list with data on weather and stations for a selected county.
#' This function serves as a wrapper to several functions from the \code{rnoaa}
#' package, which pull weather data from all relevant stations in a county.
#' This function filters and averages data returned by \code{rnoaa} functions
#' across all weather stations in a county based on user-specified
#' coverage specifications.
#'
#' @note Because this function uses the NOAA API to identify the weather
#'    monitors within a U.S. county, you will need to get an access token from
#'    NOAA to use this function. Visit NOAA's token request page
#'    (\url{http://www.ncdc.noaa.gov/cdo-web/token}) to request a token by
#'    email. You then need to set that API code in your R session (e.g., using
#'    \code{options(noaakey = "your key")}, replacing "your key" with the API
#'    key you've requested from NOAA). See the package vignette for more details.
#'
#' @param stations A dataframe containing station metadata, returned from
#'    the function \code{daily_stations}.
#' @param coverage A numeric value in the range of 0 to 1 that specifies
#'    the desired percentage coverage for the weather variable (i.e., what
#'    percent of each weather variable must be non-missing to include data from
#'    a monitor when calculating daily values averaged across monitors. The
#'    default is to include all monitors with any available data (i.e.,
#'    \code{coverage = 0}).)
#' @param var A character vector specifying desired weather variables. For
#'    example, \code{var = c("tmin", "tmax", "prcp")} for maximum temperature,
#'    minimum temperature, and precipitation. The default is \code{"all"},
#'    which includes all available weather variables at any weather station in
#'    the county. For a full list of all
#'    possible variable names, see NOAA's README file for the Daily Global
#'    Historical Climatology Network (GHCN-Daily) at
#'    \url{http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt}. Many of
#'    the weather variables are available for some, but not all, monitors, so
#'    your output from this function may not include all the variables
#'    specified using this argument. If you specify a variable here but it is
#'    not included in the output dataset, it means that it was not available in
#'    the time range for any monitor in the county.
#' @param average_data TRUE / FALSE to indicate if you want the function to
#'    average daily weather data across multiple monitors. If you choose
#'    FALSE, the function will return a dataframe with separate entries for
#'    each monitor, while TRUE (the default) outputs a single estimate
#'    for each day in the dataset, giving the average value of the weather
#'    metric across all available monitors in the county that day.
#' @inheritParams daily_stations
#'
#' @return A list with two elements. \code{daily_data} is a dataframe of daily
#'    weather data averaged across multiple monitors and includes columns
#'    (\code{"var"_reporting}) for each weather variable showing the number of
#'    stations contributing to the average for that variable on that day.
#'    The element \code{station_df} is a dataframe of station metadata for each
#'    station contributing weather data. A weather station will have one row per
#'    weather variable to which it contributes data. In addition to information
#'    such as station id, name, latitude, and longitude, the \code{station_df}
#'    dataframe includes statistical information about weather values
#'    contributed by each station for each weather variable. These statistics
#'    include \code{calc_coverage} (the percent of non-missing values for each
#'    station-weather variable combination for the specified date range),
#'    \code{standard_dev} (standard deviation), \code{max}, and \code{min},
#'    (giving the minimum and maximum values), and \code{range}, giving the
#'    range of values in each station-weather variable combination. The
#'    element \code{radius} is the calculated radius within which stations were
#'    pulled from the county's center. Elements \code{lat_center} and
#'    \code{lon_center} are the latitude and longitude of the county's center.
#'
#' @examples
#' \dontrun{
#' stations <- daily_stations(fips = "12086", date_min = "2010-01-01",
#'                            date_max = "2010-02-01")
#' fips_list <- daily_df(stations = stations, coverage = 0.90,
#'                  var = c("tmax", "tmin", "prcp"),
#'                  date_min = "2010-01-01", date_max = "2010-02-01")
#' averaged_data <- fips_list$daily_data
#' head(averaged_data)
#' station_info <- fips_list$station_df
#' head(station_info)
#' }
daily_df <- function(stations, coverage = NULL, var = "all", date_min = NULL,
                     date_max = NULL, average_data = TRUE) {

  # get tidy full dataset for all monitors
  quiet_pull_monitors <- purrr::quietly(rnoaa::meteo_pull_monitors)

  if (var == "all") {
    meteo_var <- "all"
  } else {
    meteo_var <- toupper(var)
  }

  meteo_df <- quiet_pull_monitors(monitors = stations$id,
                                  keep_flags = FALSE,
                                  date_min = date_min,
                                  date_max = date_max,
                                  var = meteo_var)$result

  # calculate coverage for each weather variable
  coverage_df <- rnoaa::meteo_coverage(meteo_df, verbose = FALSE)

  # filter station dataset based on specified coverage
  filtered <- filter_coverage(coverage_df,
                              coverage = coverage)
  good_monitors <- unique(filtered$id)

  # filter weather dataset based on stations with specified coverage
  filtered_data <- dplyr::filter_(meteo_df, ~ id %in% good_monitors)

  # steps to filter out erroneous data from individual stations
  # precipitation
  if ("prcp" %in% var) {
    filtered_data$prcp <- filtered_data$prcp / 10
    if (max(filtered_data$prcp, na.rm = TRUE) > 1100) {
      bad_prcp <- which(with(filtered_data, prcp > 1100))
      filtered_data <- filtered_data[-bad_prcp,]
    }
  }

  # snowfall
  if ("snow" %in% var) {
    if(max(filtered_data$snow, na.rm = TRUE) > 1600) {
      bad_snow <- which(with(filtered_data, snow > 1600))
      filtered_data <- filtered_data[-bad_snow,]
    }
  }

  # snow depth
  if ("snwd" %in% var) {
    if (max(filtered_data$snwd, na.rm = TRUE) > 11500) {
      bad_snwd <- which(with(filtered_data, snwd > 11500))
      filtered_data <- filtered_data[-bad_snwd,]
    }
  }

  # tmax
  if ("tmax" %in% var) {
    filtered_data$tmax <- filtered_data$tmax / 10
    if (max(filtered_data$tmax, na.rm = TRUE) > 57) {
      bad_tmax <- which(with(filtered_data, tmax > 57))
      filtered_data <- filtered_data[-bad_tmax,]
    }
  }

  # tmin
  if ("tmin" %in% var) {
    filtered_data$tmin <- filtered_data$tmin / 10
    if (min(filtered_data$tmin, na.rm = TRUE) < -62) {
      bad_tmin <- which(with(filtered_data, tmin < -62))
      filtered_data <- filtered_data[-bad_tmin,]
    }
  }

  all_cols <- colnames(filtered_data)
  not_vars <- c("id", "date")
  g_cols <- all_cols[!all_cols %in% not_vars]

  group_cols <- c("id", "key")

  stats <- filtered_data %>%
    dplyr::select_(quote(-date)) %>%
    tidyr::gather_(key_col = "key", value_col = "value", gather_cols = g_cols) %>%
    dplyr::group_by_(.dots = group_cols) %>%
    dplyr::summarize_(standard_dev = ~ sd(value, na.rm = TRUE),
                      min = ~ min(value, na.rm = TRUE),
                      max = ~ max(value, na.rm = TRUE),
                      range = ~ max - min)

  filtered <- dplyr::filter_(filtered, ~ id %in% good_monitors)
  stats <- dplyr::full_join(stats, filtered, by = c("id", "key"))

  stations <- dplyr::filter_(stations, ~ id %in% good_monitors)

  stations <- dplyr::full_join(stats, stations, by = "id") %>%
    dplyr::select_(quote(id), quote(name), quote(key), quote(latitude),
                   quote(longitude), quote(calc_coverage), quote(standard_dev),
                   quote(min), quote(max), quote(range))

  colnames(stations)[3] <- "var"

  if (average_data == TRUE) {
    filtered_data <- ave_daily(filtered_data)
  }

  out <- list("daily_data" = filtered_data, "station_df" = stations)

  return(out)

}

#' Write daily weather timeseries files for U.S. counties.
#'
#' Given a vector of U.S. county FIPS codes, this function saves each element of
#' the lists created from the function \code{daily_fips} to a separate folder
#' within a given directory. This function therefore allows you to pull and
#' save weather data time series for multiple counties at once.
#' The dataframe \code{daily_data} is saved to
#' a subdirectory of the given directory called "data." This timeseries
#' dataframe gives the values for specified weather variables and the
#' number of weather stations contributing to the average value for each day
#' within the specified date range. The element \code{station_metadata}, which
#' gives information about stations contributing to the time series, as well as
#' statistical information about the values contributed by these stations, is saved
#' in a subdirectory called "metadata." The element \code{station_map}, which is
#' a map of contributing station locations, is saved in a subdirectory called
#' "maps."
#'
#' @return Writes out three subdirectories of a given directory with daily
#'    weather files saved in "data", station metadata saved in "metadata",
#'    and a map of weather station locations saved in "maps" for each FIPS code
#'    specified provided there is available data for that county. The user can
#'    specify either .rds or .csv format for the data and metadata files, using
#'    the arguments \code{data_type} and \code{metadata_type}, respectively.
#'    Maps are saved as .png files.
#'
#' @inheritParams daily_df
#' @inheritParams daily_stations
#' @param out_directory The absolute or relative pathname for the directory
#'    where you would like the three subdirectories ("data", "metadata", and
#'    "plots") to be created.
#' @param data_type A character string indicating that you would like either
#'    .rds files (data_type = "rds") or .csv files (data_type = "csv") for the
#'    timeseries output. This option defaults to .rds files.
#' @param metadata_type A character string indicating that you would like either
#'    .rds files (metadata_type  = "rds") or .csv files (metadata_type = "csv")
#'    for the station metadata output. This option defaults to .rds files.
#' @param keep_map TRUE / FALSE indicating if a map of the stations should
#'    be included. The map can substantially increase the size of the files, so
#'    if file size is a concern, you should consider setting this option to
#'    FALSE. If FALSE, the "maps" subdirectory will not be created.
#' @param verbose TRUE / FALSE to indicate if you want the function to print
#'    the county or vector of counties it's saving files for as the function runs.
#' @param station_label TRUE / FALSE to indicate whether to include station
#'    labels in the station map.
#'
#' @note If the function is unable to pull weather data for a particular county
#'    given the specified percent coverage, date range, and/or weather variables,
#'    \code{daily_timeseries} will not produce files for that county.
#'
#' @examples
#' \dontrun{
#' write_daily_timeseries(fips = c("37055", "15005"), coverage = 0.90,
#'                        date_min = "1995-01-01", date_max = "1995-01-31",
#'                        var = c("tmax", "tmin", "prcp"),
#'                        out_directory = "~/timeseries")
#' }
#' @export
write_daily_timeseries <- function(fips, coverage = NULL, date_min = NULL,
                                   date_max = NULL, var = "all", out_directory,
                                   data_type = "rds", metadata_type = "rds",
                                   average_data = TRUE, station_label = FALSE,
                                   keep_map = TRUE, verbose = TRUE) {

  if (verbose) {

    if (length(fips) > 2) {
      for (i in 1:length(fips)) {
        if (i == 1) {
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

    } else if (length(fips == 2)) {

      for (i in 1:length(fips)) {
        if (i == 1) {
          codes <- paste0(fips[i], " ")
        } else if (i == length(fips)) {
          codes <- paste0(codes, "and ", fips[i])
        } else {
          codes <- paste0(codes, fips[i], ", ")
        }
      }
      message(paste0("Saving daily weather files for FIPS codes ", codes,
                     " in the directory ", out_directory, ".", " This may take ",
                     "a while."))

    } else {

      message(paste0("Saving daily weather files for FIPS code ", fips,
                     " in the directory ", out_directory, ".", " This may take ",
                     "a while."))

      }

    }


  if (!dir.exists(out_directory)) {
    dir.create(out_directory)
  }

  if (!dir.exists(paste0(out_directory, "/data"))) {
    dir.create(paste0(out_directory, "/data"))
  }

  if (!dir.exists(paste0(out_directory, "/metadata"))) {
    dir.create(paste0(out_directory, "/metadata"))
  }

  for (i in 1:length(fips)) {
    possibleError <- tryCatch({

      out_list <- daily_fips(fips = fips[i], date_min = date_min,
                             date_max = date_max, var = var, verbose = FALSE,
                             average_data = average_data,
                             station_label = station_label)
      out_data <- out_list$daily_data
      out_metadata <- out_list$station_metadata

      if (data_type == "rds") {

        data_file <- paste0(out_directory, "/data", "/", fips[i], ".rds")
        saveRDS(out_data, file = data_file)

      } else if (data_type == "csv") {

        data_file <- paste0(out_directory, "/data", "/", fips[i], ".csv")
        utils::write.csv(out_data, file = data_file, row.names = FALSE)

      }

      if (metadata_type == "rds") {

        metadata_file <- paste0(out_directory, "/metadata", "/", fips[i],
                                ".rds")
        saveRDS(out_metadata, file = metadata_file)

      } else if (metadata_type == "csv") {

        metadata_file <- paste0(out_directory, "/metadata", "/", fips[i],
                                ".csv")
        utils::write.csv(out_metadata, file = metadata_file)

      }

        if (keep_map == TRUE) {

          if (!dir.exists(paste0(out_directory, "/maps"))) {
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
                   " for the specified percent coverage, date range, and/or",
                   " weather variables."))
    }
    )
    if (inherits(possibleError, "error")) next

  }

}

#' Write plot files for daily weather timeseries dataframes.
#'
#' Writes a directory with plots for every weather data time series file
#' in the specified directory (as produced by the \code{daily_timeseries}
#' function and saved in the "data" subdirectory of the directory given in that
#' function's arguments) for a particular weather variable.
#'
#' @return Writes out a directory with plots of timeseries data for a given
#' weather variable for each file present in the directory specified.
#'
#' @param var A character string specifying which weather variable for which
#' you would like to produce plots (the variable must be present in the
#' timeseries dataframe).
#' @param data_directory The absolute or relative pathname for the directory
#'    where your daily timeseries dataframes (produced by \code{daily_timeseries})
#'    are saved.
#' @param plot_directory The absolute or relative pathname for the directory
#'    where you would like the plots to be saved.
#' @param date_min A character string giving the earliest date present in the
#'    timeseries dataframe in "yyyy-mm-dd" format.
#' @param date_max  A character string giving the latest date present in the
#'    timeseries dataframe in "yyyy-mm-dd" format.
#' @param data_type A character string indicating the type of timeseries files
#'    you would like to produce plots for (either \code{"rds"} or \code{"csv"}).
#'    This option defaults to .rds files.
#'
#' @examples
#' \dontrun{
#' write_daily_timeseries(fips = c("37055", "15005"), coverage = 0.90,
#'                        date_min = "1995-01-01", date_max = "1995-01-31",
#'                        var = c("tmax", "tmin", "prcp"),
#'                        out_directory = "~/timeseries")
#' plot_daily_timeseries(var = "prcp", date_min = "1995-01-01",
#'                       date_max = "1995-01-31",
#'                       data_directory = "~/timeseries/data",
#'                       plot_directory = "~/timeseries/plots_prcp")
#' }
#' @importFrom dplyr %>%
#'
#' @export
plot_daily_timeseries <- function(var, date_min, date_max, data_directory,
                                  plot_directory, data_type = "rds") {

  files <- list.files(data_directory)

  if (!dir.exists(plot_directory)) {
    dir.create(plot_directory)
  }

  if (data_type == "rds") {
    file_names <- gsub(".rds", "", files)
  } else if (data_type == "csv"){
    file_names <- gsub(".csv", "", files)
  }


  for (i in 1:length(files)) {
    dat <- readRDS(paste0(data_directory, "/", files[i]))
      weather <- dplyr::ungroup(dat) %>%
        as.data.frame()

    file_name <- paste0(file_names[i], ".png")
    grDevices::png(filename = paste0(plot_directory, "/", file_name))
    weather$to_plot <- weather[ , var]
    graphics::plot(weather$date, weather$to_plot,
         type = "l", col = "red", main = file_names[i],
         xlab = "date", ylab = var,
         xlim = c(as.Date(date_min), as.Date(date_max)))
    grDevices::dev.off()
  }

}
