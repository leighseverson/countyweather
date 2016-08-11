#' Return average daily weather data and a plot showing the location of weather
#' stations for a particular county.
#'
#' Given a particular county FIPS code, this function returns a list with two
#' elements: "data", a dataframe of daily average weather values, and "plot",
#' a plot showing the location of weather stations contributing to the average
#' weather in "data".
#'
#' @inheritParams daily_df
#'
#' @return A list with two elements. The first element (\code{daily_data}) is a
#'    dataframe of daily weather data averaged across multiple stations, as well
#'    as columns (\code{"var"_reporting}) for each weather variable showing the
#'    number of stations contributing to the average for that variable on that
#'    day. The second element (\code{station_map}) is a plot showing points for all
#'    weather stations for a particular county satisfying the conditions present
#'    in \code{daily_fips}'s arguments (coverage, date_min, date_max,
#'    and/or var).
#'
#' @note You must have a NOAA API to use this function, and you need to set
#'    that API code in your R session (e.g., using
#'    \code{options(noaakey = "your key"}).
#'
#' @examples
#' \dontrun{
#' ex <- daily_fips("08031", coverage = 0.90,
#'                    date_min = "2010-01-01", date_max = "2010-02-01",
#'                    var = "PRCP")
#'
#' weather_data <- ex$daily_data
#' station_map <- ex$station_map
#' }
#' @export
daily_fips <- function(fips, coverage = NULL,
                         date_min = NULL, date_max = NULL,
                         var = c("PRCP", "SNOW", "SNWD", "TMAX", "TMIN"),
                         average_data = TRUE){
  stations <- fips_stations(fips = fips, date_min = date_min,
                            date_max = date_max)
  weather_data <- daily_df(stations = stations,
                                  date_min = date_min,
                                  date_max = date_max,
                                  coverage = coverage,
                                  var = var,
                                  average_data = average_data)
  station_map <- stationmap_fips(fips = fips,
                                 weather_data = weather_data)
  list <- list("daily_data" = weather_data$daily_data,
               "station_map" = station_map)
  return(list)
}

#' Return average daily weather data for a particular county.
#'
#' \code{daily_df} returns a list of two elements. The element
#' \code{averaged} is a dataframe of average daily weather values for a
#' particular county, date range, and/or specified "coverage." The element
#' \code{stations} is a dataframe containing metadata about stations
#' contributing to the average weather values in the \code{averaged} dataframe.
#' Columns in the \code{stations} dataframe include each station's latitude,
#' longitude, id, and name.
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
#' @param stations A dataframe containing station metadata, returned from
#'    the function \code{fips_stations}.
#' @param coverage A numeric value in the range of 0 to 1 that specifies
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
#' @param average_data TRUE / FALSE to indicate if you want the function to
#'    average daily weather data across multiple monitors.
#'
#' @return A list with two elements. \code{daily_data} is a dataframe of daily
#'    weather data averaged across multiple monitors, as well as columns
#'    (\code{"var"_reporting}) for each weather variable showing the number of
#'    stations contributing to the average for that variable on that day.
#'    The element \code{station_df} is a vector of weather stations contributing
#'    to the average value in the \code{daily_data} dataframe.
#'
#' @examples
#' \dontrun{
#' stations <- fips_stations(fips = "12086", date_min = "2010-01-01",
#'                           date_max = "2010-02-01")
#' list <- daily_df(stations = stations, coverage = 0.90,
#'                       var = c("TMAX", "TMIN", "PRCP"),
#'                       date_min = "2010-01-01", date_max = "2010-02-01")
#' averaged_data <- list$daily_data
#' station_info <- list$stations
#' }
daily_df <- function(stations, coverage = NULL,
                            var = c("PRCP", "SNOW", "SNWD", "TMAX", "TMIN"),
                            date_min = NULL, date_max = NULL,
                            average_data = TRUE){

  # get tidy full dataset for all monitors
  # meteo_pull_monitors() from helpers_ghcnd.R in ropenscilabs/rnoaa
  quiet_pull_monitors <- purrr::quietly(rnoaa::meteo_pull_monitors)
  meteo_df <- quiet_pull_monitors(monitors = stations$id,
                                  keep_flags = FALSE,
                                  date_min = date_min,
                                  date_max = date_max,
                                  var = var)$result

  # calculate coverage for each weather variable
  # meteo_coverage() from meteo_utils.R in ropenscilabs/rnoaa
  coverage_df <- rnoaa::meteo_coverage(meteo_df, verbose = FALSE)

  # filter station dataset based on specified coverage
  filtered <- filter_coverage(coverage_df,
                              coverage = coverage)
  good_monitors <- unique(filtered$id)

  # filter weather dataset based on stations with specified coverage
  filtered_data <- dplyr::filter_(meteo_df, ~ id %in% good_monitors)

  # steps to filter out erroneous data from individual stations
  # precipitation
  if("PRCP" %in% var){
    if(max(filtered_data$prcp, na.rm = TRUE) > 1100){
      bad_prcp <- which(with(filtered_data, prcp > 1100))
      filtered_data <- filtered_data[-bad_prcp,]
    }
  }

  # snowfall
  if("SNOW" %in% var){
    if(max(filtered_data$snow, na.rm = TRUE) > 1600){
      bad_snow <- which(with(filtered_data, snow > 1600))
      filtered_data <- filtered_data[-bad_snow,]
    }
  }

  # snow depth
  if("SNWD" %in% var){
    if(max(filtered_data$snwd, na.rm = TRUE) > 11500){
      bad_snwd <- which(with(filtered_data, snwd > 11500))
      filtered_data <- filtered_data[-bad_snwd,]
    }
  }

  # tmax
  if("TMAX" %in% var){
    if(max(filtered_data$tmax, na.rm = TRUE) > 57){
      bad_tmax <- which(with(filtered_data, tmax > 57))
      filtered_data <- filtered_data[-bad_tmax,]
    }
  }

  # tmin
  if("TMIN" %in% var){
    if(min(filtered_data$tmin, na.rm = TRUE) < -62){
      bad_tmin <- which(with(filtered_data, tmin < -62))
      filtered_data <- filtered_data[-bad_tmin,]
    }
  }

  # average across stations, add a column for number of stations that
  # contributed to each daily average
  if(average_data == TRUE){
    filtered_data <- ave_weather(filtered_data)
  }

  stations <- dplyr::filter_(stations, ~ id %in% good_monitors)

  out <- list("daily_data" = filtered_data, "station_df" = stations)

  return(out)
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
#' @importFrom dplyr %>%
#'
ave_weather <- function(weather_data){

  all_cols <- colnames(weather_data)
  not_vars <- c("id", "date")
  g_cols <- all_cols[!all_cols %in% not_vars]

  #not sure about -id -date cols - how to do NSE here
  averaged_data <- tidyr::gather_(weather_data, key_col = "key",
                                 value_col = "value",
                                 gather_cols = g_cols) %>%
    dplyr::group_by_(.dots = c("date", "key")) %>%
    dplyr::summarize_(mean = ~ mean(value, na.rm = TRUE)) %>%
    tidyr::spread_(key_col = "key", value_col = "mean")

  n_reporting <- tidyr::gather_(weather_data, key_col = "key",
                                value_col = "value",
                                gather_cols = g_cols) %>%
    dplyr::group_by_(.dots = c("date", "key")) %>%
    dplyr::summarize_(n_reporting = ~ sum(!is.na(value))) %>%
    dplyr::mutate_(key = ~ paste(key, "reporting", sep = "_")) %>%
    tidyr::spread_(key_col = "key", value_col = "n_reporting")

  averaged_data <- dplyr::left_join(averaged_data, n_reporting,
                             by = "date")
  return(averaged_data)
}

#' Filter stations based on "coverage" requirements.
#'
#' \code{filter_coverage} filters available weather variables
#' based on a specified required minimum coverage (i.e., percent non-missing
#' daily observations).
#'
#' @param coverage_df A \code{meteo_coverage} dataframe
#' @param coverage A numeric value in the range of 0 to 1 that specifies
#'    the desired percentage coverage for the weather variable (i.e., what
#'    percent of each weather variable must be non-missing to include data from
#'    a monitor when calculating daily values averaged across monitors.
#'    (Optional.)
#'
#' @return a \code{dataframe} with stations that meet the specified coverage
#'    requirements for weather variables included in the dataframe present in
#'    this function's arguments.
#'
#' @importFrom dplyr %>%
#'
filter_coverage <- function(coverage_df, coverage = NULL){

  if (is.null(coverage)){
    coverage <- 0
  }

  filtered <- dplyr::select_(coverage_df,
                            .dots = list("-start_date", "-end_date",
                                      "-total_obs")) %>%
    tidyr::gather(key, covered, -id)  %>%
    dplyr::filter_(~ covered >= coverage) %>%
    dplyr::mutate_(covered = ~ 1) %>%
    dplyr::group_by_(.dots = list("id")) %>%
    dplyr::mutate_(good_monitor = ~ sum(!is.na(covered)) > 0) %>%
    dplyr::ungroup() %>%
    dplyr::filter_(~ good_monitor) %>%
    dplyr::select_(.dots = list("-good_monitor"))
  return(filtered)
}

#' Plot daily weather stations for a particular county
#'
#' @param fips A character string giving the five-digit U.S. FIPS county code
#'    of the county for which the user wants to pull weather data.
#' @param weather_data An object returned from \code{daily_df}.
#' @param point_color The specified \code{ggplot2} color for each point
#'    representing the location of a station. (Optional. This argument defaults
#'    to "firebrick.")
#' @param point_size The specified \code{ggplot2} size for each point
#'    representing the location of a station. (Optional. The default size is 2).
#'
#' @return A plot showing points for all weather stations for a particular
#'    county satisfying the conditions present in \code{weather_fip_df}'s
#'    arguments (coverage, date_min, date_max, and/or var).
#'    (\code{stationmap_fips} takes the resulting weather dataframe from this
#'    function.)
#'
#' @examples
#' \dontrun{
#' all_stations <- fips_stations(fips = "12086", date_min = "1999-08-01",
#'                           date_max = "1999-08-31")
#' weather_data <- daily_df(stations = all_stations, coverage =
#'                                 0.90, var = "PRCP", date_min = "1999-08-01",
#'                                 date_max = "1999-08-31")$daily_data
#' stationmap_fips(fips = "12086", weather_data = weather_data)
#' }
#'
#' @importFrom dplyr %>%
#'
stationmap_fips <- function(fips, weather_data, point_color = "firebrick",
                            point_size = 2){

  census_data <- countyweather::county_centers
  row_num <- which(grepl(fips, census_data$fips))
  choro_fips <- as.numeric(census_data[row_num, "fips"])
  title <- census_data[row_num, "name"]

  to_map <- dplyr::select_(census_data, region = ~ region) %>%
    dplyr::mutate_(value = 1)

  map <- suppressMessages(choroplethr::county_choropleth(to_map,
                                        title = "", legend = "",
                           num_colors = 1, state_zoom = NULL,
                           county_zoom = choro_fips, reference_map = TRUE))

  map <- map + ggplot2::geom_point(data = weather_data$stations,
                                   ggplot2::aes_(~ longitude, ~ latitude),
                          colour = point_color, size = point_size) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(title)

  return(map)
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
#' @inheritParams daily_df
#' @param out_directory The absolute or relative pathname for the directory
#' where you would like the timeseries files to be saved.
#' @param out_type A character string indicating that you would like either .rds
#' files (out_type = "rds") or .csv files (out_type = ".csv"). This option
#' defaults to .rds files.
#'
#' @note If the function is unable to pull weather data for a particular county
#' given the specified percent coverage, date range, and/or weather variables,
#' \code{daily_timeseries} will not produce a file for that county.
#'
#' @examples
#' \dontrun{
#' daily_timeseries(fips = c("41005", "13089"), coverage = 0.90,
#'            date_min = "2000-01-01", date_max = "2000-01-10",
#'            var = c("TMAX", "TMIN", "PRCP"),
#'            out_directory = "~/timeseries_data")
#' }
#'
#' @export
daily_timeseries <- function(fips, coverage, date_min, date_max, var,
                              average_data = TRUE,
                              out_directory, out_type = "rds"){

  if(!dir.exists(out_directory)){
    dir.create(out_directory)
  }
  for(i in 1:length(fips)) {
    possibleError <- tryCatch({
      stations <- fips_stations(fips = fips[i], date_min = date_min,
                                date_max = date_max)
      out_df <- daily_df(stations = stations,
                                coverage = coverage,
                                var = var, date_min = date_min,
                                date_max = date_max)$daily_data

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
#' present in the specified directory (produced by the \code{daily_timeseries}
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
#' where your daily timeseries dataframes (produced by \code{daily_timeseries})
#' are saved.
#' @param file_type A character string indicating the type of timeseries files
#' you would like to produce plots for (either "rds" or "csv"). This option
#' defaults to .rds files.
#' @param plot_directory The absolute or relative pathname for the directory
#' where you would like the plots to be saved.
#' @param date_min A character string giving the earliest date present in the
#' timeseries dataframe in "yyyy-mm-dd" format.
#' @param date_max  A character string giving the latest date present in the
#' timeseries dataframe in "yyyy-mm-dd" format.
#'
#' @examples
#' \dontrun{
#'plot_daily_timeseries(var = "prcp",
#'                file_directory = "~/Desktop/exposure_data/ihapps_timeseries",
#'                plot_directory = "~/Desktop/exposure_data/plots_prcp")
#'
#'plot_daily_timeseries(files = files, var = "tmax",
#'                file_directory = "~/Desktop/exposure_data/ihapps_timeseries",
#'                plot_directory = "~/Desktop/exposure_data/plots_tmax")
#'
#'plot_daily_timeseries(var = "tmin",
#'                file_directory = "~/Desktop/exposure_data/ihapps_timeseries",
#'                plot_directory = "~/Desktop/exposure_data/plots_tmin")
#' }
#' @export
plot_daily_timeseries <- function(var, file_directory, file_type = "rds",
                            plot_directory, date_min, date_max){

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
    grDevices::png(filename = file_name)
    graphics::plot(data$date, data[,var],
         type = "l", col = "red", main = file_names[i],
         xlab = "date", ylab = var,
         xlim = c(as.Date(date_min), as.Date(date_max)))
    grDevices::dev.off()
  }

}