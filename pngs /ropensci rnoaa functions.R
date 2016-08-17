#' Pull GHCND weather data for multiple weather monitors
#'
#' This function takes a vector of one or more weather station IDs. It will pull
#' the weather data from the Global Historical Climatology Network's daily
#' data (GHCND) for each of the stations and join them together in a single tidy
#' dataframe. For any weather stations that the user calls that are not
#' available by ftp from GHCND, the function will return a warning
#' giving the station ID.
#'
#' @param monitors A character vector listing the station IDs for all
#'    weather stations the user would like to pull. To get a full and
#'    current list of stations, the user can use the \code{\link{ghcnd_stations}}
#'    function. To identify stations within a certain radius of a location, the
#'    user can use the \code{\link{meteo_nearby_stations}} function.
#' @inheritParams meteo_tidy_ghcnd
#' @inheritParams ghcnd_search
#'
#' @return A data frame of daily weather data for multiple weather monitors,
#'    converted to a tidy format. All weather variables may not exist for all
#'    weather stations. Examples of variables returned are:
#'    \itemize{
#'    \item \code{id}: Character string with the weather station site id
#'    \item \code{date}: Date of the observation
#'    \item \code{prcp}: Precipitation, in mm
#'    \item \code{tavg}: Average temperature, in degrees Celsius
#'    \item \code{tmax}: Maximum temperature, in degrees Celsius
#'    \item \code{tmin}: Minimum temperature, in degrees Celsius
#'    \item \code{awnd}: Average daily wind speed, in meters / second
#'    \item \code{wsfg}: Peak gust wind speed, in meters / second
#'    }
#'    There are other possible weather variables in the Global Historical
#'    Climatology Network; see
#'    \url{http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt} for a full
#'    list. If the \code{var} argument is something other than "all", then
#'    only variables included in that argument will be included in the output
#'    data frame. The variables \code{prcp}, \code{tmax}, \code{tmin}, and \code{tavg}
#'    have all been converted from tenths of their metric to the metric (e.g.,
#'    from tenths of degrees Celsius to degrees Celsius). All other variables
#'    are in the units specified in the linked file.
#'
#' @note The weather flags, which are kept by specifying
#' \code{keep_flags = TRUE} are:
#' \itemize{
#' \item \code{*_mflag}: Measurement flag, which gives some information on how
#'    the observation was measured.
#' \item \code{*_qflag}: Quality flag, which gives quality information on the
#'    measurement, like if it failed to pass certain quality checks.
#' \item \code{*_sflag}: Source flag. This gives some information on the
#'    weather collection system (e.g., U.S. Cooperative Summary of the Day,
#'    Australian Bureau of Meteorology) the weather observation comes from.
#' }
#' More information on the interpretation of these flags can be found in the
#' README file for the NCDC's Daily Global Historical Climatology Network's
#' data at \url{http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt}.
#'
#' @note This function may take a while to run.
#'
#' @author Brooke Anderson \email{brooke.anderson@@colostate.edu}
#'
#' @references
#'
#' For more information about the data pulled with this function, see:
#'
#' Menne, M.J., I. Durre, R.S. Vose, B.E. Gleason, and T.G. Houston, 2012:
#' An overview of the Global Historical Climatology Network-Daily Database.
#' Journal of Atmospheric and Oceanic Technology, 29, 897-910,
#' doi:10.1175/JTECH-D-11-00103.1.
#'
#' @examples
#' \dontrun{
#'
#' monitors <- c("ASN00003003", "ASM00094299", "ASM00094995", "ASM00094998")
#' all_monitors_clean <- meteo_pull_monitors(monitors)
#'
#' }
#'
#' @export
meteo_pull_monitors <- function(monitors, keep_flags = FALSE, date_min = NULL,
                                date_max = NULL, var = "all"){
  monitors <- unique(monitors)

  safe_meteo_tidy_ghcnd <- purrr::safely(meteo_tidy_ghcnd)
  all_monitors_clean <- lapply(monitors, safe_meteo_tidy_ghcnd,
                               keep_flags = keep_flags, date_min = date_min,
                               date_max = date_max, var = var)

  check_station <- sapply(all_monitors_clean, function(x) is.null(x$result))
  bad_stations <- monitors[check_station]
  if(length(bad_stations) > 0){
    warning(paste("The following stations could not be pulled from",
                  "the GHCN ftp:\n", paste(bad_stations, collapse = ", "),
                  "\nAny other monitors were successfully pulled from GHCN."))
  }

  all_monitors_out <- lapply(all_monitors_clean[!check_station],
                             function(x) x$result)
  all_monitors_out <- suppressWarnings(dplyr::bind_rows(all_monitors_out))
  return(all_monitors_out)
}

#' Create a tidy GHCND dataset from a single monitor
#'
#' This function inputs an object created by \code{\link{ghcnd}} and cleans up
#' the data into a tidy form.
#'
#' @param keep_flags TRUE / FALSE for whether the user would like to keep all the flags
#'    for each weather variable. The default is to not keep the flags (FALSE).
#'    See the note below for more information on these flags.
#' @inheritParams ghcnd_search
#'
#' @return A data frame of daily weather data for a single weather monitor,
#'    converted to a tidy format. All weather variables may not exist for all
#'    weather stations. Examples of variables returned are:
#'    \itemize{
#'    \item \code{id}: Character string with the weather station site id
#'    \item \code{date}: Date of the observation
#'    \item \code{prcp}: Precipitation, in mm
#'    \item \code{tavg}: Average temperature, in degrees Celsius
#'    \item \code{tmax}: Maximum temperature, in degrees Celsius
#'    \item \code{tmin}: Minimum temperature, in degrees Celsius
#'    \item \code{awnd}: Average daily wind speed, in meters / second
#'    \item \code{wsfg}: Peak gust wind speed, in meters / second
#'    }
#'    There are other possible weather variables in the Global Historical
#'    Climatology Network; see
#'    \url{http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt} for a full
#'    list. The variables \code{prcp}, \code{tmax}, \code{tmin}, and \code{tavg}
#'    have all been converted from tenths of their metric to the metric (e.g.,
#'    from tenths of degrees Celsius to degrees Celsius). All other variables
#'    are in the units specified in the linked file.
#'
#' @note The weather flags, which are kept by specifying
#' \code{keep_flags = TRUE} are:
#' \itemize{
#' \item \code{*_mflag}: Measurement flag, which gives some information on how
#'    the observation was measured.
#' \item \code{*_qflag}: Quality flag, which gives quality information on the
#'    measurement, like if it failed to pass certain quality checks.
#' \item \code{*_sflag}: Source flag. This gives some information on the
#'    weather collection system (e.g., U.S. Cooperative Summary of the Day,
#'    Australian Bureau of Meteorology) the weather observation comes from.
#' }
#' More information on the interpretation of these flags can be found in the
#' README file for the NCDC's Daily Global Historical Climatology Network's
#' data at \url{http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt}.
#'
#' @author Brooke Anderson \email{brooke.anderson@@colostate.edu}
#'
#' @seealso \code{\link{meteo_pull_monitors}}
#'
#' @examples
#' \dontrun{
#' # One station in Australia is ASM00094275
#' cleaned_df <- meteo_tidy_ghcnd(stationid = "ASN00003003")
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
meteo_tidy_ghcnd <- function(stationid, keep_flags = FALSE, var = "all",
                             date_min = NULL, date_max = NULL){

  dat <- suppressWarnings(ghcnd_search(stationid = stationid, var = var,
                                       date_min = date_min,
                                       date_max = date_max)) %>%
    lapply(meteo_tidy_ghcnd_element, keep_flags = keep_flags)
  cleaned_df <- do.call(rbind.data.frame, dat) %>%
    tidyr::spread_("key", "value")

  which_vars_tenths <- which(colnames(cleaned_df) %in%
                               c("prcp", "tmax", "tmin", "tavg"))
  cleaned_df <- dplyr::tbl_df(cleaned_df)
  # All these variables are in tenths of units
  cleaned_df[, which_vars_tenths] <- vapply(cleaned_df[, which_vars_tenths],
                                            FUN.VALUE = numeric(nrow(cleaned_df)),
                                            FUN = function(x){
                                              x <- ifelse(x == -9999, NA, x)
                                              x <- as.numeric(x) / 10
                                            })

  which_other_vars <- which(colnames(cleaned_df) %in%
                              c("snow", "snwd"))
  cleaned_df[, which_other_vars] <- vapply(cleaned_df[, which_other_vars],
                                           FUN.VALUE = numeric(nrow(cleaned_df)),
                                           FUN = function(x){
                                             x <- ifelse(x == -9999, NA, x)
                                             x <- as.numeric(x)
                                           })
  return(cleaned_df)
}

#' Restructure element of ghcnd_search list
#'
#' This function restructures a single element of the list object created
#' by \code{\link{ghcnd_search}}, to add a column giving the variable name
#' (\code{key}) and change the name of the variable column to \code{value}.
#' These changes facilitate combining all elements from the list created by
#' \code{\link{ghcnd_search}}, to create a tidy dataframe of the weather
#' observations from the station.
#'
#' @param x A dataframe with daily observations for a single monitor for a
#'    single weather variable. This dataframe is one of the elements returned
#'    by \code{\link{ghcnd_search}}.
#' @inheritParams meteo_tidy_ghcnd
#'
#' @return A dataframe reformatted to allow easy aggregation of all weather
#'    variables for a single monitor.
#'
#' @author Brooke Anderson \email{brooke.anderson@@colostate.edu}
meteo_tidy_ghcnd_element <- function(x, keep_flags = FALSE){
  var_name <- colnames(x)[2]
  if(keep_flags){
    flag_locs <- grep("flag", colnames(x))
    colnames(x)[flag_locs] <- paste(colnames(x)[flag_locs], var_name, sep = "_")
    x <- tidyr::gather_(x, "key", "value",
                        gather_cols =  dplyr::select_vars(names(x), -id, -date))
  } else {
    x <- dplyr::select_(x, "-ends_with('flag')")
    x <- tidyr::gather_(x, "key", "value",
                        gather_cols =  dplyr::select_vars(names(x), -id, -date))
  }
  return(x)
}

#' Determine the "coverage" for a station data frame
#'
#' Call this function after pulling down observations for a set of stations
#' to retrieve the "coverage" (i.e. how complete each field is). If either
#' or both \code{obs_start_date} or \code{obs_end_date} are specified,
#' the coverage test will be limited to that date range.
#'
#' There is an \code{autoplot} method for the output of this function.
#' @importFrom scales comma
#' @param meteo_df an \emph{meteo} \code{data.frame}
#' @param obs_start_date specify either or both (obs_start_date, obs_end_date) to constrain
#'        coverate tests. These should be \code{Date} objects.
#' @param obs_end_date specify either or both (obs_start_date, obs_end_date) to constrain
#'        coverate tests. These should be \code{Date} objects.
#' @param verbose if \code{TRUE} will display the coverage summary along
#'        with returning the coverage data.frame
#' @return a \code{data.frame} with the coverage for each station, minimally
#' containing: \preformatted{
#' $ id         (chr)
#' $ start_date (time)
#' $ end_date   (time)
#' $ total_obs  (int)
#' }
#' with additional fields (and their coverage percent) depending on what
#' was available for the weather station.
#' @export
#' @examples
#' monitors <- c("ASN00095063", "ASN00024025", "ASN00040112", "ASN00041023",
#'              "ASN00009998", "ASN00066078", "ASN00003069", "ASN00090162",
#'              "ASN00040126", "ASN00058161")
#' obs <- meteo_pull_monitors(monitors)
#' obs_covr <- meteo_coverage(obs)
#' autoplot(obs_covr)
meteo_coverage <- function(meteo_df,
                           obs_start_date=NULL,
                           obs_end_date=NULL,
                           verbose=FALSE) {

  if (!is.null(obs_start_date)) {
    dots <- list(~as.Date(date) >= obs_start_date)
    meteo_df <- dplyr::filter_(meteo_df, .dots = dots)
  }

  if (!is.null(obs_end_date)) {
    dots <- list(~as.Date(date) <= obs_end_date)
    meteo_df <- dplyr::filter_(meteo_df, .dots = dots)
  }

  dplyr::group_by_(meteo_df, ~id) %>%
    dplyr::do({
      rng <- range(.$date)
      dat <- data.frame(start_date = rng[1],
                        end_date = rng[2],
                        total_obs = nrow(.), stringsAsFactors=FALSE)
      if (verbose) cat(sprintf("Station Id: %s\n", .$id[1]))
      if (verbose) cat(sprintf("\n  Date range for observations: %s\n\n",
                               paste0(as.character(rng), sep="", collapse=" to ")))
      if (verbose) cat(sprintf("  Total number of observations: %s\n\n",
                               scales::comma(nrow(.))))
      meteo_cols <- dplyr::setdiff(colnames(.), c("id", "date"))
      col_cov <- lapply(meteo_cols, function(x, n) {
        if (verbose) cat(sprintf("  Column %s completeness: %5s\n",
                                 formatC(sprintf("'%s'", x), width = (n+2)),
                                 scales::percent(sum(!is.na(.[,x])) / nrow(.))))
        sum(!is.na(.[,x])) / nrow(.)
      }, max(vapply(colnames(.), nchar, numeric(1), USE.NAMES=FALSE)))
      if (verbose) cat("\n")
      col_cov <- setNames(cbind.data.frame(col_cov, stringsAsFactors=FALSE), meteo_cols)
      dplyr::bind_cols(dat, col_cov)
    }) -> out
  class(out) <- c("meteo_coverage", class(out))
  if (verbose) return(invisible(out))
  out
}

autoplot.meteo_coverage <- function(df) {

  gg <- ggplot2::ggplot(df)
  gg <- gg + ggplot2::geom_segment(data = df, ggplot2::aes(x = reorder(id, start_date),
                                                           xend = reorder(id, start_date),
                                                           y = start_date, yend = end_date))
  gg <- gg + ggplot2::scale_x_discrete(expand = c(0, 0.25))
  gg <- gg + ggplot2::scale_y_datetime(expand = c(0, 0))
  gg <- gg + ggplot2::coord_flip()
  gg <- gg + ggplot2::labs(x = NULL, y = NULL, title = "Time coverage by station")
  gg <- gg + ggplot2::theme_bw(base_family = "Arial Narrow")
  gg <- gg + ggplot2::theme(panel.grid = element_line(color="#b2b2b2", size=0.1))
  gg <- gg + ggplot2::theme(panel.grid.major.x = element_line(color = "#b2b2b2", size = 0.1))
  gg <- gg + ggplot2::theme(panel.grid.major.y = element_blank())
  gg <- gg + ggplot2::theme(panel.grid.minor = element_blank())
  gg <- gg + ggplot2::theme(panel.border = element_blank())
  gg <- gg + ggplot2::theme(axis.ticks = element_blank())
  gg <- gg + ggplot2::theme(plot.title = element_text(margin = margin(b = 12)))
  ggtime <- gg

  df_long <- tidyr::gather(dplyr::select(df, -start_date, -end_date, -total_obs),
                           observation, value, -id)

  gg <- ggplot2::ggplot(df_long)
  gg <- gg + ggplot2::geom_segment(aes(x = 0, xend = value,
                                       y = observation, yend = observation, group = id))
  gg <- gg + ggplot2::scale_x_continuous(labels = percent, limits = c(0, 1))
  gg <- gg + ggplot2::facet_wrap(~id, scales = "free_x")
  gg <- gg + ggplot2::labs(x = NULL, y = NULL, title = "Observation coverage by station")
  gg <- gg + ggplot2::theme_bw(base_family = "Arial Narrow")
  gg <- gg + ggplot2::theme(panel.grid = element_line(color = "#b2b2b2", size = 0.1))
  gg <- gg + ggplot2::theme(panel.grid.major.x = element_line(color = "#b2b2b2", size = 0.1))
  gg <- gg + ggplot2::theme(panel.grid.major.y = element_blank())
  gg <- gg + ggplot2::theme(panel.grid.minor = element_blank())
  gg <- gg + ggplot2::theme(panel.border = element_blank())
  gg <- gg + ggplot2::theme(axis.ticks = element_blank())
  gg <- gg + ggplot2::theme(plot.title = element_text(margin = margin(b = 12)))
  gg <- gg + ggplot2::theme(strip.background = element_blank())
  gg <- gg + ggplot2::theme(strip.text = element_text(hjust = 0))
  gg <- gg + ggplot2::theme(panel.margin.x = grid::unit(12, "pt"))
  gg <- gg + ggplot2::theme(panel.margin.y = grid::unit(8, "pt"))
  gg <- gg + ggplot2::theme(plot.margin = margin(t = 30, b = 5, l = 20, r = 20))
  ggobs <- gg

  gridExtra::grid.arrange(ggtime, ggobs, ncol=1, heights=c(0.4, 0.6))

}


#' Find weather monitors near locations
#'
#' This function inputs a dataframe with latitudes and longitudes of locations
#' and creates a dataframe with monitors within a certain radius of those
#' locations. The function can also be used, with the \code{limit} argument, to pull
#' a certain number of the closest weather monitors to each location.
#' The weather monitor IDs in the output dataframe can be used with other
#' \code{rnoaa} functions to pull data from all available weather stations near
#' a location (e.g., \code{\link{meteo_pull_monitors}}).
#'
#' Great circle distance is used to determine whether a weather monitor is
#' within the required radius.
#'
#' @param lat_lon_df A dataframe that contains the latitude, longitude, and
#'    a unique identifier for each location (\code{id}). For an example of the
#'    proper format for this dataframe, see the examples below. Latitude and
#'    longitude must both be in units of decimal degrees. Southern latitudes
#'    and Western longitudes should be given as negative values.
#' @param lat_colname A character string giving the name of the latitude column
#'    in the \code{lat_lon_df} dataframe.
#' @param lon_colname A character string giving the name of the longitude column
#'    in the \code{lat_lon_df} dataframe.
#' @param station_data The output of \code{ghcnd_stations()[[1]]}, which is
#'    a current list of weather stations available through NOAA for the GHCND
#'    dataset. The format of this is a dataframe
#'    with one row per weather station. Latitude and longitude for the station
#'    locations should be in columns with the names "latitude" and "longitude",
#'    consistent with the output from \code{ghcnd_stations()[[1]]}. To save time, run the
#'    \code{ghcnd_stations} call and save the output to an object, rather than
#'    rerunning the default every time (see the examples in
#'    \code{\link{meteo_nearby_stations}}).
#' @param year_min A numeric value giving the earliest year from which you
#'    ultimately want weather data (e.g., 2013, if you only are interested in
#'    data from 2013 and later).
#' @param year_max A numeric value giving the latest year from which you
#'    ultimately want weather data.
#' @param radius A numeric vector giving the radius (in kilometers) within which
#'    to search for monitors near a location.
#' @param limit An integer giving the maximum number of monitors to include for
#'    each location. The [x] closest monitors will be kept. Default is NULL
#'    (pull everything available, within the radius if the radius is specified).
#' @inheritParams ghcnd_search
#'
#' @return A list containing dataframes with the sets of unique weather stations within
#'    the search radius for each location. Site IDs for the weather stations
#'    given in this dataframe can be used in conjunction with other functions in the
#'    \code{rnoaa} package to pull weather data for the station. The dataframe
#'    for each location includes:
#'    \itemize{
#'    \item \code{id}: The weather station ID, which can be used in other
#'    functions to pull weather data from the station;
#'    \item \code{name}: The weather station name;
#'    \item \code{latitude}: The station's latitude, in decimal degrees. Southern
#'    latitudes will be negative;
#'    \item \code{longitude}: The station's longitude, in decimal degrees. Western
#'    longitudes will be negative;
#'    \item \code{distance}: The station's distance, in kilometers, from the
#'    location.
#'    }
#'
#' @note By default, this function will pull the full station list from NOAA
#'    to use to identify nearby locations. If you will be creating lists of
#'    monitors nearby several stations, you can save some time by using the
#'    \code{\link{ghcnd_stations}} function separately to create an object
#'    with all stations and then use the argument \code{station_data} in
#'    this function to reference that object, rather than using this function's
#'    defaults (see examples).
#'
#' @seealso The weather monitor IDs generated by this function can be used in
#'    other functions in the \code{rnoaa} package, like
#'    \code{\link{meteo_pull_monitors}} and \code{\link{meteo_tidy_ghcnd}}, to
#'    pull weather data from weather monitors near a location.
#'
#' @author Alex Simmons \email{a2.simmons@@qut.edu.au},
#'    Brooke Anderson \email{brooke.anderson@@colostate.edu}
#'
#' @examples
#' \dontrun{
#'
#' station_data <- ghcnd_stations()[[1]] # Takes a while to run
#'
#' lat_lon_df <- data.frame(id = c("sydney", "brisbane"),
#'                          latitude = c(-33.8675, -27.4710),
#'                          longitude = c(151.2070, 153.0234))
#' nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df,
#'                     station_data = station_data, radius = 10)
#'
#' miami <- data.frame(id = "miami", latitude = 25.7617, longitude = -80.1918)
#'
#' # Get all stations within 50 kilometers
#' meteo_nearby_stations(lat_lon_df = miami, station_data = station_data,
#'                       radius = 50, var = c("PRCP", "TMAX"),
#'                       year_min = 1992, year_max = 1992)
#' # Get the closest 10 monitors
#' meteo_nearby_stations(lat_lon_df = miami, station_data = station_data,
#'                       limit = 10, var = c("PRCP", "TMAX"),
#'                       year_min = 1992, year_max = 1992)
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
meteo_nearby_stations <- function(lat_lon_df, lat_colname = "latitude",
                                  lon_colname = "longitude",
                                  station_data = ghcnd_stations()[[1]],
                                  var = "all", year_min = NULL,
                                  year_max = NULL, radius = NULL,
                                  limit = NULL){

  var <- tolower(var)

  # Handle generic values for `var`, `year_min`, and `year_max` arguments
  if(is.null(year_min)) year_min <- min(station_data$first_year, na.rm = TRUE)
  if(is.null(year_max)) year_max <- max(station_data$last_year, na.rm = TRUE)
  if(length(var) == 1 && var == "all"){
    var <- unique(station_data$element)
  }

  dots <- list(~last_year >= year_min & first_year <= year_max &
                 element %in% toupper(var) & !is.na(element))
  station_data <- dplyr::filter_(station_data, .dots = dots) %>%
    dplyr::select_(~id, ~name, ~latitude, ~longitude) %>%
    dplyr::distinct_()

  location_stations <- as.data.frame(lat_lon_df) %>%
    split(.[, lat_colname], .[, lon_colname]) %>%
    purrr::map(function(x) {
      station_ids <- meteo_distance(station_data = station_data,
                                    lat = x[ , lat_colname],
                                    long = x[ , lon_colname],
                                    radius = radius,
                                    limit = limit)
      return(station_ids)
    })
  names(location_stations) <- lat_lon_df$id
  return(location_stations)
}

#' Find all monitors within a radius of a location
#'
#' This function will identify all weather stations with a specified radius of
#' a location. If no radius is given, the function will return a dataframe
#' of all available monitors, sorted by distance to the location. The
#' \code{limit} argument can be used to limit the output dataframe to the [x]
#' closest monitors to the location.
#'
#' @param lat Latitude of the location. Southern latitudes should be given
#'    as negative values.
#' @param long Longitude of the location. Western longitudes should be given as
#'    negative values.
#' @param units Units of the latitude and longitude values. Possible values
#'    are:
#'    \itemize{
#'    \item \code{deg}: Degrees (default);
#'    \item \code{rad}: Radians.
#'    }
#' @inheritParams meteo_nearby_stations
#'
#' @return A dataframe of weather stations near the location. This is the
#'    single-location version of the return value for
#'    \code{\link{meteo_nearby_stations}}.
#'
#' @author Alex Simmons \email{a2.simmons@@qut.edu.au},
#'    Brooke Anderson \email{brooke.anderson@@colostate.edu}
#'
#' @export
meteo_distance <- function(station_data, lat, long,
                           units = 'deg', radius = NULL, limit = NULL) {

  data <- meteo_process_geographic_data(
    station_data = station_data,
    lat = lat,
    long = long
  )

  if(!is.null(radius)) {
    data <- data[data$distance < radius, ]
  }

  if(!is.null(limit)) {
    data <- data[1:min(limit, nrow(data)), ]
  }
  return(data)
}

#' Calculate the distances between a location and all available stations
#'
#' This function takes a single location and a dataset of available weather stations
#' and calculates the distance between the location and each of the stations,
#' using the great circle method. A new column is added to the dataset of
#' available weather stations giving the distance between each station and
#' the input location. The station dataset is then sorted from closest to
#' furthest distance to the location and returned as the function output.
#'
#' @inheritParams meteo_distance
#'
#' @return The \code{station_data} dataframe that is input, but with a
#'    \code{distance} column added that gives the distance to the location
#'    (in kilometers), and re-ordered by distance between each station and
#'    the location (closest weather stations first).
#'
#' @author Alex Simmons \email{a2.simmons@@qut.edu.au},
#'    Brooke Anderson \email{brooke.anderson@@colostate.edu}
#'
#' @export
meteo_process_geographic_data <- function(station_data,
                                          lat,
                                          long,
                                          units = 'deg') {

  # Convert headers to lowercase for consistency across code
  names(station_data) <- tolower(names(station_data))

  # Caluclate distance between points
  station_data$distance <- meteo_spherical_distance(lat1 = lat, long1 = long,
                                                    lat2 = station_data$latitude,
                                                    long2 = station_data$longitude,
                                                    units = "deg")

  # Sort data into ascending order by distance column
  station_data <- dplyr::arrange_(station_data, ~ distance)

  return(station_data)
} # End meteo_process_geographic_data

#' Calculate the distance between two locations
#'
#' This function uses the haversine formula to calculate the great circle
#' distance between two locations, identified by their latitudes and longitudes.
#'
#' @param lat1 Latitude of the first location.
#' @param long1 Longitude of the first location.
#' @param lat2 Latitude of the second location.
#' @param long2 Longitude of the second location.
#' @inheritParams meteo_distance
#'
#' @return A numeric value giving the distance (in kilometers) between the
#'    pair of locations.
#'
#' @note This function assumes an earth radius of 6,371 km.
#'
#' @author Alex Simmons \email{a2.simmons@@qut.edu.au},
#'    Brooke Anderson \email{brooke.anderson@@colostate.edu}
#'
#' @examples
#'
#' meteo_spherical_distance(lat1 = -27.4667, long1 = 153.0217,
#'                          lat2 = -27.4710, long2 = 153.0234)
#'
#' @export
meteo_spherical_distance <- function(lat1, long1, lat2, long2, units = 'deg') {

  radius_earth <- 6371

  # Convert angle values into radians
  if (units == 'deg') {
    lat1 <- deg2rad(lat1)
    long1 <- deg2rad(long1)
    lat2 <- deg2rad(lat2)
    long2 <- deg2rad(long2)
  } else if(units != 'rad'){
    stop("The `units` argument must be `deg` or `rad`.")
  }

  # Determine distance using the haversine formula, assuming a spherical earth
  a <- sin((lat2 - lat1) / 2) ^ 2 + cos(lat1) * cos(lat2) *
    sin((long2 - long1) / 2) ^ 2

  d <- 2 * atan2(sqrt(a), sqrt(1 - a)) * radius_earth
  return(d)

} # End calculate_spherical_distance

#' Convert from degrees to radians
#'
#' @param deg A numeric vector in units of degrees.
#'
#' @return The input numeric vector, converted to units of radians.
deg2rad <- function(deg) {
  return(deg*pi/180)
} # End deg2rad

