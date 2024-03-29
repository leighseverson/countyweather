% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hourly_helpers.R
\name{filter_hourly}
\alias{filter_hourly}
\title{Filter NOAA ISD stations based on "coverage" requirements, and calculate
coverage and statistical information for each station-variable combination.}
\usage{
filter_hourly(fips, hourly_data, coverage = NULL)
}
\arguments{
\item{fips}{A character string giving the five-digit U.S. FIPS
county code of the county for which the user wants to pull weather data.}

\item{hourly_data}{A dataframe as returned by the \code{df} element from an
\code{isd_monitors_data} call.}

\item{coverage}{A numeric value in the range of 0 to 1 that specifies the
desired percentage coverage for each weather variable (i.e., what percent
of each weather variable must be non-missing to include the data from a
station when calculating hourly values averaged across stations).}
}
\value{
A list with two elements: \code{df} and \code{stations}. \code{df} is
   a dataframe of hourly weather data filtered based on the specfified
   coverage, as well as columns (\code{"var"_reporting}) for each weather
   variable showing the number of stations contributing to the average for that
   variable for each hour. The second element, \code{stations}, is a dataframe
   giving statistical information for stations that meet the specified coverage
   requirements. The column \code{station} gives the station id (USAF and
   WBAN identification numbers pasted together, separated by "-"). Note: One
   of these identification ids is sometimes missing. For example, a value in
   \code{station} might be \code{722029-NA}. The column \code{var}
   gives the weather variable associated with the row of statistical values
   for each station and variable combination. \code{calc_coverage} gives the
   percentage coverage for each station-weather variable combination. These
   values will all be greater than or equal to the specified \code{coverage}
   value. \code{standard_dev} gives the standard deviation of values for each
   station-weather variable combination. \code{max} and \code{min} give the
   minimum and maximum values, and \code{range} gives the range of values in
   each station-weather variable combination. These last four statistical
   calculations (\code{standard_dev}, \code{max}, \code{min}, and
   \code{range}) are only included for the seven core hourly weather variables,
   which include \code{"wind_direction"}, \code{"wind_speed"},
   \code{"ceiling_height"}, \code{"visibility_distance"}, \code{"temperature"},
   \code{"temperature_dewpoint"}, and \code{"air_pressure"}. (The values of
   these columns are set to \code{NA} for other variables, such as quality
   flag data.)
}
\description{
Filters available weather stations based on a specified
minimum coverage (i.e., percent non-missing hourly observations). Weather
stations with non-missing data for fewer days than specified by
\code{coverage} will be excluded from the county average.
}
