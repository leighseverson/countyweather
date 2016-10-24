% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/daily_helpers.R
\name{daily_stations}
\alias{daily_stations}
\title{NOAA NCDC station IDs per county.}
\usage{
daily_stations(fips, date_min = NULL, date_max = NULL)
}
\arguments{
\item{fips}{A string with the five-digit U.S. FIPS code of a county
in numeric, character, or factor format.}

\item{date_min}{A string with the desired starting date in character, ISO
format ("yyyy-mm-dd"). The dataframe returned will include only stations
that have data for dates including and after the specified date.}

\item{date_max}{A string with the desired ending date in character, ISO
format ("yyyy-mm-dd"). The dataframe returned will include only stations
that have data for dates up to and including the specified date.}
}
\description{
\code{daily_stations} returns a dataframe showing NOAA NCDC station IDs for
a single U.S. county. This function has options to filter stations based on
start and end date of available data, as well as percent of data coverage.
}
\note{
Because this function uses the NOAA API to identify the weather
   monitors within a U.S. county, you will need to get an access token from
   NOAA to use this function. Visit NOAA's token request page
   (\url{http://www.ncdc.noaa.gov/cdo-web/token}) to request a token by
   email. You then need to set that API code in your R session (e.g., using
   \code{options(noaakey = "your key")}, replacing "your key" with the API
   key you've requested from NOAA). See the package vignette for more details.
}
\examples{
\dontrun{
ex <- daily_stations("36005")
ex2 <- daily_stations("12086", date_min = "1999-01-01", date_max = "2012-12-31")
}

}
