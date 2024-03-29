% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{county_centers}
\alias{county_centers}
\title{County latitude and longitude designations.}
\format{
A dataframe with 3,143 rows and 5 variables:
\describe{
   \item{state}{A character vector giving the two-letter abbreviation for
                the state of each county}
   \item{fips}{A numeric vector giving the county's five-digit Federal
               Information Processing Standard (FIPS) code}
   \item{name}{A character vector giving the name and state for each county}
   \item{latitude}{A numeric vector giving the latitude of the geographic
                   center of each county}
   \item{longitude}{A numeric vector giving the longitude of the geographic
                    center of each county}
   \item{region}{A numeric vector giving the four-digit or five-digit Federal
                 Information Processing Standard (FIPS) code (values in this
                 column are identical to those in the "fips" column, but do
                 not include leading zeros)}
}
}
\source{
\url{https://www.census.gov/geo/maps-data/data/gazetteer2010.html}
}
\usage{
county_centers
}
\description{
A dataframe containing county-specific data: state, FIPS code, name,
latitude and longitude of geographic center, and region code. It includes
each U.S. county as of the 2010 census. This dataset was put together using a
dataframe from the U.S. Census Bureau, which was pulled from the website
listed in "Source." (Note: The names---in county, state format---for each
county were pulled from the 2010 U.S. Census file found here:
\url{http://www2.census.gov/geo/docs/reference/cenpop2010/county/CenPop2010_Mean_CO.txt}.)
}
\keyword{datasets}
