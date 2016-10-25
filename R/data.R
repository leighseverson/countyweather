#' County latitude and longitude designations.
#'
#' A dataframe containing state, FIPS code, name, geographic latitude and
#' longitude, and region code for each U.S. county as of the 2010 census. This
#' dataset was put together using a dataframe from the U.S. Census Bureau, which
#' was pulled from the website listed in "Source." (Note: the names (in county,
#' state format) for each county were pulled from the 2010 U.S. Census file found here:
#' \url{http://www2.census.gov/geo/docs/reference/cenpop2010/county/CenPop2010_Mean_CO.txt}.)
#'
#' @format A dataframe with 3,143 rows and 5 variables:
#' \describe{
#'    \item{state}{A character vector giving the two-letter abbreviation for
#'                 the state of each county}
#'    \item{fips}{A numeric vector giving the county's five-digit Federal
#'                Information Processing Standard (FIPS) code}
#'    \item{name}{A character vector giving the name and state for each county}
#'    \item{latitude}{A numeric vector giving the latitude at the geographic
#'                    center of each county}
#'    \item{longitude}{A numeric vector giving the longitude at the geographic
#'                     center of each county}
#'    \item{region}{A numeric vector giving the four-digit or five-digit Federal
#'                  Information Processing Standard (FIPS) code (values in this
#'                  column are identical to those in the "fips" column, but do
#'                  not include leading zeros)}
#' }
#'
#' @source
#'
#' \url{https://www.census.gov/geo/maps-data/data/gazetteer2010.html}
"county_centers"

#' County land area data.
#'
#' A dataframe containing the FIPS code and an estimate for radius in km for
#' each U.S. county. This dataset was put together using a dataset from the U.S.
#' Census American FactFinder data dissemination tool. This dataset was downloaded
#' from the geographic identifiers 'G001' option for the 2010 Summary File1
#' (SF1). The file was found by following the instructions recommended by the
#' U.S. census here: \url{https://ask.census.gov/faq.php?id=5000&faqId=7825}. The
#' website listed in "Source" gives more information about this dataset.
#'
#' @format A dataframe with 3,143 rows and 2 variables:
#' \describe{
#'
#'    \item{fips}{A character vector giving the county's five-digit Federal
#'                Information Processing Standard (FIPS) code}
#'
#'    \item{county_radius}{A numeric vector giving an estimate for each
#'                         county's radius from its center, in km. This value
#'                         was calculated by dividing the U.S. Census Land Area
#'                         estimates (which are in square meters) by 1,000,000,
#'                         and then taking the square root of each area and
#'                         dividing by pi. Each county was estimated to be
#'                         roughly circular for the purposes of estimating
#'                         county radii.}
#' }
#'
#' @source
#'
#' \url{http://www.census.gov/prod/cen2010/doc/sf1.pdf}
"county_radius"
