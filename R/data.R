#' County latitude and longitude designations
#'
#' A dataframe containing latitude, longitude, FIPS code, name, and region code
#' for each US county. This dataset was put together using a dataframe from the US
#' Census Bureau, which was pulled from the website listed in "Sources."
#'
#' @format A dataframe with 3,143 rows and 5 variables:
#' \describe{
#'    \item{latitude}{A numeric vector giving the latutude at the center of each
#'                    county}
#'    \item{longitude}{A numeric vector giving the longitude at the center of
#'                     each county}
#'    \item{fips}{A numeric vector giving the county's five-digit Federal
#'                Information Processing Standard (FIPS) code}
#'    \item{name}{A character vector giving the name and state for each county}
#'    \item{region}{A numeric vector giving the four-digit or five-digit Federal
#'                  Information Processing Standard (FIPS) code (values in this
#'                  column are identical to those in the "fips" column, but do
#'                  not include leading zeros)}
#' }
#'
#' @source
#'
#' \url{http://www2.census.gov/geo/docs/reference/cenpop2010/county/CenPop2010_Mean_CO.txt}
"county_centers"

#' County land area data
#'
#' A dataframe containing FIPS code and an estimate for county radius in km for
#' each US county. This dataset was put together using a dataset from the US
#' Census American FactFinder data dissemination tool. This dataset was downloaded
#' from the geographic identifiers 'G001' option for the 2010 Summary File1
#' (SF1). The file was found by following the instructions recommended by the
#' US census here: \url{https://ask.census.gov/faq.php?id=5000&faqId=7825}. The
#' website listed in "Sources" gives more information about this dataset.
#'
#' @format A dataframe with 3,143 rows and 2 variables:
#' \describe{
#'    \item{fips}{A character vector giving the county's five-digit Federal
#'                Information Processing Standard (FIPS) code}
#'         {county_radius}{A numeric vector giving an estimate for each
#'                         county's radius from its center, in km. This value
#'                         was calculated by dividing the US Census Land Area
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
