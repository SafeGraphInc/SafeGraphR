#' Unweighted Population by Census Block Group
#'
#' A dataset containing the unweighted population by Census Block Group from the Open Census file. Use with \code{fips_from_cbg} to get state and county FIPS codes from the CBG ID.
#'
#' @format A \code{data.table} with 220333 rows and 2 variables:
#' \describe{
#'   \item{poi_cbg}{Census Block Group ID, named for easy merging with the patterns data.}
#'   \item{unweighted_pop}{Unweighted population from the 2016 American Community Survey}
#' }
#' @source \url{https://docs.safegraph.com/docs/open-census-data}
"cbg_pop"

#' Unweighted Population by County
#'
#' A dataset containing the unweighted population by county from the Open Census file. Merge with \code{data(fips_to_names)} to name the states and counties.
#'
#' @format A \code{data.table} with 3220 rows and 3 variables:
#' \describe{
#'   \item{unweighted_pop}{Unweighted population from the 2016 American Community Survey}
#'   \item{state_fips}{State FIPS code for the census block group}
#'   \item{county_fips}{County FIPS code for the census block group}
#' }
#' @source \url{https://docs.safegraph.com/docs/open-census-data}
"county_pop"

#' State and county names by FIPS codes
#'
#' A dataset that linkes state and county FIPS codes (as numeric values) to the names of those states and counties
#'
#' @format A \code{data.table} with 3142 rows and 3 variables:
#' \describe{
#'   \item{state_fips}{State FIPS code}
#'   \item{county_fips}{County FIPS code}
#'   \item{statename}{The full name of the state}
#'   \item{countyname}{The full name of the county, including "County"}
#' }
#' @source \url{US Census}
"fips_to_names"

#' NAICS Code Titles
#'
#' A dataset that linkes NAICS codes to their descriptive titles.
#'
#' @format A \code{data.table} with 1069 rows and 2 variables:
#' \describe{
#'   \item{naics_code}{The NAICS code}
#'   \item{naics_title}{The title of the NAICS code}
#' }
#' @source \url{US Census NAICS page}
"naics_codes"
