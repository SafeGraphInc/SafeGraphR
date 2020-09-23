#' Unweighted Population by Census Block Group
#'
#' A dataset containing the unweighted population by Census Block Group from the Open Census file. Use with \code{fips_from_cbg} to get state and county FIPS codes from the CBG ID.
#'
#' @format A \code{data.table} with 220333 rows and 2 variables:
#' \describe{
#'   \item{poi_cbg}{Census Block Group ID, named for easy merging with the patterns data.}
#'   \item{unweighted_pop}{Population from the 2016 American Community Survey (the "unweighted" part is outdated but kept for consistency with old code).}
#' }
#' @source \url{https://docs.safegraph.com/docs/open-census-data}
"cbg_pop"

#' Unweighted Population by County
#'
#' A dataset containing the unweighted population by county from the Open Census file. Merge with \code{data(fips_to_names)} to name the states and counties.
#'
#' @format A \code{data.table} with 3220 rows and 3 variables:
#' \describe{
#'   \item{unweighted_pop}{Population from the 2016 American Community Survey (the "unweighted" part is outdated but kept for consistency with old code).}
#'   \item{state_fips}{State FIPS code for the census block group}
#'   \item{county_fips}{County FIPS code for the census block group}
#' }
#' @source \url{https://docs.safegraph.com/docs/open-census-data}
"county_pop"

#' State and county names by FIPS codes
#'
#' A dataset that linkes state and county FIPS codes (as numeric values) to the names of those states and counties
#'
#' @format A \code{data.table} with 3142 rows and 4 variables:
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

#' Example Normalization Data
#'
#' The normalization file from the July 1 weekly patterns pull, for the vignette. See [SafeGraph Docs](https://docs.safegraph.com/docs) for full documentation.
#'
#' @format A \code{data.table} with 7 rows and 7 variables:
#' \describe{
#'   \item{date}{The date}
#'   \item{total_visits}{The total number of visits recorded in SafeGraph on that day}
#'   \item{total_devices_seen}{The total number of individual devices recorded in SafeGraph on that day}
#'   \item{total_home_visits}{Total devices with at least one visit to the home location that day}
#' }
#' @source SafeGraph
"norm"

#' Example Patterns Data Aggregated by NAICS Code
#'
#' The patterns file from the July 1 weekly patterns pull, aggregated to the NAICS code level for the vignette. See [SafeGraph Docs](https://docs.safegraph.com/docs) for full documentation.
#'
#' @format A \code{data.table} with 9247 rows and 7 variables:
#' \describe{
#'   \item{date}{The date}
#'   \item{start_date,day}{The first date present in the patterns file, and whether this observation is from that first date (1), the second (2), etc.}
#'   \item{state_fips,county_fips}{Originally would have been the state and county identifiers, but since things were aggregated to the NAICS level (rather than NAICS/state/county), they have been summed up and now mean nothing.}
#'   \item{naics_code}{Six-digit NAICS code}
#'   \item{visits_by_day}{The total number of visits to POIs of this NAICS code on this day}
#' }
#' @source SafeGraph
"pat_naics"

#' Example Panel Information Data
#'
#' The home_panel_summary file from the July 1 weekly patterns pull, processed. See [SafeGraph Docs](https://docs.safegraph.com/docs) for full documentation.
#'
#' @format A \code{data.table} with 1155 rows and 4 variables:
#' \describe{
#'   \item{start_date}{The first date present in the patterns file}
#'   \item{state_fips,county_fips}{State and county identifiers}
#'   \item{number_devices_residing}{The total number of devices with home locations in those counties}
#' }
#' @source SafeGraph
"panel"


#' Example New York and New Jersey Patterns Data Aggregated by County
#'
#' The patterns file from the July 1 weekly patterns pull, aggregated to the county level for the vignette, for New York and New Jersey only. See [SafeGraph Docs](https://docs.safegraph.com/docs) for full documentation.
#'
#' @format A \code{data.table} with 2324 rows and 7 variables:
#' \describe{
#'   \item{date}{The date}
#'   \item{start_date,day}{The first date present in the patterns file, and whether this observation is from that first date (1), the second (2), etc.}
#'   \item{state_fips,county_fips}{The state and county identifiers}
#'   \item{naics_code}{Originally this was the six-digit NAICS code of the associated POI. But since aggregation didn't preserve NAICS, this is nonsense}
#'   \item{visits_by_day}{The total number of visits to POIs in this county on this day}
#' }
#' @source SafeGraph
"pat_naics"

#' Example Stay-at-Home Data
#'
#' Distancing data from June 1 to June 14, aggregated to the county level for the vignette. See [SafeGraph Docs](https://docs.safegraph.com/docs) for full documentation.
#'
#' @format A \code{data.table} with 45158 rows and 7 variables:
#' \describe{
#'   \item{date}{The date}
#'   \item{state_fips,county_fips}{The state and county identifiers}
#'   \item{device_count}{The total number of devices observed}
#'   \item{completely_home_device_count}{The total number of devices observed that did not leave their home location on this day}
#'   \item{part_time_work_behavior_devices,full_time_work_behavior_devices}{The total number of devices observed that appear to be engaging in part-time or full-time work behavior (experimental)}
#' }
#' @source SafeGraph
"distancing"
