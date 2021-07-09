#' Unweighted Population by Census Block Group
#'
#' A dataset containing the unweighted population by Census Block Group from the Open Census file (US Only). Use with \code{fips_from_cbg} to get state and county FIPS codes from the CBG ID.
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
#' A dataset containing the unweighted population by county from the Open Census file (US only). See \code{canada_cd_pop} for Canadian county population. Merge with \code{data(fips_to_names)} to name the states and counties.
#'
#' @format A \code{data.table} with 3220 rows and 3 variables:
#' \describe{
#'   \item{unweighted_pop}{Population from the 2016 American Community Survey (the "unweighted" part is outdated but kept for consistency with old code).}
#'   \item{state_fips}{State FIPS code for the county}
#'   \item{county_fips}{County FIPS code for the county}
#' }
#' @source \url{https://docs.safegraph.com/docs/open-census-data}
"county_pop"

#' Canadian Census District Populations
#'
#' Population by census district (with \code{state_fips} and \code{county_fips} identifiers to link with other data sets in the package - sorry for the naming, Canadians). The "unweighted" in the variable name \code{unweighted_pop} doesn't refer to anything specific in the Canadian census, but is so you can easily \code{rbind} this with \code{county_pop}.
#'
#' This comes from the Canadian census directly instead of SafeGraph.
#'
#' @format A \code{data.table} with 293 rows and 3 variables:
#' \describe{
#'   \item{unweighted_pop}{Population from the 2016 Canadian census.}
#'   \item{state_fips}{Province SGC for the county}
#'   \item{county_fips}{Census division code}
#' }
#' @source \url{https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/pd-pl/Tables/CompFile.cfm?Lang=Eng&T=701&OFT=FULLCSV}
"canada_cd_pop"

#' State and county names by FIPS codes
#'
#' A dataset that links state and county FIPS codes in the US (as character values) and province and census division codes (Canada) to the names of those states/provinces and counties/census divisions. This data predates the inclusion of Canada in SafeGraph, thus the US-centric naming.
#'
#' @format A \code{data.table} with 3142 rows and 4 variables:
#' \describe{
#'   \item{state_fips}{State FIPS code / Canadian SGC code}
#'   \item{county_fips}{County FIPS code / Canadian Census division}
#'   \item{statename}{The full name of the state / province}
#'   \item{countyname}{The full English name of the county / census division, including "County" for US entries. Merge with \code{canada_cd_types} to get the equivalent division type for Canada and French names.}
#'   \item{iso_country_code}{Indicator for US or Canada}
#' }
#' @source \url{US Census}
"fips_to_names"

#' Additional census division information for Canada
#'
#' A dataset that can be merged with \code{fips_to_names} with information on French names for locations as well as the type of census division it is.
#'
#' \describe{
#'   \item{state_fips}{Canadian SGC code}
#'   \item{county_fips}{Canadian Census division}
#'   \item{countyname_french}{Census division name in French}
#'   \item{cd_type}{Census district type in English}
#'   \item{cd_type_french}{Census district type in French}
#' }
"canada_cd_types"

#' State Information
#'
#' A dataset that links state (and Washington DC) names, FIPs codes, two-letter abbreviations (called "region" because this is what it is called in SafeGraph files that use it), and Census regions. Can be merged with \code{fips_to_names} using \code{state_fips} and \code{statename}.
#'
#' This also includes Canadian data on provinces.
#'
#' Note that this is a data set purely of Canadian provinces, US *states*, and DC. Some SafeGraph files contain information on \code{region} values of \code{GU} (Guam), \code{PR} (Puerto Rico), etc., but those will be lost if merging with \code{state_info}.
#'
#' @format A \code{data.table} with 51 rows and 4 variables:
#' \describe{
#'   \item{statename}{The full name of the state / province}
#'   \item{CensusRegion}{The broad Census regions}
#'   \item{region}{The state's two-digit abbreviation / the province's international alpha code}
#'   \item{state_fips}{State FIPS code / Canadian SGC code}
#'   \item{iso_country_code}{Indicator for US or Canada}
#' }
#' @source \url{US Census}
"state_info"

#' NAICS Code Titles
#'
#' A dataset that links six-digit NAICS codes to their descriptive titles using 2017 NAICS codes.
#'
#' @format A \code{data.table} with 1069 rows and 2 variables:
#' \describe{
#'   \item{naics_code}{The NAICS code}
#'   \item{naics_title}{The title of the NAICS code}
#' }
#' @source \url{US Census NAICS page}
"naics_codes"

#' NAICS 4-Digit Code Titles
#'
#' A dataset that links four-digit NAICS codes to their descriptive titles using 2017 NAICS codes.
#'
#' @format A \code{data.table} with 311 rows and 2 variables:
#' \describe{
#'   \item{naics_code}{The NAICS code}
#'   \item{naics_title}{The title of the NAICS code}
#' }
#' @source \url{US Census NAICS page}
"naics_4"


#' NAICS 4-Digit Code Titles
#'
#' A dataset that links two-digit NAICS codes to their descriptive titles using 2017 NAICS codes.
#'
#' Notice that some \code{naics_title} values are repeated because they cross several two-digit codes.
#'
#' @format A \code{data.table} with 24 rows and 2 variables:
#' \describe{
#'   \item{naics_code}{The NAICS code}
#'   \item{naics_title}{The title of the NAICS code}
#' }
#' @source \url{US Census NAICS page}
"naics_2"

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
