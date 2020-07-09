#' Adjust SafeGraph Data for Sampling Size Differences
#'
#' This function uses 2016 American Community Survey data to adjust SafeGraph counts for the portion of the population that is sampled. This function will return a \code{data.table} with columns for a geographic ID and the variable \code{adjust_factor}, which you can merge into your data and then multiply whatever count variables you like by \code{adjust_factor} to adjust them for sampling differences.
#'
#' @param data A \code{data.frame} (or \code{tibble} or \code{data.table}) containing (among other things potentially) geographic ID variables and a variable for the number of SafeGraph devices observed in that area. Often this is from a \code{home-panel-summary} file.
#' @param from_id A character vector either giving the variable name of the census block group ID, or both the state FIPS and county FIPS variables (which must be numeric, and in state, then county order). Census block group must be specified if \code{from_level='cbg'}.
#' @param sample_id A character variable giving the variable name of the variable in \code{data} that has the number of SafeGraph observations.
#' @param from_level Either \code{'cbg'} or \code{'county'}, indicating the geographic level that is to be adjusted.
#' @param to_level Either \code{'county'} or \code{'state'}, indicating the geographic level that the \code{from_level} components are to be adjusted to, for example \code{from_level='county'} and \code{to_level='state'} wouuld give an adjustment factor for each county as though each county in the state was sampled at the same rate.
#' @param by The data returned will be on the \code{from_level} level. Specify other vairables here to have it instead be on the \code{from_level}-\code{by} level, perhaps a timecode. \code{by} should not split the \code{from_level} counts. If, for example, \code{by} is used to split a county in two geographic subcounties, then the population adjustment will not be correct.
#' @param pop_data If a populatinon data file other than \code{data(cbg_pop)} or \code{data(county_pop)} should be used, enter it here. Should be in the same format, and with the same variable names, as \code{cbg_pop} if \code{from_level='cbg'}, or the same as \code{county_pop} if \code{from_level='county'}.
#' @examples
#' \dontrun{
#' # The current working directory has many home_panel_summary files
#' # Do some futzing with the census_block_group variable to
#' # Get it in the same format as how it is in cbg_pop
#' home_panel <- read_many_csvs(colClasses= c(census_block_group='character'))
#' home_panel[,census_block_group := as.character(as.numeric(census_block_group))]
#'
#' # Create the data set with the adjust_factor variable
#' # This will adjust CBG populations to county ones, by default
#' adj_factor <- sample_size_adjust(home_panel, by = 'date_range_start')
#'
#' # Now take some distancing data I have
#' # (where census_block_group is stored as origin_census_block_group)
#' data.table::setnames(adj_factor, census_block_group, origin_census_block_group)
#' # and merge in the adjustment factor
#' distancing <- merge(distancing, adj_factor, all.x = TRUE, by = 'origin_census_block_group')
#' # And use that adjustment factor to adjust!
#' distancing[,adj_device_count := device_count*adj_factor]
#'
#' }
#' @export

sample_size_adjust <- function(data,from_id = 'census_block_group',
                               sample_id = 'number_devices_residing',
                               from_level = 'cbg',
                               to_level = 'county',
                               by = NULL,
                               pop_data = NULL) {

  if (!(from_level %in% c('cbg','county'))) {
    stop('from_level must be cbg or county.')
  }
  if (!(to_level %in% c('county','state'))) {
    stop('to_level must be county or state.')
  }
  if (length(from_id) > 1 & from_level == 'cbg') {
    stop('Only specify the census block group variable for from_id if from_level is cbg.')
  }

  if (from_level == 'cbg' & is.null(pop_data)) {
    data("cbg_pop", package = 'SafeGraphR')
    pop_data <- cbg_pop
  } else if (from_level == 'county' & is.null(pop_data)) {
    data("county_pop", package = 'SafeGraphR')
    pop_data <- county_pop
  }

  # See what proportion the from group is of the to group population
  if (to_level == 'county') {
    pop_data[,big_pop := sum(unweighted_pop, na.rm = TRUE),by=c('state_fips','county_fips')]
  } else if (to_level == 'state') {
    pop_data[,big_pop := sum(unweighted_pop, na.rm = TRUE),by='state_fips']
  }

  pop_data[,pop_prop := unweighted_pop/big_pop]

  # if we have CBG, get state and county FIPS
  data <- data.table::as.data.table(data)
  data <- subset(data, select = c(sample_id, from_id, by))

  # For ease of use

  data.table::setnames(data,sample_id,'sample_pop')

  # Create county and state FIPS if we don't have them
  if (length(from_id) == 1) {
    data[,c('state_fips','county_fips') := fips_from_cbg(eval(parse(text=from_id)))]

    # If we're from-county, collapse
    if (from_level == 'county') {
      data <- data[, .(sample_pop = sum(sample_pop)), by = c('state_fips','county_fips',
                                                             by)]
    }

    # Line up names for cbg
    data.table::setnames(pop_data, 'poi_cbg', from_id)

  } else {
    # rename the population data state and county IDs to match the data
    data.table::setnames(pop_data,c('state_fips','county_fips'),from_id)
  }

  # Get the to-level of the sample
  data <- data[,.(top_sample = sum(sample_pop),
                  sample_pop = sample_pop),by=c(from_id,by)]

  # Merge together
  data <- merge(data,pop_data, all.x = TRUE, by = from_id)

  # And create adjust_factor
  data[,adjust_factor := (unweighted_pop/big_pop)/(sample_pop/top_sample)]

  return(unique(subset(data,select=c(from_id,by,'adjust_factor'))))
}

