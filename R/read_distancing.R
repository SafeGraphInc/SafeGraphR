#' Read in Stay-at-Home Data
#'
#' Takes a folder of stay-at-home Safegraph data structured how it comes from AWS (i.e. folders 2020/04/03 for April 3 2020) and reads them in.
#'
#' Note that after reading in data, if \code{gen_fips = TRUE}, state and county names can be merged in using \code{data(fips_to_names)}.
#'
#' @param start Date object with the starting date to read in stay-at-home data.
#' @param end Ending date to read stay-at-home data to.
#' @param dir The folder in which the "2020" (etc.) folder resides.
#' @param gen_fips Set to \code{TRUE} to use the \code{origin_census_block_group} variable to generate \code{state_fips} and \code{county_fips} as numeric variables. This will also result in \code{origin_census_block_group} being converted to character.
#' @param by After reading, collapse to this level by \code{sum}ming all the data. Usually \code{c('state_fips','county_fips')} with \code{gen_fips = TRUE}. Set to \code{NULL} to aggregate across all initial rows, or set to \code{FALSE} to not aggregate at all.
#' @param filter A character string describing a logical statement for filtering the data, for example \code{filter = 'state_fips == 6'} would give you only data from California. Will be used as an \code{i} argument in a \code{data.table}, see \code{help(data.table)}.  Filtering here instead of afterwards can cut down on time and memory demands.
#' @param select Character vector of variables to get from the file. Set to \code{NULL} to get all variables.
#' @param ... Other arguments to be passed to \code{data.table::fread} when reading in the file. For example, \code{nrows} to only read in a certain number of rows.
#' @examples
#'
#' \dontrun{
#'
#' # The directory distdat is the folder we have downloaded the distancing data to from AWS.
#' # Read and compile all distancing data from May 1 to May 7
#' distancing <- read_distancing(
#'     start = lubridate::ymd('2020-05-01'),
#'     end = lubridate::ymd('2020-05-07'),
#'     dir = distdat
#' )
#'
#' }
#' @export

read_distancing <- function(start,end,dir = '.',gen_fips = TRUE, by = c('state_fips','county_fips'), filter = NULL, select = c('origin_census_block_group',
                                                                                                             'device_count',
                                                                                                             'completely_home_device_count',
                                                                                                             'part_time_work_behavior_devices',
                                                                                                             'full_time_work_behavior_devices'), ...) {


  # Make sure defaults are desired
  if (getOption("distancing.warning", TRUE) &
      identical(select,c('origin_census_block_group',
                         'device_count',
                         'completely_home_device_count',
                         'part_time_work_behavior_devices',
                         'full_time_work_behavior_devices')) &
      identical(by, c('state_fips','county_fips'))) {
    message("Running read_distancing with default select and by - this will select only the device count variables, and aggregate to the county level. Change the select and by options if you don't want this. This message will be displayed only once per session.")
    options("distancing.warning" = FALSE)
  }

  # Make sure dir ends with /
  if (dir == '') {
    dir <- '.'
  }
  if (stringr::str_sub(dir,-1) != '/') {
    dir <- paste0(dir, '/')
  }

  # List of dates that I want
  dates <- start + lubridate::days(0:(end - start))

  # Read in dates one at a time, keep memory low if possible!
  for (r in dates) {

    # Where's the prize
    datechar <- as.character(lubridate::as_date(r))
    target <- paste0(dir,stringr::str_sub(datechar,1,4),
                     '/',stringr::str_sub(datechar,6,7),
                     '/',stringr::str_sub(datechar,9,10))
    target <- paste0(target,'/',list.files(target))

    print(target)

    # Read in only these columns
    if (is.null(select)) {
      dt <- data.table::fread(file = target,...)
    } else {
      dt <- data.table::fread(file = target,select = select,...)
    }

    # Convert CBG to string so we can easily extract state and county indicators
    if (gen_fips) {
      dt[,origin_census_block_group := as.character(origin_census_block_group)]
      dt[,c('state_fips','county_fips') := fips_from_cbg(origin_census_block_group)]
    }

    # Do filter after gen_fips so you can filter on fips
    if (!is.null(filter)) {
      dt <- dt[eval(parse(text=filter))]
    }

    # Collapse
    if (!is.logical(by)) {
      # Can keep only summable or by-variables
      dt <- subset(dt,select = (sapply(dt,is.numeric) |
        names(dt) %in% by))
      dt <- dt[,lapply(.SD, sum, na.rm=TRUE), by=by]
    }

    # Add the date column
    dt[,date := lubridate::as_date(r)]

    # Slap it all together
    if (r == dates[1]) {
      compiled_data <- dt
    } else {
      compiled_data <- rbind(dt,compiled_data)
    }
  }

  return(compiled_data)
}
