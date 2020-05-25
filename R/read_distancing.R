#' Read in Stay-at-Home Data
#'
#' Takes a folder of stay-at-home Safegraph data structured how it comes from AWS (i.e. folders 2020/04/03 for April 3 2020) and reads them in.
#'
#' @param start Date object with the starting date to read in stay-at-home data.
#' @param end Ending date to read stay-at-home data to.
#' @param dir The folder in which the "2020" (etc.) folder resides.
#' @param gen_fips Set to \code{TRUE} to use the \code{origin_census_block_group} variable to generate \code{state_fips} and \code{county_fips} variables. This will also result in \code{origin_census_block_group} being converted to character.
#' @param by After reading, collapse to this level by \code{sum}ming all the data. Usually \code{c('state_fips','county_fips')} with \code{gen_fips = TRUE}. Set to \code{NULL} to aggregate across all initial rows, or set to \code{FALSE} to not aggregate at all.
#' @param select Character vector of variables to get from the file. Set to \code{NULL} to get all variables.
#' @param ... Other arguments to be passed to \code{data.table::fread} when reading in the file. For example, \code{nrows} to only read in a certain number of rows.
#' @export

read_distancing <- function(start,end,dir = '.',gen_fips = TRUE, by = c('state_fips','county_fips'), select = c('origin_census_block_group',
                                                                                                             'device_count',
                                                                                                             'completely_home_device_count',
                                                                                                             'part_time_work_behavior_devices',
                                                                                                             'full_time_work_behavior_devices'), ...) {

  # List of dates that I want
  dates <- dplyr::tibble(date = start + lubridate::days(0:(end - start))) %>%
    mutate(year = lubridate::year(date),
           # Preserve leading 0s
           month = stringr::str_sub(as.character(date),6,7),
           day = stringr::str_sub(as.character(date),9,10))

  # Read in dates one at a time, keep memory low if possible!
  for (r in 1:nrow(dates)) {

    # Where's the prize
    target <- paste0(dir,dates$year[r],'/',dates$month[r],'/',dates$day[r])
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
      dt[,c('state','county') := fips_from_cbg(origin_census_block_group)]
    }

    # Collapse
    if (!is.logical(by)) {
      # Can keep only summable or by-variables
      dt <- subset(dt,select = (sapply(dt,is.numeric) |
        names(dt) %in% by))
      dt <- dt[,lapply(.SD, sum, na.rm=TRUE), by=by]
    }

    # Add the date column
    dt[,date := dates$date[r]]

    # Slap it all together
    if (r == 1) {
      compiled_data <- dt
    } else {
      compiled_data <- rbind(dt,compiled_data)
    }
  }

  return(compiled_data)
}
