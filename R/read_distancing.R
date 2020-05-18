#' Read in Stay-at-Home Data
#'
#' Takes a folder of stay-at-home Safegraph data structured how it comes from AWS (i.e. folders 2020/04/03 for April 3 2020) and reads them in.
#'
#' @param start Date object with the starting date to read in stay-at-home data.
#' @param end Ending date to read stay-at-home data to.
#' @param dir The folder in which the "2020" (etc.) folder resides.
#' @export

read_distancing <- function(start,end,dir) {

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
    dt <- temp_unzip(target,data.table::fread,select=c('origin_census_block_group',
                                           'device_count',
                                           'completely_home_device_count',
                                           'part_time_work_behavior_devices',
                                           'full_time_work_behavior_devices'))

    # Convert CBG to string so we can easily extract state and county indicators
    dt[,origin_census_block_group := as.character(origin_census_block_group)]
    dt[,c('state','county') := fips_from_cbg(origin_census_block_group)]
    dt[,origin_census_block_group := NULL]

    # Collapse to the county level, summing up number of devices
    dt <- dt[,lapply(.SD, sum, na.rm=TRUE), by=.(state,county)]
    # Add the date column
    dt[,date := dates$date[r]]

    # Slap it all together
    if (r == 1) {
      compiled_data <- dt
    } else {
      compiled_data <- rbind(dt,compiled_data)
    }
  }

}
