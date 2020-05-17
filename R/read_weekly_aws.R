#' Read Weekly Patterns as Downloaded from AWS
#'
#' This function takes a single \code{.csv.gz} patterns file as provided for weekly data via AWS and reads in the \code{visits_by_day} information. The result is two data sets, both by-state-by-day, one also by-brand, and one by-NAICS.
#'
#' @param filename The filename of the \code{.csv.gz} file.
#' @param dir The directory in which the file sits.
#' @export

read_weekly_aws <- function(filename,dir) {
  print(filename)

  f <- paste0(dir,filename)

  patterns <- temp_unzip(f,data.table::fread,select = c('brands',
                                            'naics_code',
                                            'region',
                                            'visits_by_day',
                                            'raw_visit_counts'))



  # Total visits
  total_visits <- expand_integer_json(patterns,
                                      expand = 'visits_by_day',
                                      index = 'day',
                                      by = 'region')
  total_visits <- dplyr::as_tibble(total_visits)


  # Get daily visits by subcategory
  # Do naics_code
  # sub_category
  # and brands

  # naics_code first

  # For speed!
  data.table::setkey(patterns,'naics_code','region')

  daily_visits <- expand_integer_json(patterns,
                                      expand = 'visits_by_day',
                                      index = 'day',
                                      by = c('region','naics_code'),
                                      set_key = FALSE)


  # Flatten patterns down
  p_naics_code <- patterns[,.(raw_visit_counts=sum(raw_visit_counts,na.rm=TRUE)),
                           by=.(naics_code,region)]

  p_naics_code <- p_naics_code %>%
    dplyr::as_tibble() %>%
    dplyr::left_join(daily_visits %>% dplyr::as_tibble) %>%
    dplyr::mutate(year = 2020,
           month = as.numeric(stringr::str_sub(filename,6,7))) %>%
    dplyr::left_join(total_visits) %>%
    dplyr::filter(!is.na(day), !is.na(naics_code)) %>%
    dplyr::mutate(date = lubridate::ymd(paste(year,month,as.numeric(str_sub(filename,9,10)),sep='-'))+lubridate::days(day-1))

  # then brands
  data.table::setkey(patterns,'brands','region')

  daily_visits <- expand_integer_json(patterns,
                                      expand = 'visits_by_day',
                                      index = 'day',
                                      by = c('region','brands'),
                                      set_key = FALSE)

  # Flatten patterns down
  p_brands <- patterns[brands != '',
                       .(raw_visit_counts=sum(raw_visit_counts,na.rm=TRUE)),
                       by=.(brands,region)]

  p_brands <- p_brands %>%
    dplyr::as_tibble() %>%
    dplyr::left_join(daily_visits %>% dplyr::as_tibble()) %>%
    dplyr::mutate(year = 2020,
           month = as.numeric(stringr::str_sub(filename,6,7))) %>%
    dplyr::left_join(total_visits) %>%
    dplyr::filter(!is.na(day),!is.na(brands)) %>%
    dplyr::mutate(date = lubridate::ymd(paste(year,month,as.numeric(stringr::str_sub(filename,9,10)),sep='-'))+lubridate::days(day-1))


  # And return what we've made
  return(list(p_naics_code,p_brands))
}
