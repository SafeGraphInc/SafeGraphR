#' Read Monthly Patterns from the SafeGraph Shop
#'
#' This function takes a monthly patterns file as downloaded from the SafeGraph shop and returns \code{visits_by_day} on a daily level in three formats: by NAICS code by state, by brand by state, and by \code{sub_category} by state. Plus, a normalization file.
#'
#' @param filename The filename of the ZIP file from the SafeGraph shop which contains patterns, normalization, etc.
#' @param dir The directory in which the ZIP file sits.
#' @export

read_monthly_shop <- function(filename,dir='./') {
  print(filename)

  # Open up the zip file
  unzip(paste0(dir,filename))

  # Some of the files now are not .gz
  flist <- list.files(pattern="\\.gz$")

  dat1 <- list()
  dat2 <- list()
  dat3 <- list()

  for (f in flist) {
    print(paste(f,Sys.time()))

    patterns <- temp_unzip(f,data.table::fread,select = c('brands',
                                              'naics_code',
                                              'top_category',
                                              'sub_category',
                                              'region',
                                              'visits_by_day',
                                              'raw_visit_counts'))
    file.remove(f)


    norm <- data.table::fread('normalization_stats.csv')

    # Total visits
    total_visits <- patterns[visits_by_day != '',
                             .(total_visits=Reduce("+",
                                                   map(
                                                     str_split(
                                                       str_sub(
                                                         visits_by_day,2,
                                                         nchar(visits_by_day)-1),
                                                       ','),
                                                     as.numeric))),
                             by=.(region)]
    total_visits[,day := seq_len(.N),by=region]
    total_visits <- dplyr::as_tibble(total_visits)


    # Get daily visits by subcategory
    # Do naics_code
    # sub_category
    # and brands

    # naics_code first

    # For speed!
    data.table::setkey(patterns,'naics_code','region')

    daily_visits <- patterns[visits_by_day != '',
                             .(visits_by_day=Reduce("+",
                                                    purrr::map(
                                                      str_split(
                                                        str_sub(
                                                          visits_by_day,2,
                                                          nchar(visits_by_day)-1),
                                                        ','),
                                                      as.numeric))),
                             by=.(naics_code,region)]
    daily_visits[,day := seq_len(.N),by=.(naics_code,region)]

    # Flatten patterns down
    p_naics_code <- patterns[,.(raw_visit_counts=sum(raw_visit_counts,na.rm=TRUE)),
                             by=.(naics_code,region)]

    p_naics_code <- p_naics_code %>%
      dplyr::as_tibble() %>%
      dplyr::left_join(daily_visits %>% dplyr::as_tibble) %>%
      mutate(year = as.numeric(stringr::str_sub(filename,42,45)),
             month = as.numeric(stringr::str_sub(filename,47,48))) %>%
      dplyr::left_join(total_visits) %>%
      dplyr::filter(!is.na(day)) %>%
      dplyr::mutate(date = lubridate::ymd(paste(year,month,day,sep='-')))

    dat1[[f]] <- p_naics_code

    # then brands
    data.table::setkey(patterns,'brands','region')

    daily_visits <- patterns[brands != '' & visits_by_day != '',
                             .(visits_by_day=Reduce("+",
                                                    purrr::map(
                                                      stringr::str_split(
                                                        stringr::str_sub(
                                                          visits_by_day,2,
                                                          nchar(visits_by_day)-1),
                                                        ','),
                                                      as.numeric))),
                             by=.(brands,region)]
    daily_visits[,day := seq_len(.N),by=.(brands,region)]

    # Flatten patterns down
    p_brands <- patterns[brands != '',
                         .(raw_visit_counts=sum(raw_visit_counts,na.rm=TRUE)),
                         by=.(brands,region)]

    p_brands <- p_brands %>%
      dplyr::as_tibble() %>%
      dplyr::left_join(daily_visits %>% as_tibble()) %>%
      mutate(year = as.numeric(stringr::str_sub(filename,42,45)),
             month = as.numeric(stringr::str_sub(filename,47,48))) %>%
      dplyr::left_join(total_visits) %>%
      dplyr::filter(!is.na(day)) %>%
      dplyr::mutate(date = lubridate::ymd(paste(year,month,day,sep='-')))

    dat2[[f]] <- p_brands

    # then sub_category
    data.table::setkey(patterns,'top_category','sub_category','region')

    daily_visits <- patterns[visits_by_day != '',
                             .(visits_by_day=Reduce("+",
                                                    purrr::map(
                                                      stringr::str_split(
                                                        stringr::str_sub(
                                                          visits_by_day,2,
                                                          nchar(visits_by_day)-1),
                                                        ','),
                                                      as.numeric))),
                             by=.(top_category,sub_category,region)]
    daily_visits[,day := seq_len(.N),by=.(top_category,sub_category,region)]

    # Flatten patterns down
    p_sub_category <- patterns[sub_category != '',
                               .(raw_visit_counts=sum(raw_visit_counts,na.rm=TRUE)),
                               by=.(top_category,sub_category,region)]

    p_sub_category <- p_sub_category %>%
      dplyr::as_tibble() %>%
      dplyr::left_join(daily_visits %>% dplyr::as_tibble) %>%
      mutate(year = as.numeric(stringr::str_sub(filename,42,45)),
             month = as.numeric(stringr::str_sub(filename,47,48))) %>%
      dplyr::left_join(total_visits) %>%
      dplyr::filter(!is.na(day)) %>%
      dplyr::mutate(date = lubridate::ymd(paste(year,month,day,sep='-')))

    dat3[[f]] <- p_sub_category
  }

  # Clean up
  file.remove('brand_info.csv',
              'home_panel_summary.csv',
              'README.txt',
              'visit_panel_summary.csv',
              'normalization_stats.csv')

  p_naics_code <- dplyr::bind_rows(dat1) %>%
    dplyr::filter(!is.na(naics_code)) %>%
    data.table::as.data.table()
  p_naics_code <- p_naics_code[,.(visits_by_day = sum(visits_by_day),
                                  total_visits = first(total_visits),
                                  raw_visit_counts = sum(raw_visit_counts)),
                               by = .(naics_code, region, date)] %>%
    dplyr::as_tibble()
  p_brands <- dplyr::bind_rows(dat2) %>%
    dplyr::filter(!is.na(brands)) %>%
    data.table::as.data.table()
  p_brands <- p_brands[,.(visits_by_day = sum(visits_by_day),
                          total_visits = first(total_visits),
                          raw_visit_counts = sum(raw_visit_counts)),
                       by = .(brands, region, date)] %>%
    dplyr::as_tibble()
  p_sub_category <- dplyr::bind_rows(dat3) %>%
    dplyr::filter(!is.na(sub_category)) %>%
    data.table::as.data.table()
  p_sub_category <- p_sub_category[,.(visits_by_day = sum(visits_by_day),
                                      total_visits = first(total_visits),
                                      raw_visit_counts = sum(raw_visit_counts)),
                                   by = .(top_category, sub_category, region, date)] %>%
    dplyr::as_tibble()

  # And return what we've made
  return(list(p_naics_code,p_brands,p_sub_category,norm))
}
