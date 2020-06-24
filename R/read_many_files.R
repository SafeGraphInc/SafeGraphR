#' Read and row-bind many CSVs
#'
#' This accepts a directory. It will load every \code{csv} or \code{csv.gz} in that folder and attempt to row-bind them together. You can alternately specify a list of files if you don't want everything in the folder. This is designed for use with the normalization and home-summary files as downloaded from AWS.
#'
#' @param dir Name of the directory the files are in.
#' @param filelist Optionally specify only a subset of the filename to read in.
#' @param makedate Use \code{year}, \code{month}, and \code{day} columns in the data to create a \code{date} variable. Works with normalization files.
#' @param ... Other arguments to pass to \code{data.table::fread}.
#' @examples
#' \dontrun{
#'
#' # The current working directory contains all the normalization .csv files
#' normalization <- read_many_csvs(makedate = TRUE)
#'
#' }
#' @export

read_many_csvs <- function(dir = '.', filelist = NULL, makedate = FALSE, ...) {

  if (is.null(filelist)) {
    filelist <- union(list.files(path=dir,pattern = '\\.csv'),
                      list.files(path=dir,pattern = '\\.csv\\.gz'))
  }

  if (stringr::str_sub(dir,nchar(dir)) == '/') {
    filelist <- paste0(dir,filelist)
  } else {
    filelist <- paste(dir,filelist,sep='/')
  }

  if (makedate) {
    filelist %>%
      purrr::map(function(x) {
        dt <- data.table::fread(x,...)
        dt[,date := lubridate::ymd(paste(year,month,day,sep='-'))]
        return(dt)
        }) %>%
      data.table::rbindlist() %>%
      return()
  } else {
    filelist %>%
      purrr::map(function(x) data.table::fread(x,...)) %>%
      data.table::rbindlist() %>%
      return()
  }
}

#' Read and row-bind many patterns files
#'
#' This accepts a directory. It will use \code{read_patterns} to load every \code{csv.gz} in that folder, assuming they are all patterns files. It will then row-bind together each of the produced processed files.
#'
#' Note that after reading in data, if \code{gen_fips = TRUE}, state and county names can be merged in using \code{data(fips_to_names)}.
#'
#' @param dir Name of the directory the files are in.
#' @param filelist Optionally specify only a subset of the files to read in.
#' @param start_date A vector of dates giving the first date present in each zip file, to be passed to \code{read_patterns} giving the first date present in the file, as a date object.
#' @param by,fun,na.rm,filter,expand_int,expand_cat,expand_name,multi,naics_link,select,gen_fips,silent,... Arguments to be passed to \code{read_patterns}, specified as in \code{help(read_patterns)}.
#' @examples
#' \dontrun{
#' # Our current working directory is full of .csv.gz files!
#' # Too many... we will probably run out of memory if we try to read them all in at once, so let's chunk it
#' files <- list.files(pattern = '.gz')
#' patterns <- read_many_patterns(filelist = files[1:10],
#'     # We only need these variables (and poi_cbg which is auto-added with gen_fips = TRUE)
#'     select = c('brands','visits_by_day'),
#'     # We want two formatted files to come out. The first aggregates to the state-brand-day level, getting visits by day
#'     multi = list(list(name = 'by_brands', by = c('state_fips','brands'), expand_int = 'visits_by_day'),
#'     # The second aggregates to the state-county-day level but only for Colorado and COnnecticut (see the filter)
#'     list(name = 'co_and_ct', by = c('state_fips','county_fips'), filter = 'state_fips %in% 8:9', expand_int = 'visits_by_day')))
#' patterns_brands <- patterns[[1]]
#' patterns_co_and_ct <- patterns[[2]]
#' }
#' @export

read_many_patterns <- function(dir = '.',filelist=NULL,by = NULL, fun = sum, na.rm = TRUE, filter = NULL,
                        expand_int = NULL, expand_cat = NULL,
                        expand_name = NULL, multi = NULL, naics_link = NULL,
                        select=NULL, gen_fips = TRUE, start_date = NULL, silent = FALSE, ...) {
  if (is.null(filelist)) {
    filelist <- list.files(path=dir,pattern = '\\.csv\\.gz')
  }
  if (!is.null(start_date)) {
    if (length(start_date) != length(filelist)) {
      stop(paste0('Number of files (',length(filelist),
                  ') does not match number of start_dates (',
                  length(start_date),') to go along with them.'))
    }
  }

  # If there's only one type specified, we'll get back a
  # data table that can be bound right away
  if (is.null(multi)) {
    1:length(filelist) %>%
      purrr::map(function(x) read_patterns(filelist[x], dir = dir, by = by, fun = fun, na.rm = na.rm, filter = filter,
                                  expand_int = expand_int, expand_cat = expand_cat,
                                  expand_name = expand_name, multi = NULL, naics_link = naics_link,
                                  select = select, gen_fips = gen_fips, start_date = start_date[x], silent = silent, ...)) %>%
      rbindlist() %>%
      return()
  }

  # Otherwise we'll get back a list that we need to unpack before binding
  patterns <- 1:length(filelist) %>%
    purrr::map(function(x) read_patterns(filelist[x], dir = dir, multi = multi, naics_link = naics_link,
                                select = select, gen_fips = gen_fips, start_date = start_date[x], silent = silent, ...))

  # Bind each of them together
  return(rbind_by_list_pos(patterns))
}


#' Read and row-bind many files from the SafeGraph Shop
#'
#' This accepts a directory. It will use \code{read_shop} to load every \code{zip} in that folder, assuming they are all files downloaded from the SafeGraph shop. It will then row-bind together each of the subfiles, so you'll get a list where one entry all the normalization data row-bound together, another is all the patterns files, and so on.
#' .
#' Note that after reading in data, if \code{gen_fips = TRUE}, state and county names can be merged in using \code{data(fips_to_names)}.
#'
#' @param dir Name of the directory the files are in.
#' @param filelist Optionally specify only a subset of the filename to read in.
#' @param start_date A vector of dates giving the first date present in each zip file, to be passed to \code{read_patterns} giving the first date present in the file, as a date object. When using \code{read_many_shop} this **really** should be included, since the patterns file names in the shop files are not in a format \code{read_patterns} can pick up on automatically. If left unspecified, will produce an error. To truly go ahead unspecified, set this to \code{FALSE}.
#' @param keeplist,exdir,cleanup Arguments to be passed to \code{read_shop}, specified as in \code{help(read_shop)}.
#' @param by,fun,na.rm,filter,expand_int,expand_cat,expand_name,multi,naics_link,select,gen_fips,silent,... Other arguments to be passed to \code{read_patterns}, specified as in \code{help(read_patterns)}.
#' @export

read_many_shop <- function(dir = '.',filelist=NULL,start_date = NULL,
                           keeplist = c('patterns','normalization_stats.csv','home_panel_summary.csv','visit_panel_summary.csv','brand_info.csv'),
                           exdir = dir, cleanup = TRUE,
                           by = NULL, fun = sum, na.rm = TRUE, filter = NULL,
                           expand_int = NULL, expand_cat = NULL,
                           expand_name = NULL, multi = NULL, naics_link = NULL,
                           select=NULL, gen_fips = TRUE, silent = FALSE, ...) {

  if (is.null(filelist)) {
    filelist <- list.files(path=dir,pattern = '\\.zip')
  }

  if (is.null(start_date)) {
    stop('It is very strongly recommended that you specify a vector of starting dates in start_date when using read_many_shop. Otherwise you will likely have patterns data without dates in them. To proceed without specifying dates, set start_date = FALSE.')
  }
  if (is.logical(start_date)) {
    if (!start_date) {
      start_date <- NULL
    } else {
      stop('stop_date cannot be TRUE.')
    }
  }
  if (!is.null(start_date)) {
    if (length(start_date) != length(filelist)) {
      stop(paste0('Number of files (',length(filelist),
                  ') does not match number of start_dates (',
                  length(start_date),') to go along with them.'))
    }
  }

  1:length(filelist) %>%
    purrr::map(function(x)
      read_shop(filelist[x],dir = dir,keeplist = keeplist,
               exdir = exdir, cleanup = cleanup,
               by = by, fun = fun, na.rm = na.rm, filter = filter,
               expand_int = expand_int, expand_cat = expand_cat,
               expand_name = expand_name, multi = multi, naics_link = naics_link,
               select=select, gen_fips = gen_fips, start_date = start_date[x],silent=silent, ...)) %>%
    rbind_by_list_pos() %>%
    return()
}

