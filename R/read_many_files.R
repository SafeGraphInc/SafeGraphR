#' Read and row-bind many CSVs
#'
#' This accepts a directory. It will load every \code{csv} or \code{csv.gz} in that folder and attempt to row-bind them together. You can alternately specify a list of files if you don't want everything in the folder. This is designed for use with the normalization and home-summary files as downloaded from AWS.
#'
#' @param dir Name of the directory the files are in.
#' @param filelist Optionally specify only a subset of the filename to read in.
#' @param makedate Use \code{year}, \code{month}, and \code{day} columns in the data to create a \code{date} variable. Works with normalization files.
#' @param ... Other arguments to pass to \code{data.table::fread}.
#' @export

read_many_csvs <- function(dir = '.', filelist = NULL, makedate = FALSE, ...) {

  if (is.null(filelist)) {
    filelist <- union(list.files(path=dir,pattern = '\\.csv'),
                      list.files(path=dir,pattern = '\\.csv.gz'))
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
#' @param dir Name of the directory the files are in.
#' @param filelist Optionally specify only a subset of the filename to read in.
#' @param by,fun,na.rm,filter,expand_int,expand_cat,expand_name,multi,select,gen_fips,start_date,... Arguments to be passed to \code{read_patterns}, specified as in \code{help(read_patterns)}.
#' @export

read_many_patterns <- function(filelist,dir = '.',by = NULL, fun = sum, na.rm = TRUE, filter = NULL,
                        expand_int = NULL, expand_cat = NULL,
                        expand_name = NULL, multi = NULL,
                        select=NULL, gen_fips = TRUE, start_date = NULL, ...) {
  if (is.null(filelist)) {
    filelist <- list.files(path=dir,pattern = '\\.csv.gz')
  }

  # If there's only one type specified, we'll get back a
  # data table that can be bound right away
  if (is.null(multi)) {
    filelist %>%
      purrr::map(function(x) read_patterns(x, dir = dir, by = by, fun = fun, na.rm = na.rm, filter = filter,
                                  expand_int = expand_int, expand_cat = expand_cat,
                                  expand_name = expand_name, multi = NULL,
                                  select = select, gen_fips = gen_fips, start_date = start_date, ...)) %>%
      rbindlist() %>%
      return()
  }

  # Otherwise we'll get back a list that we need to unpack before binding
  patterns <- filelist %>%
    purrr::map(function(x) read_patterns(x, dir = dir, multi = multi,
                                select = select, gen_fips = gen_fips, start_date = start_date, ...))

  # Bind each of them together
  retDT <- list()
  for (m in multi) {
    name <- m[['name']]

    retDT[[name]] <- patterns %>%
      purrr::map(function(x) x[[name]]) %>%
      data.table::rbindlist()

  }
}
