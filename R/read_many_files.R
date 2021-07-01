#' Read and row-bind many CSVs
#'
#' This accepts a directory. It will load every \code{csv} or \code{csv.gz} in that folder and attempt to row-bind them together. You can alternately specify a list of files if you don't want everything in the folder. This is designed for use with the normalization and home-summary files as downloaded from AWS.
#'
#' @param dir Name of the directory the files are in.
#' @param recursive Search in all subdirectories as well.
#' @param filelist Optionally specify only a subset of the filename to read in (can contain paths).
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

read_many_csvs <- function(dir = '.', recursive = TRUE, filelist = NULL, makedate = FALSE, ...) {

  if (is.null(filelist)) {
    filelist <- union(list.files(path=dir,pattern = '\\.csv', recursive = recursive),
                      list.files(path=dir,pattern = '\\.csv\\.gz', recursive = recursive))
  }

  if (dir != '.') {
    if (stringr::str_sub(dir,nchar(dir)) == '/') {
      filelist <- paste0(dir,filelist)
    } else {
      filelist <- paste(dir,filelist,sep='/')
    }
  }

  if (makedate) {
    filelist %>%
      purrr::map(function(x) {
        dt <- data.table::fread(x,...)
        dt[,date := lubridate::ymd(paste(year,month,day,sep='-'))]
        dt[,year := NULL]
        dt[,month := NULL]
        dt[,day := NULL]
        return(dt)
        }) %>%
      data.table::rbindlist(fill = TRUE) %>%
      return()
  } else {
    filelist %>%
      purrr::map(function(x) data.table::fread(x,...)) %>%
      data.table::rbindlist(fill = TRUE) %>%
      return()
  }
}

#' Read and row-bind many patterns files
#'
#' This accepts a directory. It will use \code{read_patterns} to load every \code{csv.gz} in that folder, assuming they are all patterns files. It will then row-bind together each of the produced processed files. Finally, if \code{post_by} is specified, it will re-perform the aggregation, handy for new-format patterns files that split the same week's data across multiple files.
#'
#' Note that after reading in data, if \code{gen_fips = TRUE}, state and county names can be merged in using \code{data(fips_to_names)}.
#'
#' @param dir Name of the directory the files are in.
#' @param recursive Search in all subdirectories as well, as with the since-June-24-2020 format of the AWS downloads. There is not currently a way to include only a subset of these subdirectory files. Perhaps run \code{list.files(recursive=TRUE)} on your own and pass a subset of the results to the \code{filelist} option.
#' @param filelist A vector of filenames to read in, OR a named list of options to send to \code{patterns_lookup()}. This list must include \code{dates} for the dates of data you want, and \code{list_files} will be set to \code{TRUE}. If you like, add \code{key} and \code{secret} to this list to also download the files you need.
#' @param start_date A vector of dates giving the first date present in each zip file, to be passed to \code{read_patterns} giving the first date present in the file, as a date object. Unlike in \code{read_patterns}, this value will be added to the data as a variable called \code{start_date} so you can use it in \code{post_by}.
#' @param post_by After reading in all the files, re-perform aggregation to this level. Use a character vector of variable names (or a list of vectors if using \code{multi}). Or just set to \code{TRUE} to have \code{post_by = by} plus, if present, \code{expand_name} or \code{'date'}. Set to \code{FALSE} to skip re-aggregation. Including \code{'start_date'} in both \code{by} and \code{post_by} is a good idea if you aren't using an approach that creates a \code{date} variable. By default this is \code{TRUE} unless \code{by = NULL} (if \code{by = NULL} in a \code{multi} option, it will still be \code{TRUE} by default for that).
#' @param by,fun,na.rm,filter,expand_int,expand_cat,expand_name,multi,naics_link,select,gen_fips,silent,... Arguments to be passed to \code{read_patterns}, specified as in \code{help(read_patterns)}.
#' @examples
#' \dontrun{
#' # Our current working directory is full of .csv.gz files!
#' # Too many... we will probably run out of memory if we try to read them all in at once, so let's chunk it
#' files <- list.files(pattern = '.gz', recursive = TRUE)
#' patterns <- read_many_patterns(filelist = files[1:10],
#'     # We only need these variables (and poi_cbg which is auto-added with gen_fips = TRUE)
#'     select = c('brands','visits_by_day'),
#'     # We want two formatted files to come out. The first aggregates to the state-brand-day level, getting visits by day
#'     multi = list(list(name = 'by_brands', by = c('state_fips','brands'), expand_int = 'visits_by_day'),
#'     # The second aggregates to the state-county-day level but only for Colorado and COnnecticut (see the filter)
#'     list(name = 'co_and_ct', by = c('state_fips','county_fips'), filter = 'state_fips %in% 8:9', expand_int = 'visits_by_day')))
#' patterns_brands <- patterns[[1]]
#' patterns_co_and_ct <- patterns[[2]]
#'
#' # Alternately, find the files we need for the seven days starting December 7, 2020,
#' # read them all in (and if we'd given key and secret too, download them first),
#' # and then aggregate to the state-date level
#' dt <- read_many_patterns(filelist = list(dates = lubridate::ymd("2020-12-07") + lubridate::days(0:6)),
#'                          by = "state_fips", expand_int = 'visits_by_day',
#'                          select = 'visits_by_day')
#'
#'
#' # don't forget that if you want weekly data but AREN'T using visits_by_day
#' # (for example if you're using visitors_home_cbg)
#' # you want start_date in your by option, as in the second list in multi here
#' dt <- read_many_patterns(filelist = list(dates = lubridate::ymd("2020-12-07") + lubridate::days(0:6)),
#'                          select = c('visits_by_day','visitor_home_cbgs'),
#'                          multi = list(list(name = 'visits',by = 'state_fips',
#'                          expand_int = 'visits_by_day',filter = 'state_fips == 6'),
#'                          list(name = 'cbg',by = c('start_date','state_fips'),
#'                          expand_cat = 'visitor_home_cbgs', filter = 'state_fips == 6')))
#' }
#' @export

read_many_patterns <- function(dir = '.',recursive=TRUE, filelist=NULL, start_date = NULL, post_by = !is.null(by), by = NULL, fun = sum, na.rm = TRUE, filter = NULL,
                        expand_int = NULL, expand_cat = NULL,
                        expand_name = NULL, multi = NULL, naics_link = NULL,
                        select=NULL, gen_fips = TRUE, silent = FALSE, ...) {

  # Figure out our files
  if (is.null(filelist)) {
    filelist <- list.files(path=dir,pattern = '\\.csv\\.gz', recursive = recursive)
  }
  # If it's a list, that's a patterns_lookup call
  if (is.list(filelist)) {
    filelist[['list_files']] <- TRUE
    if (is.null(filelist[['dates']])) {
      stop('If specifying filelist as a list for patterns_lookup, the dates option must be specified in that list.')
    }
    filelist <- do.call(patterns_lookup, filelist)
  }


  if (!is.null(start_date)) {
    if (length(start_date) != length(filelist)) {
      stop(paste0('Number of files (',length(filelist),
                  ') does not match number of start_dates (',
                  length(start_date),') to go along with them.'))
    }
  }
  # post_by expanded if only entered once but we need one for each multi
  if (!isFALSE(post_by) & class(post_by) != 'list' & !is.null(multi)) {
    post_by <- 1:length(multi) %>%
      purrr::map(function(x) post_by)
  }

  # If there's only one type specified, we'll get back a
  # data table that can be bound right away
  if (is.null(multi)) {
    patlist <- 1:length(filelist) %>%
      purrr::map(function(x) {
        read_patterns(filelist[x], dir = dir, by = by, fun = fun, na.rm = na.rm, filter = filter,
                      expand_int = expand_int, expand_cat = expand_cat,
                      expand_name = expand_name, multi = NULL, naics_link = naics_link,
                      select = select, gen_fips = gen_fips, start_date = start_date[x], silent = silent, ...)
        })

    patlist <- data.table::rbindlist(patlist, fill = TRUE)

    # Do our re-aggregation
    patlist <- re_aggregate(patlist, post_by, by, fun, expand_name, expand_int, expand_cat)


    return(patlist)

  }

  # Otherwise we'll get back a list that we need to unpack before binding
  patterns <- 1:length(filelist) %>%
    purrr::map(function(x) read_patterns(filelist[x], dir = dir, multi = multi, naics_link = naics_link,
                                select = select, gen_fips = gen_fips, start_date = start_date[x], silent = silent, ...))

  # Bind each of them together
  patterns <- rbind_by_list_pos(patterns)

  # And re-aggregate
  if (length(filelist) > 1) {
    for (i in 1:length(patterns)) {
      func <- multi[[i]][['fun']]
      if (is.null(func)) {
        func <- sum
      }

      patterns[[i]] <- re_aggregate(patterns[[i]],
                                    post_by[[i]],
                                    multi[[i]][['by']],
                                    func,
                                    multi[[i]][['expand_name']],
                                    multi[[i]][['expand_int']],
                                    multi[[i]][['expand_cat']])
    }
  }

  return(patterns)
}


re_aggregate <- function(d, post_by, by, fun, ename, eint, ecat) {
  if (!isFALSE(post_by)) {
    if (isTRUE(post_by)) {
      post_by <- by

      if (!is.null(d[['date']]) & !('date' %in% by)) {
        post_by <- c(by, 'date')
      }

      if (!is.null(d[['start_date']])  & !('start_date' %in% by)) {
        post_by <- unique(c(by, 'date', 'start_date'))
      }
    }

    # Other expansion indices
    if (!is.null(ename)) {
      post_by <- unique(c(post_by, ename))
    } else if (!is.null(eint)) {
      if (is.null(d[['date']])) {
        post_by <- unique(c(post_by, paste0(eint, '_index')))
      }
    } else if (!is.null(ecat)) {
      post_by <- unique(c(post_by, paste0(ecat, '_index')))
    }

    # Avoid type conflicts
    numcols <- names(d)[sapply(d, is.numeric)]
    for (nc in numcols) {
      nctext <- paste0(nc, ' := as.double(', nc, ')')
      d[, eval(parse(text = nctext))]
    }

    d <- d[, lapply(.SD, fun),
                       by = post_by]
  }

  return(d)
}


#' Read and row-bind many files from the SafeGraph Shop
#'
#' This accepts a directory. It will use \code{read_shop} to load every \code{zip} in that folder, assuming they are all files downloaded from the SafeGraph shop. It will then row-bind together each of the subfiles, so you'll get a list where one entry all the normalization data row-bound together, another is all the patterns files, and so on.
#' .
#' Note that after reading in data, if \code{gen_fips = TRUE}, state and county names can be merged in using \code{data(fips_to_names)}.
#'
#' @param dir Name of the directory the files are in.
#' @param recursive Look for files in all subdirectories as well.
#' @param filelist Optionally specify only a subset of the filename to read in.
#' @param start_date A vector of dates giving the first date present in each zip file, to be passed to \code{read_patterns} giving the first date present in the file, as a date object. When using \code{read_many_shop} this **really** should be included, since the patterns file names in the shop files are not in a format \code{read_patterns} can pick up on automatically. If left unspecified, will produce an error. To truly go ahead unspecified, set this to \code{FALSE}.
#' @param keeplist,exdir,cleanup Arguments to be passed to \code{read_shop}, specified as in \code{help(read_shop)}.
#' @param by,fun,na.rm,filter,expand_int,expand_cat,expand_name,multi,naics_link,select,gen_fips,silent,... Other arguments to be passed to \code{read_patterns}, specified as in \code{help(read_patterns)}.
#' @examples
#'
#' \dontrun{
#' # In the working directory we have two shop ZIP files, one for March and one for April.
#' mydata <- read_shop(# I only want some of the sub-files
#'                     keeplist = c('patterns','home_panel_summary.csv'),
#'                     # For patterns, only keep these variables
#'                     select = c('raw_visit_counts', 'region', 'bucketed_dwell_times', 'location_name'),
#'                     # I want two aggregations of patterns - one of total visits by state ('region')
#'                     # and another by location_name that has the dwell times for each brand
#'                     multi = list(
#'                       list(name = 'all',
#'                            by = 'region'),
#'                       list(name = 'location_dwells',
#'                            by = 'location_name',
#'                            expand_cat = 'bucketed_dwell_times',
#'                            expand_name = 'bucketed_times')
#'                       ),
#'                     # Be sure to specify start_date for read_shop
#'                     start_date = c(lubridate::ymd('2020-03-01'),lubridate::ymd('2020-04-01')))
#'
#' # The result is a list with two items- patterns and home_panel_summary.csv
#' # patterns itself is a list with two data.tables inside - 'all' and 'location_name',
#' # aggregated as given.
#'
#' }
#'
#' @export

read_many_shop <- function(dir = '.',recursive = FALSE,filelist=NULL,start_date = NULL,
                           keeplist = c('patterns','normalization_stats.csv','home_panel_summary.csv','visit_panel_summary.csv','brand_info.csv'),
                           exdir = dir, cleanup = TRUE,
                           by = NULL, fun = sum, na.rm = TRUE, filter = NULL,
                           expand_int = NULL, expand_cat = NULL,
                           expand_name = NULL, multi = NULL, naics_link = NULL,
                           select=NULL, gen_fips = FALSE, silent = FALSE, ...) {

  if (is.null(filelist)) {
    filelist <- list.files(path=dir,pattern = '\\.zip', recursive = recursive)
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

  all_files <- 1:length(filelist) %>%
    purrr::map(function(x)
      read_shop(filelist[x],dir = dir,keeplist = keeplist,
               exdir = exdir, cleanup = cleanup,
               by = by, fun = fun, na.rm = na.rm, filter = filter,
               expand_int = expand_int, expand_cat = expand_cat,
               expand_name = expand_name, multi = multi, naics_link = naics_link,
               select=select, gen_fips = gen_fips, start_date = start_date[x],silent=silent, ...))

  if (is.null(multi) | !('patterns' %in% keeplist)) {
    all_files %>%
      rbind_by_list_pos() %>%
      return()
  } else {
    # rbind_by_list_pos won't work on a multi-driven patterns since that's nested deeper.
    # So split out patterns
    pat <- list()
    for (af in 1:length(all_files)) {
      pat[[af]] <- all_files[[af]][['patterns']]
      all_files[[af]][['patterns']] <- NULL
    }
    pat <- rbind_by_list_pos(pat)
    all_files <- rbind_by_list_pos(all_files)
    all_files[['patterns']] <- pat
    return(all_files)
  }

}

