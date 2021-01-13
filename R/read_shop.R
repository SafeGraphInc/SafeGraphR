#' Read a ZIP file with patterns and other data as it comes from the SafeGraph Shop
#'
#' This will open up a ZIP file from the SafeGraph shop and will read all of the data in, performing processing of the patterns files using \code{read_patterns}.
#'
#' The result will be a named list with each of the components of the data.
#'
#' @param filename The filename of the \code{.zip} file from the shop.
#' @param dir The directory the file is in.
#' @param keeplist Character vector of the files in the ZIP to read in. Use \code{'patterns'} to refer to the patterns files.
#' @param exdir Name of the directory to unzip to.
#' @param cleanup Set to \code{TRUE} to delete all the unzipped files after being read in.
#' @param start_date An argument to be passed to \code{read_patterns} giving the first date present in the file, as a date object. When using \code{read_shop} this should usually be included, since the patterns file names in the shop files are not in a format \code{read_patterns} can pick up on automatically.
#' @param by,fun,na.rm,filter,expand_int,expand_cat,expand_name,multi,naics_link,select,gen_fips,silent,... Other arguments to be passed to \code{read_patterns}, specified as in \code{help(read_patterns)}. NOte that \code{gen_fips} is \code{FALSE} here by default, rather than \code{TRUE} as elsewhere, as files from the shop often do not contain the \code{poi_cbg} variable necessary to use it. Check which state indicator variables you have access to, perhaps \code{region}.
#' @examples
#'
#' \dontrun{
#' # In the working directory I have the file 'shop_file.zip' to read in
#'
#' mydata <- read_shop('shop_file.zip',
#'                     # I only want some of the files
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
#'                     start_date = lubridate::ymd('2020-03-01'))
#'
#' # The result is a list with two items- patterns and home_panel_summary.csv
#' # patterns itself is a list with two data.tables inside - 'all' and 'location_name',
#' # aggregated as given.
#' }
#'
#' @export

read_shop <- function(filename,dir = '.',keeplist = c('patterns','normalization_stats.csv','home_panel_summary.csv','visit_panel_summary.csv','brand_info.csv'),
                      exdir = dir, cleanup = TRUE,
                      by = NULL, fun = sum, na.rm = TRUE, filter = NULL,
                      expand_int = NULL, expand_cat = NULL,
                      expand_name = NULL, multi = NULL, naics_link = NULL,
                      select=NULL, gen_fips = FALSE, silent = FALSE, start_date = NULL, ...) {

  # Where's our zip?
  if (stringr::str_sub(dir,nchar(dir)) == '/') {
    f <- paste0(dir,filename)
  } else {
    f <- paste(dir,filename,sep='/')
  }

  # Get the list of files
  files_in_zip <- utils::unzip(f,list=TRUE)$Name
  # And unzip
  # If patterns isn't in there, then don't unzip them
  # Otherwise, don't bother not-unzipping the rest, won't make a difference
  if ('patterns' %in% keeplist) {
    utils::unzip(f, exdir = exdir)
  } else {
    utils::unzip(f, files = keeplist, exdir = exdir)
    files_in_zip <- keeplist
  }

  # Edit the multi option to make gen_fips FALSE by default
  if (!is.null(multi)) {
    for (m in 1:length(multi)) {
      if (is.null(multi[[m]]$gen_fips)) {
        multi[[m]]$gen_fips <- FALSE
      }
    }
  }

  retDT <- list()

  for (k in keeplist) {
    if (k == 'patterns') {
      # Get our full list of patterns files
      patfiles <- files_in_zip[stringr::str_detect(files_in_zip,'patterns')]

      retDT[['patterns']] <- read_many_patterns(filelist = patfiles, dir = exdir, recursive = FALSE, by = by, fun = fun, na.rm = na.rm, filter = filter,
                                           expand_int = expand_int, expand_cat = expand_cat,
                                           expand_name = expand_name, multi = multi, naics_link = naics_link,
                                           select = select, gen_fips = gen_fips, start_date = start_date, silent = silent)
    } else {
      if (stringr::str_sub(dir,nchar(exdir)) == '/') {
        target <- paste0(exdir,k)
      } else {
        target <- paste(exdir,k,sep='/')
      }
      retDT[[k]] <- data.table::fread(file=target)
    }
  }

  if (cleanup) {
    for (fiz in files_in_zip) {
      if (stringr::str_sub(dir,nchar(exdir)) == '/') {
        file.remove(paste0(exdir,fiz))
      } else {
        file.remove(paste(exdir,fiz,sep='/'))
      }
    }
  }

  return(retDT)
}
