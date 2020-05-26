#' Read a ZIP file with patterns and other data as it comes from the SafeGraph Shop
#'
#' This will open up a ZIP file from the SafeGraph shop and will read all of the data in, performing processing of the patterns files using \code{read_patterns}.
#'
#' The result will be a named list with each of the components of the data.
#'
#' @param file The filename of the \code{.zip} file from the shop.
#' @param dir The directory the file is in.
#' @param keeplist Character vector of the files in the ZIP to read in. Use \code{'patterns'} to refer to the patterns files.
#' @param exdir Name of the directory to unzip to.
#' @param cleanup Set to \code{TRUE} to delete all the unzipped files after being read in.
#' @param start_date An argument to be passed to \code{read_patterns} giving the first date present in the file, as a date object. When using \code{read_shop} this should usually be included, since the patterns file names in the shop files are not in a format \code{read_patterns} can pick up on automatically.
#' @param by,fun,na.rm,filter,expand_int,expand_cat,expand_name,multi,select,gen_fips,... Other arguments to be passed to \code{read_patterns}, specified as in \code{help(read_patterns)}.
#' @export

read_shop <- function(file,dir = '.',keeplist = c('patterns','normalization_stats.csv','home_panel_summary.csv','visit_panel_summary.csv','brand_info.csv'),
                      exdir = dir, cleanup = TRUE,
                      by = NULL, fun = sum, na.rm = TRUE, filter = NULL,
                      expand_int = NULL, expand_cat = NULL,
                      expand_name = NULL, multi = NULL,
                      select=NULL, gen_fips = TRUE, start_date = NULL, ...) {

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

  retDT <- list()

  for (k in keeplist) {
    if (k == 'patterns') {
      # Get our full list of patterns files
      patfiles <- files_in_zip[stringr::str_detect(files_in_zip,'\\.csv\\.gz')]

      retDT[['patterns']] <- read_many_patterns(patfiles, dir = exdir, by = by, fun = fun, na.rm = na.rm, filter = filter,
                                           expand_int = expand_int, expand_cat = expand_cat,
                                           expand_name = expand_name, multi = multi,
                                           select = select, gen_fips = gen_fips, start_date = start_date, ...)
    } else {
      if (stringr::str_sub(dir,nchar(dir)) == '/') {
        target <- paste0(dir,k)
      } else {
        target <- paste(dir,k,sep='/')
      }
      retDT[[k]] <- data.table::fread(target)
    }
  }

  return(retDT)
}
