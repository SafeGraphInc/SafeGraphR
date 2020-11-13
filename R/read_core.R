#' Read SafeGraph Core
#'
#' Be aware that the files this is designed to work with are large and this function may take a while to execute. This function takes a single \code{.zip} SafeGraph Core file and reads it in. The output is a \code{data.table}.
#'
#' If you want \emph{all} of the rows and only want to extract their \code{naics_code}, you can use the deprecated but still-working \code{link_poi_naics()} function.
#'
#' @param filename The filename of the \code{ZIP} Core Places file.
#' @param dir The directory that the file is in.
#' @param exdir Name of the directory to unzip to.
#' @param cleanup Set to \code{TRUE} to delete all the unzipped files after being read in.
#' @param filter A character string describing a logical statement for filtering the data, for example \code{filter = 'naics_code == 512131'} would give you only movie theater POIs. Will be used as an \code{i} argument in a \code{data.table}, see \code{help(data.table)}. Filtering here instead of afterwards can cut down on time and memory demands.
#' @param select Character vector of variables to get from the file. Set to \code{NULL} to get all variables. If you plan to link the results to a patterns file, you will probably want to include \code{'safegraph_place_id'} or \code{'placekey'} in this vector. Note that any variables mentioned in \code{filter} MUST be in \code{select} unless \code{select = NULL}.
#' @param silent Suppress timing messages.
#' @param ... Other arguments to be passed to \code{data.table::fread} when reading in the \code{CSV} files inside of the \code{ZIP}. For example, \code{nrows} to only read in a certain number of rows.
#' @examples
#'
#' \dontrun{
#' # Location of our CORE file
#' dir <- '../SafeGraph/2020/10/'
#' filename <- 'Core-USA-Oct-CORE_POI-2020_09-2020-10-19.zip'
#'
#' # Let's only get retail POIs in California
#' # And
#' locations <- read_core(filename = filename,
#'                        dir = dir,
#'                        filter = 'region == "CA" & floor(naics_code/10000) %in% 44:45')
#' }
#' @export

read_core <- function(filename,
                      dir = '.',
                      exdir = '.',
                      cleanup = TRUE,
                      filter = NULL,
                      select = NULL,
                      silent = FALSE,
                      ...) {


  # Where's our zip?
  if (!(stringr::str_sub(dir,nchar(dir)) == '/')) {
    dir <- paste0(dir,'/')
  }
  if (stringr::str_sub(exdir,nchar(exdir)) == '/') {
    exdir <- stringr::str_sub(exdir, 1, nchar(exdir)-1)
  }

  f <- paste0(dir,filename)

  # Get the list of files
  files_in_zip <- utils::unzip(f,list=TRUE)$Name
  # Only the .csv.gzs count
  files_in_zip <- files_in_zip[grep('\\.csv\\.gz',files_in_zip)]
  # And unzip
  utils::unzip(f, files = files_in_zip, exdir = exdir)

  files_in_zip %>%
    paste0(dir,.) %>%
    purrr::map(function(x) {
      if (!silent) {
        message(paste('Starting to read',x,'at',Sys.time()))
      }
      patterns <- data.table::fread(x, select = select, ...)
      if (!is.null(filter)) {
        patterns <- patterns[eval(parse(text=filter))]
      }
      if (cleanup) {
        file.remove(x)
      }
      return(patterns)
    }) %>%
    data.table::rbindlist() %>%
    unique() %>%
    return()
}

