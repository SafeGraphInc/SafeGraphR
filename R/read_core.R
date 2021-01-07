#' Read SafeGraph Core
#'
#' Be aware that the files this is designed to work with are large and this function may take a while to execute. This function takes folder of Core files and reads it them in. The output is a \code{data.table}.
#'
#' AS OF SafeGraphR VERSION 0.3.0 THIS FUNCTION ONLY WORKS WITH NEW CORE FILE FORMATS. For old-format Core files, you can still use the less-flexible and otherwise deprecated  \code{link_poi_naics()} function.
#'
#' @param dir The directory that the CORE files are in. If this folder contains multiple months of Core files, it will use the most recent (this only works if you are using the standard AWS file structure).
#' @param filter A character string describing a logical statement for filtering the data, for example \code{filter = 'naics_code == 512131'} would give you only movie theater POIs. Will be used as an \code{i} argument in a \code{data.table}, see \code{help(data.table)}. Filtering here instead of afterwards can cut down on time and memory demands.
#' @param select Character vector of variables to get from the file. Set to \code{NULL} to get all variables. If you plan to link the results to a patterns file, you will probably want to include \code{'safegraph_place_id'} or \code{'placekey'} in this vector. Note that any variables mentioned in \code{filter} MUST be in \code{select} unless \code{select = NULL}.
#' @param key A character string containing an AWS Access Key ID. If \code{key} and \code{secret} are both specified, \code{read_core} will download the most recent Core files and process them. This process assumes your system date is set correctly, and will only check this month's Core and last month's Core, since one of those shold exist.
#' @param secret A character string containing an AWS Secret Access Key.
#' @param silent Suppress timing messages.
#' @param ... Other arguments to be passed to \code{data.table::fread} when reading in the \code{CSV} files inside of the \code{ZIP}. For example, \code{nrows} to only read in a certain number of rows.
#' @examples
#'
#' \dontrun{
#' # Location of our CORE file
#' # Note we probably don't have to specify 2020/10 if that's the most recent one
#' dir <- '../SafeGraph/core_poi/2020/10/'
#'
#' # Let's only get retail POIs in California
#' # And
#' locations <- read_core(dir = dir,
#'                        filter = 'region == "CA" & floor(naics_code/10000) %in% 44:45')
#' }
#' @export

read_core <- function(dir = 'core_poi/',
                      filter = NULL,
                      select = NULL,
                      key = NULL,
                      secret = NULL,
                      silent = FALSE,
                      ...) {

  # Are we downloading?
  if (!is.null(key) & !is.null(secret)) {
    current_date <- lubridate::today()

    corefiles <- list.files(dir, pattern = '.csv.gz', recursive = TRUE)
    coredates <- NA
    if (length(corefiles) > 0) {
      coredates <- corefiles %>%
        purrr::map_chr(find_date) %>%
        lubridate::ymd() %>%
        max()
    }
    if (is.na(coredates)) {
      coredates <- lubridate::ymd('1970-01-01')
    }



    # If we have the current month, good to go
    if (!(lubridate::year(current_date) == lubridate::year(coredates) &
        lubridate::month(current_date) == lubridate::month(coredates))) {

      created_dir <- FALSE
      if (!dir.exists(paste0(dir,year(current_date),'/',str_pad(month(current_date),2,"left","0"),'/'))) {
        dir.create(paste0(dir,year(current_date),'/',str_pad(month(current_date),2,"left","0"),'/'),
                   recursive = TRUE)
        created_dir <- TRUE
      }

      try(safegraph_aws(paste0(dir,year(current_date),'/',str_pad(month(current_date),2,"left","0"),'/'),
          'core-new',
          prefix = paste0('core_poi/',year(current_date),'/',str_pad(month(current_date),2,"left","0"),'/'),
          key = key, secret = secret))

      # Did we just download anything? If not, try last month too
      if (length(list.files(dir, pattern = '.csv.gz', recursive = TRUE)) == length(corefiles)) {
        if (created_dir) {
          unlink(paste0(dir,year(current_date),'/',str_pad(month(current_date),2,"left","0"),'/'),
                 recursive = TRUE)
        }

        current_date <- current_date - lubridate::months(1)

        try(safegraph_aws(paste0(dir,year(current_date),'/',str_pad(month(current_date),2,"left","0"),'/'),
              'core-new',
              prefix = paste0('core_poi/',year(current_date),'/',str_pad(month(current_date),2,"left","0"),'/'),
              key = key, secret = secret))
      }
    }
  }

  # Check if we have multiple months of data
  corefiles <- list.files(dir, pattern = '.csv.gz', recursive = TRUE)
  if (sum(stringr::str_detect(corefiles, 'core_poi-part1.csv.gz')) > 1) {
    coredates <- corefiles %>%
      purrr::map_chr(find_date) %>%
      lubridate::ymd() %>%
      max()
    if (is.na(coredates)) {
      stop('More than one month of Core data detected in dir, but I can\'t figure out which set of files is most recent. Did you change the file structure or filenames from AWS?')
    }

    # Use only the recent month
    corefiles <- corefiles[stringr::str_detect(corefiles,
                                               paste0(lubridate::year(coredates),'/',stringr::str_pad(lubridate::month(coredates),2,'left','0'),'/',stringr::str_pad(lubridate::day(coredates),2,'left','0'))
    )]
  }

  # Now read
  corefiles %>%
    paste0(dir,.) %>%
    purrr::map(function(x) {
      if (!silent) {
        message(paste('Starting to read',x,'at',Sys.time()))
      }
      patterns <- data.table::fread(x, select = select, ...)
      if (!is.null(filter)) {
        patterns <- patterns[eval(parse(text=filter))]
      }

      return(patterns)
    }) %>%
    data.table::rbindlist() %>%
    unique() %>%
    return()
}

