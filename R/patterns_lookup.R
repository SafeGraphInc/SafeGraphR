#' SafeGraph File Lookup
#'
#' This function, given a date or range of dates, will return a character vector of folder paths in the weekly (new or backfill) data you will need to run through \code{list.files(pattern = '.csv.gz', full.names = TRUE)} after downloading files (or just set \code{list_files = TRUE}. This is done because the subfolder after this is based on the hour the data is released, which can't be predicted ahead of time for future weeks.
#'
#' @param dates A vector of \code{Date} objects (perhaps taking a single \code{Date} object and adding \code{+lubridate::days(0:finish)}) to find the associated files for.
#' @param dir If specified, will append \code{dir} to the start of the filepaths, to get full filepaths. If using both "old" (pre-June 15, 2020) and "new" (post) dates, this will only work if both the "patterns_backfill" (old) and "patterns" (new) folders are in the same folder. Superseded by \code{old_dir} and \code{new_dir} for old and new files, respectively.
#' @param old_dir If specified, will append \code{old_dir} to the start of the filepaths for all "old" (pre-Dec 7 2020) files. This should be the folder that contains the \code{patterns_backfill} folder.
#' @param new_dir If specified, will append \code{new_dir} to the start of the filepaths for all "new" (post-Dec 7, 2020) files. This should be the folder that contains the \code{patterns} folder.
#' @param subfolder Which folder in the AWS bucket to look at. Will append "_backfill" for backfill data. Usually this is "patterns", "normalization_data", or "home_panel_summary".
#' @param silent If specified, will omit the warning for using any dates after the package author last checked the consistency of the SafeGraph file structure.
#' @param add_ma Also looks at the \code{add_ma} days before the dates listed in \code{dates}, so you can calculate an \code{add_ma}-day moving average. Or you could just change the \code{dates} argument yourself to allow this.
#' @param patterns_backfill_date Character variable with the folder structure for the most recent \code{patterns_backfill} pull. i.e., the 2018, 2019, and 2020 folders containing backfill data in their subfolders should set in the \code{paste0(old_dir,'/patterns_backfill/',patterns_backfill_date)} folder.
#' @param key A character string containing an AWS Access Key ID. If \code{key} and \code{secret} are both specified, \code{patterns_lookup} will download all the files it finds.
#' @param secret A character string containing an AWS Secret Access Key.
#' @param list_files After creating folderpaths (and, possibly, downloading files), run each of them through \code{list.files(pattern = '.csv', recursive = TRUE, full.names = TRUE)} to get a usable list of files. This only works if all the files have already been downloaded.
#' @examples
#'
#' # We have already downloaded all of AWS data into the working directory and just need to locate and load it
#' # (if we also wanted to download, we could leave off list_files and pass this to safegraph_aws,
#' # or add our key and secret here and it would download)
#' filelist <- patterns_lookup(lubridate::ymd('2020-9-01') + lubridate::days(0:100),
#'                              list_files = TRUE)
#'
#' dt <- read_many_patterns(filelist = filelist, by = 'brands', expand_int = 'visits_by_day')
#'
#' # Now let's get the normalization files
#'
#' normlist <- patterns_lookup(lubridate::ymd('2020-9-01') + lubridate::days(0:100),
#'                             subfolder = 'normalization_stats',
#'                             list_files = TRUE)
#' norm <- read_many_csvs(filelist = normlist, makedate = TRUE)
#'
#' @export

patterns_lookup <- function(dates,
                            dir = NULL,
                            old_dir = NULL,
                            new_dir = NULL,
                            subfolder = "patterns",
                            silent = FALSE,
                            add_ma = 0,
                            patterns_backfill_date = '2020/12/14/21/',
                            key = NULL,
                            secret = NULL,
                            list_files = FALSE) {

  if (!lubridate::is.Date(dates)) {
    stop('dates must be a vector of Date objects.')
  }
  if (add_ma < 0) {
    stop('add_ma must be nonnegative.')
  }

  # Fill in null values of dir
  if (is.null(dir)) {
    dir <- ""
  }
  if (is.null(old_dir)) {
    old_dir <- dir
  }
  if (is.null(new_dir)) {
    new_dir <- dir
  }

  # and add trailing /
  if (nchar(old_dir) > 0 & stringr::str_sub(old_dir,-1) != '/') {
    old_dir <- paste0(old_dir,'/')
  }
  if (nchar(new_dir) > 0 & stringr::str_sub(new_dir, -1) != '/') {
    new_dir <- paste0(new_dir,'/')
  }
  if (nchar(patterns_backfill_date) > 0 & stringr::str_sub(patterns_backfill_date, -1) != '/') {
    patterns_backfill_date <- paste0(patterns_backfill_date,'/')
  }

  # Warn about new dates
  if (!silent) {
    if (max(dates) > lubridate::ymd('2021-07-09')) {
      warning('This function has been tested to match the SafeGraph file structure as of July 9, 2021. Any file structure changes since then could make your result wrong.')
    }
  }

  # Add moving-average days
  if (add_ma > 0) {
    dates2 <- dates

    for (d in 1:add_ma) {
      dates2 <- unique(c(dates2, dates - lubridate::days(d)))
    }

    dates <- sort(dates2)
  }

  # Split the dates into new and old
  old <- dates[dates <= lubridate::ymd('2020-12-06')]
  new <- dates[dates >= lubridate::ymd('2020-12-07')]

  filelist <- c()

  if (length(old) > 0) {
    old_dt <- data.table::data.table(date = old)
    # Find the most recent wday = 2, which is the first day in the file
    old_dt[, recent := date + lubridate::days(2 - lubridate::wday(date)) - lubridate::days(7*(lubridate::wday(date) == 1))]

    # And filename
    old_dt[, filename := paste0(
      old_dir,
      subfolder,
      '_backfill/',
      patterns_backfill_date,
      lubridate::year(recent),'/',
      stringr::str_pad(lubridate::month(recent), 2, 'left', '0'), '/',
      stringr::str_pad(lubridate::day(recent), 2, 'left', '0'), '/'
    )]

    filelist <- unique(old_dt$filename)

    # Are we downloading?
    if (!is.null(key) & !is.null(secret)) {
      for (fn in filelist) {
        safegraph_aws(old_dir,
                      dataset = 'weekly-backfill',
                      key = key,
                      secret = secret,
                      prefix = stringr::str_sub(fn, nchar(old_dir)+1),
                      prefix_is_dir = TRUE,
                      max_print = 1)
      }
    }

    if (list_files) {
      filelist <- filelist %>%
        purrr::map(function(x) {
          fls <- list.files(x, pattern = '.csv',
                            recursive = TRUE, full.names = TRUE)

          if (length(fls) == 0) {
            warning(paste0('Found no files in ',x,'. list_files requires files be downloaded first.'))
            return('')
          } else {
            return(fls)
          }
        }) %>% unlist()
    }
  }
  if (length(new) > 0) {
    new_dt <- data.table::data.table(date = new)

    # Find the most recent wday = 2, which is the first day in the file
    # Then add 9 days to get to release date
    new_dt[, recent := date + lubridate::days(2 - lubridate::wday(date)) + lubridate::days(9)  - lubridate::days(7*(lubridate::wday(date) == 1))]
    # This file was late
    new_dt[recent == lubridate::ymd('2021-03-10'), recent := lubridate::ymd('2021-03-11')]

    # And filename
    new_dt[, filename := paste0(
      new_dir,
      subfolder, '/',
      lubridate::year(recent),'/',
      stringr::str_pad(lubridate::month(recent), 2, 'left', '0'), '/',
      stringr::str_pad(lubridate::day(recent), 2, 'left', '0'), '/'
    )]

    # Are we downloading?
    if (!is.null(key) & !is.null(secret)) {
      for (fn in unique(new_dt$filename)) {
        safegraph_aws(new_dir,
                      dataset = 'weekly-new',
                      key = key,
                      secret = secret,
                      prefix = stringr::str_sub(fn, nchar(new_dir)+1),
                      prefix_is_dir = TRUE,
                      max_print = 1)
      }
    }

    if (!list_files) {
      filelist <- c(filelist,unique(new_dt$filename))
    } else {
      filelist <- c(filelist,
                    unique(new_dt$filename) %>%
                      purrr::map(function(x) {
                        fls <- list.files(x, pattern = '.csv',
                                          recursive = TRUE, full.names = TRUE)
                        fls <- fls[!grepl('home_panel_summary',fls)]
                        if (length(fls) == 0) {
                          warning(paste0('Found no files in ',x,'. list_files requires files be downloaded first.'))
                          return(NULL)
                        } else {
                          return(fls)
                        }
                      })) %>%
        unlist()
      filelist <- stringr::str_replace_all(filelist, '//', '/')
    }

  }

  if (list_files) {
    filelist <- filelist[filelist != '']
  }

  return(filelist)
}
