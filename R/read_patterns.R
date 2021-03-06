#' Read SafeGraph Patterns
#'
#' Be aware that the files this is designed to work with are large and this function may take a while to execute. This function takes a single \code{.csv.gz} SafeGraph patterns file and reads it in. The output is a \code{data.table} (or a list of them if multiple are specified) including the file \code{filename} collapsed and expanded in different ways.
#'
#' Note that after reading in data, if \code{gen_fips = TRUE}, state and county names can be merged in using \code{data(fips_to_names)}.
#'
#' @param filename The filename of the \code{.csv.gz} file or the path to the file. Note that if \code{start_date} is not specified, \code{read_patterns} will attempt to get the start date from the first ten characters of the path. In "new format" filepaths ("2020/01/09/core-patterns-part-1.csv.gz"), nine days will be subtracted from the date found.
#' @param dir The directory in which the file sits.
#' @param by A character vector giving the variable names of the level to be collapsed to using \code{fun}. The resulting data will have X rows per unique combination of \code{by}, where X is 1 if no expand variables are specified, or the length of the expand variable if specified. Set to \code{NULL} to aggregate across all initial rows, or set to \code{FALSE} to not aggregate at all (this will also add an \code{initial_rowno} column showing the original row number). You can also avoid aggregating by doing \code{by = 'placekey'} which might play more nicely with some of the other features..
#' @param fun Function to use to aggregate the expanded variable to the \code{by} level.
#' @param filter A character string describing a logical statement for filtering the data, for example \code{filter = 'state_fips == 6'} would give you only data from California. Will be used as an \code{i} argument in a \code{data.table}, see \code{help(data.table)}. Filtering here instead of afterwards can cut down on time and memory demands.
#' @param na.rm Whether to remove any missing values of the expanded variable before aggregating. Does not remove missing values of the \code{by} variables. May not be necessary if \code{fun} handles \code{NA}s on its own.
#' @param expand_int A character variable with the name of The first e JSON variable in integer format ([1,2,3,...]) to be expanded into rows. Cannot be specified along with \code{expand_cat}.
#' @param expand_cat A JSON variable in categorical format ({A: 2, B: 3, etc.}) to be expanded into rows.  Ignored if \code{expand_int} is specified.
#' @param expand_name The name of the new variable to be created with the category index for the expanded variable.
#' @param multi A list of lists, for the purposes of creating a list of multiple processed files. This will vastly speed up processing over doing each of them one at a time. Each named list has the entry \code{name} as well as any of the options \code{by, fun, filter, expand_int, expand_cat, expand_name} as specified above. If specified, will override other entries of \code{by}, etc..
#' @param naics_link A \code{data.table}, possibly produced by \code{link_poi_naics}, that links \code{placekey} and \code{naics_code}. This will allow you to include \code{'naics_code'} in the \code{by} argument. Technically you could have stuff other than \code{naics_code} in here and use that in \code{by} too, I won't stop ya.
#' @param select Character vector of variables to get from the file. Set to \code{NULL} to get all variables. **Specifying select is very much recommended, and will speed up the function a lot.**
#' @param gen_fips Set to \code{TRUE} to use the \code{poi_cbg} variable to generate \code{state_fips} and \code{county_fips} variables. This will also result in \code{poi_cbg} being converted to character.
#' @param start_date The first date in the file, as a date object. If omitted, will assume that the filename begins YYYY-MM-DD.
#' @param silent Set to TRUE to suppress timecode message.
#' @param ... Other arguments to be passed to \code{data.table::fread} when reading in the file. For example, \code{nrows} to only read in a certain number of rows.
#' @examples
#'
#' \dontrun{
#' # 'patterns-part-1.csv.gz' is a weekly patterns file in the main-file folder, which is the working directory
#' patterns <- read_patterns('patterns-part-1.csv.gz',
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

read_patterns <- function(filename,dir = '.',by = NULL, fun = function(x) sum(x, na.rm = TRUE), na.rm = TRUE, filter = NULL,
                        expand_int = NULL, expand_cat = NULL,
                        expand_name = NULL, multi = NULL, naics_link = NULL,
                        select=NULL, gen_fips = TRUE, start_date = NULL, silent = FALSE, ...) {

  if (!silent) {
    message(paste('Starting to read',filename,'at',Sys.time()))
  }

  if (stringr::str_sub(dir,nchar(dir)) == '/') {
    f <- paste0(dir,filename)
  } else {
    f <- paste(dir,filename,sep='/')
  }
  if (dir == '.') {
    f <- filename
  }

  # To do gen_fips you need poi_cbg
  if (gen_fips & !is.null(select)) {
    select <- union(select,'poi_cbg')
  }

  # To do naics_link you need placekey
  if (!is.null(naics_link) & !is.null(select)) {
    select <- union(select,'placekey')
  }

  # Read in data
  if (is.null(select)) {
    patterns <- data.table::fread(file = f, ...)
  } else {
    patterns <- data.table::fread(file = f, select = select, ...)
  }

  if (!is.null(naics_link)) {
    patterns <- merge(patterns,naics_link,all.x=TRUE,by='placekey')
  }

  # Get state and county fips
  if (gen_fips) {
    patterns[,poi_cbg := as.character(poi_cbg)]
    patterns[,c('state_fips','county_fips') := fips_from_cbg(poi_cbg)]

    if (!('county_fips' %in% by) & (!is.null(by))) {
      patterns[, county_fips := NULL]
    }
  }

  if (is.null(start_date)) {
    datefromfile <- find_date(filename)

    start_date <- lubridate::ymd(datefromfile)
    if (stringr::str_detect(filename, 'patterns/')) {
      start_date <- start_date - lubridate::days(9)
      # Was late this one week!
      if (start_date == lubridate::ymd('2021-03-02')) {
        start_date <- start_date - lubridate::days(1)
      }
    }
    if (is.na(start_date)) {
      message(paste0('Attempted to find start_date from filename but failed. Start of filename is ',
                     stringr::str_sub(filename,1,10)))
    } else {
      message(paste0('Attempted to find start_date from filename. I think it\'s YYYY-MM-DD ',start_date))
    }
  }
  patterns[,start_date := start_date]

  if (is.null(multi) & is.null(by) & is.null(expand_int) & is.null(expand_cat)) {
    if (!is.null(filter)) {
      patterns <- patterns[eval(parse(text=filter))]

      # If we've dropped everything, return a blank data.table
      if (nrow(patterns) == 0) {
        warning(paste0('After applying filter, no observations left in ',filename))
        return(data.table::data.table())
      }
    }

    return(patterns)
  }

  # If one is specified, convert to list format for code reuse
  if (is.null(multi)) {
    multi <- list(list(name = 'out', by = by, fun = fun, na.rm = na.rm, filter = filter, expand_int = expand_int,
                       expand_cat = expand_cat, expand_name = expand_name))
  }

  retDT <- list()

  # Go through the list of specified outs!
  for (o in multi) {
    name <- o[['name']]
    by <- o[['by']]
    fun <- o[['fun']]
    if (is.null(fun)) {
      fun <- sum
    }
    na.rm <- o[['na.rm']]
    if (is.null(na.rm)) {
      na.rm <- TRUE
    }
    filter <- o[['filter']]
    expand_int <- o[['expand_int']]
    expand_cat <- o[['expand_cat']]
    expand_name <- o[['expand_name']]

    # Work with a copy
    if (is.null(filter)) {
      patternsb <- patterns
    } else {
      patternsb <- patterns[eval(parse(text=filter))]
      if (nrow(patternsb) == 0) {
        warning(paste0('After applying filter, no observations left in ',filename))
        return(data.table::data.table())
      }
    }


    # If no collapsing is desired
    if (class(by) == 'logical') {
      if (!by) {
        patternsb[,initial_rowno := 1:(.N)]
        by <- 'initial_rowno'
      }
    }

    # If expanded name not specified
    if (is.null(expand_name)) {
      if (!is.null(expand_cat)) {
        expand_name <- paste0(expand_cat,'_index')
      }
      if (!is.null(expand_int)) {
        expand_name <- paste0(expand_int,'_index')
        if (expand_int == 'visits_by_day') {
          expand_name <- 'day'
        }
      }
    }

    data.table::setkeyv(patternsb,by)

    if (!is.null(expand_cat)) {
      expanded_var <- expand_cat_json(patternsb,
                                      expand = expand_cat,
                                      index = expand_name,
                                      by = by,
                                      fun = fun,
                                      na.rm = na.rm,
                                      set_key = FALSE)
    }

    if (!is.null(expand_int)) {

      expanded_var <- expand_integer_json(patternsb,
                                          expand = expand_int,
                                          index = expand_name,
                                          by = by,
                                          fun = fun,
                                          na.rm = na.rm,
                                          set_key = FALSE)
    }

    if (!is.null(by)) {
      # Can only keep summable (numeric) or by-variables
      patternsb <- subset(patternsb,
                          select=(sapply(patternsb,is.numeric) & (names(patternsb) != 'initial_rowno')) |
                            names(patternsb) %in% by)

      # And collapse
      if (ncol(patternsb) > length(by)) {
        # Avoid type conflicts
        numcols <- names(patternsb)[sapply(patternsb, is.numeric)]
        for (nc in numcols) {
          nctext <- paste0(nc, ' := as.double(', nc, ')')
          patternsb[, eval(parse(text = nctext))]
        }

        patternsb <- patternsb[, lapply(.SD, fun),
                               by = by]
      } else {
        patternsb <- unique(patternsb)
      }
    }

    # Merge in expanded data
    if (!is.null(expand_cat) | !is.null(expand_int)) {
      patternsb <- merge(patternsb, expanded_var, all = TRUE, by = by)
    }

    # Add start_date back in if lost from collapsing
    patternsb[,start_date := start_date]

    # If we are working with visits_by_day, add in date
    if (!is.null(expand_int)) {
      if (expand_int == 'visits_by_day') {
        exptext <- paste0('date := start_date + lubridate::days(',expand_name,'-1)')
        patternsb[,eval(parse(text=exptext))]
      }
    }

    # Add to the list
    retDT[[name]] <- patternsb
    # And clean
    rm(patternsb)
  }


  # If we only made one, don't want a list
  if (length(retDT) == 1) {
    retDT <- retDT[[1]]
  }

  return(retDT)
}


# Function to pick the date out of a filepath
#' Find the date in a SafeGraph AWS-formatted filepath
#'
#' Given a filepath \code{s}, this function will look for the last correctly-parsed date in that string. Given how SafeGraph AWS file structures are, this will give you the date of those files, for example \code{patterns_backfill/2020/12/14/21/2018/01/01} will give you "2018/01/01".
#'
#' This function returns a string, not a date. You may want to send it to \code{as.Date()} or \code{lubridate::ymd}.
#'
#' For backfill data, the date returned will generally be the \code{start_date} for the files. However, for new data, you will want to do \code{as.Date(find_date(s)) - lubridate::days(9)} to get the \code{start_date}.
#'
#' @param s The filepath to look for a date in.
#' @examples
#'
#' start_date <- find_date('patterns_backfill/2020/12/14/21/2018/01/01') %>% as.Date()
#'
#' @export
find_date <- function(s) {
  # Go through the string and keep the last correctly-parsed date
  # requiring it start with a full four-digit year

  # First, purge any // blank directories, as this can lead to only the first digit of the day being picked up
  s <- stringr::str_replace_all(s,'//','/')

  thepos <- NA
  for (c in 1:(nchar(s)-9)) {
    check <- suppressWarnings(lubridate::ymd(
      stringr::str_sub(s, c, c+9)
    ))
    check4 <- suppressWarnings(as.numeric(
      stringr::str_sub(s, c, c+3)
    ))
    if (!is.na(check) & isTRUE(check4 > 1000)) {
      thepos <- c
    }
  }
  return(stringr::str_sub(s,thepos,thepos+9))
}
