#' Read Safegraph Patterns
#'
#' Be aware that the files this is designed to work with are large and this function may take a while to execute. This function takes a single \code{.csv.gz} SafeGraph patterns file and reads it in. The output is a \code{data.table} (or a list of them if multiple are specified) including the file \code{filename} collapsed and expanded in different ways.
#'
#' Note that after reading in data, if \code{gen_fips = TRUE}, state and county names can be merged in using \code{data(fips_to_names)}.
#'
#' @param filename The filename of the \code{.csv.gz} file.
#' @param dir The directory in which the file sits.
#' @param by A character vector giving the variable names of the level to be collapsed to using \code{sum(na.rm=TRUE)}. The resulting data will have X rows per unique combination of \code{by}, where X is 1 if no expand variables are specified, or the length of the expand variable if specified. Set to \code{NULL} to aggregate across all initial rows, or set to \code{FALSE} to not aggregate at all (this will also add an \code{initial_rowno} column showing the original row number).
#' @param fun Function to use to aggregate the expanded variable to the \code{by} level.
#' @param filter A character string describing a logical statement for filtering the data, for example \code{filter = 'state_fips == 6'} would give you only data from California. Will be used as an \code{i} argument in a \code{data.table}, see \code{help(data.table)}. Filtering here instead of afterwards can cut down on time and memory demands.
#' @param na.rm Whether to remove any missing values of the expanded before aggregating.
#' @param expand_int A character variable with the name of The first e JSON variable in integer format ([1,2,3,...]) to be expanded into rows. Cannot be specified along with \code{expand_cat}.
#' @param expand_cat A JSON variable in categorical format ({A: 2, B: 3, etc.}) to be expanded into rows.  Ignored if \code{expand_int} is specified.
#' @param expand_name The name of the new variable to be created with the category index for the expanded variable.
#' @param multi A list of named lists, for the purposes of creating a list of multiple processed files. This will vastly speed up processing over doing each of them one at a time. Each named list has the entry \code{name} as well as any of the options \code{by, fun, filter, expand_int, expand_cat, expand_name} as specified above. If specified, will override other entries of \code{by}, etc..
#' @param naics_link A \code{data.table}, possibly produced by \code{link_poi_naics}, that links \code{safegraph_place_id} and \code{naics_code}. This will allow you to include \code{'naics_code'} in the \code{by} argument. Technically you could have stuff other than \code{naics_code} in here and use that in \code{by} too, I won't stop ya.
#' @param select Character vector of variables to get from the file. Set to \code{NULL} to get all variables.
#' @param gen_fips Set to \code{TRUE} to use the \code{poi_cbg} variable to generate \code{state_fips} and \code{county_fips} variables. This will also result in \code{poi_cbg} being converted to character.
#' @param start_date The first date in the file, as a date object. If omitted, will assume that the filename begins YYYY-MM-DD.
#' @param silent Set to TRUE to suppress timecode message.
#' @param ... Other arguments to be passed to \code{data.table::fread} when reading in the file. For example, \code{nrows} to only read in a certain number of rows.
#' @examples
#'
#' \dontrun{
#' # '2020-04-06-weekly-patterns.csv.gz' is a weekly patterns file in the main-file folder, which is the working directory
#'
#' }
#' @export

read_patterns <- function(filename,dir = '.',by = NULL, fun = sum, na.rm = TRUE, filter = NULL,
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

  # To do gen_fips you need poi_cbg
  if (gen_fips & !is.null(select)) {
    select <- union(select,'poi_cbg')
  }

  # To do naics_link you need safegraph_place_id
  if (!is.null(naics_link) & !is.null(select)) {
    select <- union(select,'safegraph_place_id')
  }

  # Read in data
  if (is.null(select)) {
    patterns <- data.table::fread(file = f, ...)
  } else {
    patterns <- data.table::fread(file = f, select = select, ...)
  }

  if (!is.null(naics_link)) {
    patterns <- merge(patterns,naics_link,all.x=TRUE,by='safegraph_place_id')
  }

  # Get state and county fips
  if (gen_fips) {
    patterns[,poi_cbg := as.character(poi_cbg)]
    patterns[,c('state_fips','county_fips') := fips_from_cbg(poi_cbg)]
  }

  if (is.null(start_date)) {
    message(paste0('Attempting to find format of start_date from filename. It looks like ',
                   stringr::str_sub(filename,1,4),'-',
                   stringr::str_sub(filename,6,7),'-',
                   stringr::str_sub(filename,9,10),'.'))
    start_date <- lubridate::ymd(paste(
      stringr::str_sub(filename,1,4),
      stringr::str_sub(filename,6,7),
      stringr::str_sub(filename,9,10),
      sep = '-'
    ))
  }
  patterns[,start_date := start_date]

  if (is.null(multi) & is.null(by) & is.null(expand_int) & is.null(expand_cat)) {
    if (!is.null(filter)) {
      patterns <- patterns[eval(parse(text=filter))]
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
                                      expand = expand_int,
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
      patternsb <- patternsb[, lapply(.SD, sum, na.rm=TRUE), by=by]
    }

    # Merge in expanded data
    if (!is.null(expand_cat) | !is.null(expand_int)) {
      patternsb <- merge(patternsb, expanded_var, all = TRUE)
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

