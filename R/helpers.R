#' Pull state and county FIPS from CBG code
#'
#' This function takes a CBG code (as numeric or string) and returns the state and county FIPS codes associated with it.
#'
#' Why a list and not a vector? For \code{data.table} usage.
#'
#' @param cbg CBG code, in numeric or string form. To aid speed since this function is called millions of times, \code{cbg} is not checked to ensure it is a valid CBG identifier.
#' @param return Set to 'state' to get back only state FIPS, 'county' for only county, or 'both' for a list of both (state then county).
#' @examples
#'
#' a_cbg <- '560610112022'
#' fips_from_cbg(a_cbg)
#'
#' # Use with data.table!
#' DT <- data.table::data.table(cbg = c('560610112022','10310112022'))
#' DT[,c('state_fips','county_fips') := fips_from_cbg(cbg)]
#'
#' @export

fips_from_cbg <- function(cbg,return='both') {
  # work with string because we use nchar
  cbg <- as.character(cbg)

  # length of cbg string depends on whether it's a one-digit state fips or two
  onedigitfips <- (nchar(cbg) == 11)

  state <- as.numeric(stringr::str_sub(cbg,1,2 - onedigitfips))
  county <- as.numeric(stringr::str_sub(cbg,3 - onedigitfips,
                             5 - onedigitfips))

  if (return == 'both') {
    return(list(state,county))
  } else if (return == 'state') {
    return(state)
  } else if (return == 'county') {
    return(county)
  } else {
    stop('Invalid return option.')
  }
}

#' Expands the open_hours variable in the Core file
#'
#' This function takes the \code{open_hours} variable in an already-read Core file (stored as a \code{data.table}) and expands it to seven \code{list}-type columns, where the elements of the list in each row are a set of vectors for opening/closing times, in military time format (1:30PM = 13.5). So an observation of \code{c(8,10,12,14)} would be a business that opens at 8, closes at 10, opens again at noon, and closes again at 2PM on that day. Options are available to produce long instead of wide expansions as well.
#'
#' Returns the same \code{data.table} but with the new columns/rows added. May change the order of the data.
#'
#' @param dt A \code{data.table} containing the \code{open_hours} column (or an object that can be coerced to a \code{data.table}).
#' @param format Can be \code{'wide'} (seven new \code{list}-columns, one for each day), \code{'long'} (turn each row into seven rows, then two new columns: one for day-of-week and one \code{list}-column with opening/closing times), or \code{'long-expand'}/\code{'long_expand'} (\code{'long'} but then also break the single list-column out into a set of numeric start/end columns). Note that for \code{'long-expand'}, many locations have more than one set of open/close hours per day, so there will be more than one open/close column each.
#' @param open_hours A character variable with the name of the \code{open_hours} column.
#' @param colnames For \code{format = 'wide'}, the name stub for the column names, by default \code{'open_hours'} to get \code{'open_hoursSunday'}, \code{'open_hoursMonday'}, etc.. For \code{format='long'}, a two-element vector (by default \code{c('weekday','open_hours')}) with the name of the column indicating the day, and the \code{list}-column with the open hours information in it. For \code{format = 'long-expand'}, a three-element vector with the weekday column, the name stub for "opening hour" and the name stub for "closing hour" (with numbers 1, 2, 3, etc. appended afterwards), by default \code{c('weekday','opens','closes')}.
#' @param drop_missing Drop any rows with a missing \code{open_hours} observation.
#' @param convert_hour Convert hour strings like \code{'15:30'} to numbers like \code{15.5}. This does slow down the function.
#' @param days A character vector of the days to keep. Cutting down here can save some time/memory especially if you are not going \code{format = 'wide'}.
#' @export

expand_open_hours <- function(dt,
                              format = c('wide','long','long-expand','long_expand'),
                              open_hours = 'open_hours',
                              colnames = NULL,
                              drop_missing = FALSE,
                              convert_hour = TRUE,
                              days = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')) {
  if (length(format) > 1) {
    format <- format[1]
  }

  if (!(format %in% c('wide','long','long-expand','long_expand'))) {
    stop('Unrecognized value of format.')
  }

  dt <- data.table::as.data.table(dt)

  if (drop_missing) {
    dt <- dt[eval(
      parse(text =
              paste0(
                '!is.na(',open_hours,') & ',
                open_hours, ' != ""'
              )
      )
    )]
  }

  if (is.null(dt[[open_hours]])) {
    stop('open_hours column not found in dt.')
  }

  # Fill in defaults for colnames
  if (is.null(colnames)) {
    if (format == 'wide') {
      colnames <- 'open_hours'
      newnames <- paste0(colnames, c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
    } else if (format == 'long') {
      colnames <- c('weekday','open_hours')
      newnames <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
    } else if (format %in% c('long-expand', 'long_expand')) {
      colnames <- c('weekday','opens','closes')
      newnames <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
    }
  } else {
    if (format == 'wide') {
      newnames <- paste0(colnames, c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
    } else if (format == 'long') {
      newnames <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
    } else if (format %in% c('long-expand', 'long_expand')) {
      newnames <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
    }
  }

  # raw data often has double quotes that fromJSON doesn't like
  var_fix_quotes <- paste0(open_hours, ' := stringr::str_replace_all(',
                           open_hours, ', \'\\"\\"\',\'\\"\')')
  dt[, eval(parse(text = var_fix_quotes))]

  # Fill in nothing
  dt[eval(parse(text = paste0(
    open_hours, ' == ""'
  ))), (open_hours) := '{ \"Mon\": [], \"Tue\": [], \"Wed\": [], \"Thu\": [], \"Fri\": [], \"Sat\": [], \"Sun\": [] }']

  var_create_text <- paste0('listify_open_hours(',
                            open_hours,
                            ', convert_hour)')
  dt[, (newnames) := eval(parse(text = var_create_text))]

  # Drop original open_hours
  drop_text <- paste0(open_hours, ' := NULL')
  dt[, eval(parse(text = drop_text))]

  # Dropping days
  if (length(days) < 7) {
    dropcols <- which(!(c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday') %in% days))
    dropnames <- newnames[dropcols]
    dt[, (dropnames) := NULL]
  }


  if (format == 'wide') {
    return(dt)
  }

  # Or go for long!
  dt <- data.table::melt(dt, measure.vars = newnames,
                         variable.name = colnames[1],
                         value.name = colnames[2])

  if (format == 'long') {
    return(dt)
  }

  # Now to expand columns
  # First, we don't want to overlap
  data.table::setnames(dt, colnames[2], 'list_col_for_exp')

  # How many sets do we need? Based on multiple sets of open/closes
  sets <- max(sapply(dt$list_col_for_exp, length))/2

  # What are our new names?
  exp_names <- as.vector(sapply(1:sets,function(x) paste0(colnames[2:3], x)))

  fillin <- as.data.table(sapply(1:(sets*2), function(x) as.numeric(lapply(dt$list_col_for_exp, `[`,x))))

  dt[, (exp_names) := fillin]
  dt[, list_col_for_exp := NULL]

  return(dt)
}


listify_open_hours <- function(x, convert_hour = TRUE) {

  lst <- lapply(x, jsonlite::fromJSON)

  proc_item <- function(x, n, ch) {
    x <- as.vector(x[[n]])

    if (ch) {
      x <- unname(sapply(x, convert_time_hour))
    }

    return(x)
  }

  comb <- data.table(
    A = lapply(lst, proc_item,1,convert_hour),
    B = lapply(lst, proc_item,2,convert_hour),
    C = lapply(lst, proc_item,3,convert_hour),
    D = lapply(lst, proc_item,4,convert_hour),
    E = lapply(lst, proc_item,5,convert_hour),
    F = lapply(lst, proc_item,6,convert_hour),
    G = lapply(lst, proc_item,7,convert_hour)
  )

  return(comb)
}


convert_time_hour <- function(x) {
  tm <- stringr::str_split(x, ':')[[1]]

  return(as.numeric(tm[1]) + as.numeric(tm[2])/60)
}

#' Row-binds data.tables in a list of lists
#'
#' This function takes a list of lists of \code{data.table}s (or anything that \code{data.table::rbind} accepts, like \code{data.frame}s), and then row-binds them by position or name. For example, if passed \code{list(list(first=A,second=B),list(first=C,second=D))}, you would get back \code{list(first=rbind(A,C),second=rbind(B,D))}.
#'
#' @param dtl List of lists of \code{data.table}s.
#' @param ignore_names If the list is named, match objects across lists only by their position in the list and not by their names.
#' @examples
#'
#' list_of_lists <- list(
#'     list(data.frame(a = 1), data.frame(a = 2), data.frame(a = 3)),
#'     list(data.frame(a = 4), data.frame(a = 5), data.frame(a = 6))
#' )
#' rbind_by_list_pos(list_of_lists)
#'
#' list_of_named_lists <- list(
#'     list(A = data.frame(a = 1), B = data.frame(a = 2), C = data.frame(a = 3)),
#'     list(C = data.frame(a = 4), A = data.frame(a = 5), B = data.frame(a = 6))
#'  )
#' rbind_by_list_pos(list_of_named_lis)
#'
#' @export

rbind_by_list_pos <- function(dtl,ignore_names=FALSE) {

  # How many tables are we binding
  ntabs <- length(dtl[[1]])

  retDT <- list()

  # If we go by position
  if (ignore_names | is.null(names(dtl[[1]]))) {
    for (i in 1:ntabs) {
      retDT[[i]] <- dtl %>%
        purrr::map(function(x) x[[i]]) %>%
        data.table::rbindlist()
    }
  } else {
    for (n in names(dtl[[1]])) {
      retDT[[n]] <- dtl %>%
        purrr::map(function(x) x[[n]]) %>%
        data.table::rbindlist()
    }
  }

  return(retDT)
}

# Tell me a joke
#
# Not for export :)
tell_me_a_joke <- function() {
  message('Why did the researcher delete all the visits in the SafeGraph data except for the one recorded at her house?')

  readline(prompt="I dunno, why? [press any key]")

  message('She preferred to be left to her own devices.')
}

#' Seven-Day Moving Average
#'
#' This function returns a (by default) seven-day moving average of the variable passed in. Make sure the data is pre-sorted by date, and grouped by the appropriate grouping. The data should have no gaps in time.
#'
#' @param x The variable to calculate the moving average of.
#' @param n The number of lags to cover in the moving average.
#' @examples
#'
#' ma(1:9)
#'
#' @export
ma <- function(x,n=7){
  if(length(x) >= 7) {
    return(as.numeric(stats::filter(as.ts(x),rep(1/n,n), sides=1)))
  }
  return(NA_real_)
}
