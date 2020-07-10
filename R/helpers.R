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
