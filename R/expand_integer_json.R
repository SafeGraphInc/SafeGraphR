#' Expand and collapse an integer column
#'
#' This function accepts a \code{data.table} along with a set of grouping variables and a character-format integer-style JSON column (i.e. starts with square brackets, not curly).
#'
#' It expands that JSON column into long format, with one row per observation per value of the JSON column, and then collapses everything according to the set of grouping variables.
#'
#' @param dt data.table object
#' @param expand String indicating the JSON column to be expanded
#' @param index String indicating the name of the new index column
#' @param by Character vector indicating the variables to group by after expanding. Set to \code{NULL} to aggregate across all initial rows, or set to \code{FALSE} to not aggregate at all (this will also add an \code{initial_rowno} column showing the original row number).
#' @param fun Function that takes a vector and returns a single value to use when collapsing to the \code{by} level.
#' @param na.rm Ignore missing values of \code{expand}
#' @param set_key Set the key of \code{dt} to \code{by}. Set to \code{FALSE} if you have already set the key or want it returned without key.
#' @export

expand_integer_json <- function(dt, expand = 'visits_by_day',
                                index = 'day', by = NULL,
                                fun = sum, na.rm = TRUE, set_key = TRUE) {


  if (set_key) {
    setkey(dt,by)
  }

  if (na.rm) {
    nmstatement <- paste0('!is.na(',expand,')')
    dt <- dt[eval(parse(text=nmstatement))]
  }

  if (class(by) == 'logical') {
    if (!by) {
      dt[,initial_rowno := 1:(.N)]
      by <- 'initial_rowno'
    }
  }

  # exptext <- paste0(expand,"=
  #                     Reduce('+',purrr::map(
  #                              # Split at comma
  #                              stringr::str_split(
  #                                # Erase the start/end [ / ]
  #                                stringr::str_sub(",
  #                                   expand,",2,
  #                                  nchar(",expand,")-1),
  #                                ','),
  #                              # and turn into a number
  #                              as.numeric))")
  #
  exptext <- paste0(".(",expand,"= Reduce('+',purrr::map(stringr::str_split(stringr::str_sub(",
                                   expand,",2,nchar(",expand,")-1),','),as.numeric)))")

  dt <- dt[,
     eval(parse(text=exptext)),
     by=by]

  indextext <- paste0(index,':= seq_len(.N)')

  dt[,eval(parse(text=indextext)),by=by]

  return(dt)
}
