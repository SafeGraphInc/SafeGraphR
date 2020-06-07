#' Scale data relative to its value on a date
#'
#' Pick a date and provide some variables. Those variables will be adjusted to be relative to their value on that date. Usually used to calculate foot traffic growth relative to a certain date.
#'
#' @param data A \code{data.frame}, \code{tibble}, or \code{data.table}. Note that a \code{data.table} will be returned.
#' @param date The date you'd like everything relative to, as a date object.
#' @param date_var The name of the date variable.
#' @param adj_vars Character vector of the variable names you'd like adjusted to be relative to the date.
#' @param by Character vector of the variable names you'd like the operation to be performed by. There should only be one observation for which \code{date_var == date} within each combination of the \code{by} variables, or else your results will be arbitrary.
#' @param growth Set to \code{TRUE} to get \code{new/old - 1} (i.e. a percentage growth). Set to \code{FALSE} to get \code{new/old} (i.e. a relative value).
#' @param format_percent Set to \code{TRUE} to get back a formatted percentage, i.e. "50%", instead of a number.
#' @param accuracy If \code{format_percent = TRUE}, the number of digits after the decimal place to round to, as in \code{scales::percent}.
#' @export

scale_to_date <- function(data, date, date_var = 'date',
                          adj_vars, by = NULL, growth = FALSE,
                          format_percent = FALSE, accuracy = .1) {
  # Turn to a data.table
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }

  # Add original order for re-sorting later
  data[,orig_order := seq_len(.N)]

  # Build the ordering string
  dateorder <- paste0('!(',date_var,' == date)')
  datevars <- paste(c(by,dateorder),collapse=',')
  ordertext <- paste0('order(',datevars,')')

  # Build the variable construction
  nonby <- c(names(data)[!(names(data) %in% by)],'orig_order')
  constext <- ifelse(nonby %in% adj_vars,paste0(nonby,' = adjfcn(',nonby,')'),
                     paste0(nonby,' = ',nonby))
  constext <- paste0('.(',paste(constext,collapse = ','),')')

  # Figure out what our adjustment output looks like
  adjfcn <- function(x) {
    x <- x/first(x)

    if (growth) {
      x <- x - 1
    }

    if (format_percent) {
      x <- scales::percent(x, accuracy = accuracy)
    }

    return(x)
  }

  data <- data[eval(parse(text=ordertext)),eval(parse(text=constext)), by = by]

  setorder(data,orig_order)

  data[,orig_order := NULL]

  return(data)
}

