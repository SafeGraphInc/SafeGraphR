#' Scale data relative to its value on a date
#'
#' Pick a date and provide some variables. Those variables will be adjusted to be relative to their value on that date. Usually used to calculate foot traffic growth relative to a certain date.
#'
#' @param data Any type of data set that can be coerced to a \code{data.table}. Note that a \code{data.table} will be returned.
#' @param adj_vars Character vector of the variable names you'd like adjusted to be relative to the date.
#' @param date The date you'd like everything relative to, as a date object.
#' @param date_var The name of the date variable, as a string.
#' @param by Character vector of the variable names you'd like the operation to be performed by. There should only be one observation for which \code{date_var == date} within each combination of the \code{by} variables, or else your results will be arbitrary.
#' @param growth Set to \code{TRUE} to get \code{new/old - 1} (i.e. a percentage growth). Set to \code{FALSE} to get \code{new/old} (i.e. a relative value).
#' @param format_percent Set to \code{TRUE} to get back a formatted percentage, i.e. "50\%", instead of a number.
#' @param accuracy If \code{format_percent = TRUE}, the number of digits after the decimal place to round to, as in \code{scales::percent}.
#' @examples
#'
#' # Create some data to scale relative to
#' patterns <- data.table(date = c(lubridate::ymd('2020-01-15'),
#'                                 lubridate::ymd('2020-01-16'),
#'                                 lubridate::ymd('2020-01-17')),
#'                                 visits_by_day = c(1,2,3))
#'
#' # Make everything relative to January 15!
#' scale_to_date(patterns, 'visits_by_day', lubridate::ymd('2020-01-15'))[]
#' @export

scale_to_date <- function(data, adj_vars, date, date_var = 'date',
                          by = NULL, growth = TRUE,
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

#' Calculate year-on-year change
#'
#' This takes a data set with a date variable and calculates year-on-year changes for a set of variables of your choice. Returns a \code{data.table}.
#'
#' This will add new variables using \code{yoy_vars}, adding \code{lag} and \code{YOY} variants.
#'
#' @param data Any type of data set that can be coerced to a \code{data.table}.
#' @param yoy_vars String vector of the variable names you want to calculate year-on-year change for.
#' @param date_var The name of the date variable, as a string. Must be formatted as Date objects.
#' @param leap_year_fillin If the date is Feb. 29, the previous year will not have a Feb. 29. Set to \code{TRUE} to fill in by linear interpolation. If set to \code{TRUE}, returned data will be sorted by \code{by} and \code{date_var}.
#' @param by Character vector of the variable names you'd like the operation to be performed by. There should only be one observation per date per combination of \code{by}.
#' @param growth Set to \code{TRUE} to get \code{new/old - 1} (i.e. a percentage growth). Set to \code{FALSE} to get \code{new/old} (i.e. a relative value).
#' @param format_percent Set to \code{TRUE} to get back a formatted percentage, i.e. "50\%", instead of a number.
#' @param accuracy If \code{format_percent = TRUE}, the number of digits after the decimal place to round to, as in \code{scales::percent}.
#' @examples
#'
#' # Create some fake data to do year-on-year calculations with
#' patterns <- data.table::data.table(date = c(lubridate::ymd('2019-01-15'),
#'                                 lubridate::ymd('2019-01-16'),
#'                                 lubridate::ymd('2020-01-15'),
#'                                 lubridate::ymd('2020-01-16')),
#'                                 visits_by_day = c(1,2,3,4))
#'
#' # And scale relative to the year before!
#' scale_yoy(patterns, 'visits_by_day')[]
#'
#' @export

scale_yoy <- function(data, yoy_vars, date_var = 'date',
                      leap_year_fillin = TRUE, by = NULL,
                      growth = TRUE, format_percent = FALSE, accuracy = .1) {
  # Turn to a data.table
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }

  # Create the last-year data. Only keep variables named in options
  datalag <- subset(data,select=c(by,date_var,yoy_vars))

  # Push year forward for merging
  datalag[,eval(parse(text=paste0(date_var,' := ',date_var,'+ lubridate::years(1)')))]

  # Rename the yoy_vars to add a _lag suffix
  data.table::setnames(datalag,old = yoy_vars, new = paste0(yoy_vars,'_lag'))

  # Bring it in
  data <- merge(data,datalag,all.x = TRUE)
  rm(datalag)

  if (leap_year_fillin) {
    data.table::setorderv(data,c(by,date_var))

    feb29 <- paste0('lubridate::month(',date_var,') == 2 & lubridate::day(',date_var,') == 29')

    leapfcn <- function(x) {
      paste0(x,' := data[,.(',date_var,'=',date_var,', ',x,
      '_lag = (data.table::shift(',x,'_lag)+data.table::shift(',x,'_lag, type = "lead"))/2)][',feb29,',',x,'_lag]')
    }



    for (yv in yoy_vars) {
      data[eval(parse(text=feb29)),eval(parse(text=leapfcn(yv)))]
    }
  }

  yoyfcn <- function(x) {
    x <- paste0(x,'/',x,'_lag')

    if (growth) {
      x <- paste0(x,' - 1')
    }

    if (format_percent) {
      x <- paste0('scales::percent(',x,', accuracy = ',accuracy,')')
    }

    return(x)
  }


  # Now, calculate all the YOYs
  yoytext <- paste(paste0(yoy_vars, '_YOY = ',yoyfcn(yoy_vars)),collapse=',')
  yoytext <- paste0('`:=`(',yoytext,')')

  return(data[,eval(parse(text=yoytext))])
}
