#' Pull state and county FIPS from CBG code
#'
#' This function takes a CBG code (as numeric or string) and returns the state and county FIPS codes associated with it.
#'
#' Why a list and not a vector? For \code{data.table} usage.
#'
#' @param cbg CBG code, in numeric or string form.
#' @param return Set to 'state' to get back only state FIPS, 'county' for only county, or 'both' for a list of both (state then county).
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
