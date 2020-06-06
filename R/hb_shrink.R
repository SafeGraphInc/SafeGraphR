#' Hierarchical Bayes Shrinkage
#'
#' This is a function that takes a "success" and a "total" variable (often something like "number of devices staying home" and "total number of devices") and shrinks them to the full data set using shrinkage methods for proportions.
#'
#' This is usually called by group, either with \code{dplyr::group_by} or with the \code{by} argument in a \code{data.table}, so that individual observations can be shrunk to the group level.
#'
#' @param success A numeric integer variable containing the number of successes.
#' @param total A numeric integer variable containing the total sample size.
#' @export

hb_shrink <- function(success,total) {
  expected_theta <- mean(success/total)
  var_theta <- var(success/total)

  # Get the beta dist alpha+beta
  alpha_plus_beta <- (expected_theta*(1-expected_theta)/var_theta) - 1

  # and separate them out
  alpha <- alpha_plus_beta*expected_theta
  beta <- alpha_plus_beta*(1-expected_theta)

  # Posteriors!
  posterior_alpha <- alpha + success
  posterior_beta <- beta + (total - success)

  # Finally, estimate the mean of the beta distribution
  return(posterior_alpha/(posterior_alpha+posterior_beta))
}
