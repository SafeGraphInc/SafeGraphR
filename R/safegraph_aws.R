#' Download SafeGraph data from AWS COVID Response
#'
#' This is a thin wrapper for \code{aws.s3::s3sync} that will aim you at the right directory to synchronize.
#'
#' This function doesn't add too much, but it does make the default behavior you probably want a bit easier. If you plan to specify the \code{s3sync::aws.s3} "bucket" option yourself, this function is largely useless.
#'
#' See catalog.safegraph.io for more description of the various buckets.
#'
#' @param path The local directory to synchronize.
#' @param patterns The SafeGraph bucket to get from. Can be "weekly", "weekly-new" (new method since June 2020), "monthly", "monthly backfill", "distancing", "transactions", "core", "geo-supplement" or, to get the baseline bucket, "none". v2 versions always selected.
#' @param bucket_only Instead of doing an \code{aws.s3::s3sync} call, just return the correct bucket as a string. Then you can use that to do your own \code{aws.s3::s3sync} call, or work with the AWS CLI.
#' @param base_url The base URL to pull the data from.
#' @param key A character string containing an AWS Access Key ID.
#' @param secret A character string containing an AWS Secret Access Key.
#' @param region A character string containing the AWS region.
#' @param prefix Leading part of the objects in the bucket must have this prefix. For example, to download social distancing data only from 2020, set this to "2020/".
#' @param ... Additional parameters to be sent to \code{aws.s3::s3sync} and from there on to \code{aws.s3:s3HTTP}. "direction" will be ignored.
#' @export

safegraph_aws <- function(path = '.', patterns, bucket_only = FALSE, base_url = 's3.wasabisys.com', key, secret, region = '', prefix ='', ...) {
  buck <- 's3://sg-c19-response/'

  if (patterns == 'monthly') {
    buck <- paste0(buck,'monthly-patterns/')
  } else if (patterns == 'weekly') {
    buck <- paste0(buck,'weekly-patterns/v2/')
  } else if (patterns == 'distancing') {
    buck <- paste0(buck,'social-distancing/v2/')
  } else if (patterns == 'core') {
    buck <- paste0(buck,'core/')
  } else if (patterns == 'transactions') {
    buck <- paste0(buck, 'transactions-facteus/')
  } else if (patterns == 'monthly backfill') {
    buck <- paste0(buck, 'monthly-patterns/patterns_backfill/')
  } else if (patterns == 'geo-supplement') {
    buck <- paste0(buck, 'geo-supplement/')
  } else if (patterns == 'weekly-new') {
    buck <- paste0(buck, 'weekly-patterns-delivery/weekly/')
  } else if (patterns != 'none') {
    stop('Unrecognized value of patterns.')
  }

  if (bucket_only) {
    return(buck)
  }

  aws.s3::s3sync(path,buck,prefix,'download', key = key, base_url = base_url, secret = secret, region = region, direction = 'download', ...)
}
