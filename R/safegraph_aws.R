#' Download SafeGraph data from AWS COVID Response
#'
#' This is a thin wrapper for \code{aws.s3::s3sync} that will aim you at the right directory to synchronize.
#'
#' This function doesn't add too much, but it does make the default behavior you probably want a bit easier. If you plan to specify the \code{aws.s3::s3sync} "bucket" option yourself, this function is largely useless.
#'
#' See catalog.safegraph.io for more description of the various buckets.
#'
#' @param path The local directory to synchronize.
#' @param dataset The SafeGraph bucket to get from. Can be "weekly" (OLD VERSION), "weekly-new" (new method since December 2020), "weekly-backfill" (the new method for times before December 2020), "monthly" (OLD VERSION, but also includes the backfill data for the new version), "monthly-backfill" (method since Dec 2020), "distancing", "core" (before Nov 2020), "core-new" (Nov 2020 or later), "geo-supplement", "neighborhood" or, to get the baseline bucket, "none". v2 versions always selected.
#' @param bucket_only Instead of doing an \code{aws.s3::s3sync} call, just return the correct bucket as a string. Then you can use that to do your own \code{aws.s3::s3sync} call, or work with the AWS CLI.
#' @param base_url The base URL to pull the data from.
#' @param key A character string containing an AWS Access Key ID.
#' @param secret A character string containing an AWS Secret Access Key.
#' @param region A character string containing the AWS region.
#' @param prefix Leading part of the objects in the bucket must have this prefix. For example, to download social distancing data only from 2020, set this to "2020/".
#' @param prefix_is_dir If \code{FALSE}, the files matching \code{prefix} will be downloaded directly to \code{path}, which may not be desired behavior if \code{prefix} contains a directory (you probably want the directory structure to match!). Set to \code{TRUE} to, in effect, replace \code{path} with \code{paste0(path, prefix)} and so download files to the appropriate folder. Don't use if \code{prefix} also contains file characteristics like extension. This is \code{prefix_IS_dir}, not \code{prefix_CONTAINS_dir}.
#' @param max_print Temporarily set \code{options(max.print)} to this value. This will massively speed up the function, as \code{aws.s3::s3sync} likes to print the full list of files on the server before moving on. The option will be returned to its original value afterwards. Set to \code{NULL} to not alter any options.
#' @param ... Additional parameters to be sent to \code{aws.s3::s3sync} and from there on to \code{aws.s3:s3HTTP}. "direction" will be ignored.
#' @examples
#'
#' \dontrun{
#'
#' # Download all the recent weekly-patterns files to the working directory
#' safegraph_aws(dataset = 'weekly-new', key = 'MYINFO', secret = 'MYOTHERINFO')
#'
#' }
#'
#' @export

safegraph_aws <- function(path = '.',
                          dataset,
                          bucket_only = FALSE,
                          base_url = 's3.wasabisys.com',
                          key, secret,
                          region = '',
                          prefix ='',
                          prefix_is_dir = FALSE,
                          max_print = 1,
                          ...) {

  if (dataset == 'monthly') {
    buck <- 'monthly-patterns/'
  } else if (dataset == 'monthly-new') {
    buck <- 'monthly-patterns-2020-12/'
  } else if (dataset == 'weekly') {
    buck <- 'weekly-patterns/v2/'
  } else if (dataset == 'distancing') {
    buck <- 'social-distancing/v2/'
  } else if (dataset == 'core') {
    buck <- 'core/'
  } else if (dataset == 'geo-supplement') {
    buck <-  'geo-supplement/'
  } else if (dataset == 'weekly-new') {
    buck <-  'weekly-patterns-delivery-2020-12/weekly/'
  } else if (dataset == 'weekly-backfill') {
    buck <- 'weekly-patterns-delivery-2020-12-backfill/'
  } else if (dataset == 'core-new') {
    buck <- 'core-places-delivery/'
  } else if (dataset == 'neighborhood') {
    buck <- 'neighborhood-patterns/'
  }else if (dataset != 'none') {
    stop('Unrecognized value of dataset.')
  }

  if (bucket_only) {
    return(buck)
  }

  if (prefix_is_dir) {
    if (stringr::str_sub(path, -1) != '/') {
      path <- paste0(path, '/')
    }

    path <- paste0(path, prefix)
  }

  if (!is.null(max_print)) {
    mp <- options('max.print')$max.print

    options('max.print' = max_print)

    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
  }

  aws.s3::s3sync(path = path, bucket = 's3://sg-c19-response/',
                 prefix = paste0(buck,prefix),
                 key = key, base_url = base_url, secret = secret,
                 region = region, direction = 'download', ...)

  if (!is.null(max_print)) {
    options('max.print' = mp)
  }

  return(NULL)
}
