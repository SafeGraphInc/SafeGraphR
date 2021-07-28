#' Download SafeGraph data from AWS COVID Response
#'
#' This is a thin wrapper for \code{aws.s3::s3sync} that will aim you at the right directory to synchronize.
#'
#' NOTE THE BREAKING CHANGE WITH SafeGraphR 0.4.2: BUCKET NAMES ARE CHANGED AND ACCESS TO OUTDATED VERSIONS OF DATA IS REMOVED.
#'
#' This function doesn't add too much, but it does make the default behavior you probably want a bit easier. If you plan to specify the \code{aws.s3::s3sync} "bucket" option yourself, this function is largely useless.
#'
#' See catalog.safegraph.io for more description of the various buckets.
#'
#' @param path The local directory to synchronize.
#' @param dataset The SafeGraph bucket to get from. Can be "weekly" (new method since July 2021), "weekly-backfill" (the new method for times before July 2021), "monthly" (method since July 2021; also contains backfill folders as \code{*_backfill/}), "neighborhood" (June 2021 and forward), "neighborhood-backfill" (May 2021 and previous), "distancing",  "core", "core-canada", "geo-supplement", or, to get the baseline bucket, "none".
#' @param bucket_only Instead of doing an \code{aws.s3::s3sync} call, just return the correct bucket as a string. Then you can use that to do your own \code{aws.s3::s3sync} call, or work with the AWS CLI.
#' @param base_url The base URL to pull the data from.
#' @param key A character string containing an AWS Access Key ID.
#' @param secret A character string containing an AWS Secret Access Key.
#' @param region A character string containing the AWS region.
#' @param prefix Leading part of the objects in the bucket must have this prefix. For example, to download social distancing data only from 2020, set this to "2020/". Some of the backfill buckets can be tricky because folder structure also includes the release date. For example, for "weekly-backfill" if you want patterns data, you want "patterns_backfill/2021/07/15/15/" and THEN followed by the time period you want like "2021/". If you want backfill data from "monthly", for example patterns, it's "patterns_backfill/2021/07/15/16/", then followed by the year/month. The "neighborhood" buckets use "y=2021/m=06/" etc instead of "2021/06".
#' @param prefix_is_dir If \code{FALSE}, the files matching \code{prefix} will be downloaded directly to \code{path}, which may not be desired behavior if \code{prefix} contains a directory (you probably want the directory structure to match!). Set to \code{TRUE} to, in effect, replace \code{path} with \code{paste0(path, prefix)} and so download files to the appropriate folder. Don't use if \code{prefix} also contains file characteristics like extension. This is \code{prefix_IS_dir}, not \code{prefix_CONTAINS_dir}.
#' @param max_print Temporarily set \code{options(max.print)} to this value. This will massively speed up the function, as \code{aws.s3::s3sync} likes to print the full list of files on the server before moving on. The option will be returned to its original value afterwards. Set to \code{NULL} to not alter any options.
#' @param ... Additional parameters to be sent to \code{aws.s3::s3sync} and from there on to \code{aws.s3:s3HTTP}. "direction" will be ignored.
#' @examples
#'
#' \dontrun{
#'
#' # Download all the recent weekly-patterns files to the working directory
#' safegraph_aws(dataset = 'weekly', key = 'MYINFO', secret = 'MYOTHERINFO')
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

  if (grepl('new',dataset)) {
    stop('As of 0.4.2, the bucket names are changed and the "new" suffix is no longer required. See help(safegraph_aws).')
  }

  if (dataset == 'monthly') {
    buck <- 'monthly-patterns-2020-12/release-2021-07/'
  } else if (dataset == 'weekly') {
    buck <- 'weekly-patterns-delivery-2020-12/release-2021-07/weekly/'
  } else if (dataset == 'weekly-backfill') {
    buck <- 'weekly-patterns-delivery-2020-12-backfill/release-2021-07/weekly/'
  } else if (dataset == 'distancing') {
    buck <- 'social-distancing/v2/'
  } else if (dataset == 'geo-supplement') {
    buck <-  'geo-supplement/'
  } else if (dataset == 'core') {
    buck <- 'core-places-delivery/'
  } else if (dataset == 'core-canada') {
    buck <- 'core-places-canada/'
  } else if (dataset == 'neighborhood-backfill') {
    buck <- 'neighborhood-patterns/neighborhood-patterns/2021/07/07/release-2021-07-01/'
  } else if (dataset == 'neighborhood') {
    buck <- 'neighborhood-patterns/neighborhood-patterns/2021/07/27/release-2021-07-01/'
  } else if (dataset != 'none') {
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

  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
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
