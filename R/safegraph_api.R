#' Download SafeGraph data from the API
#'
#' THIS FUNCTION IS NOT YET FULLY OPERATIONAL AND WILL ONLY WORK FOR PLACEKEY CALLS
#'
#' This function will allow you to make API requests of the SafeGraph API. See the documentation for the Places API here: \url{https://docs.safegraph.com/docs/places-api}.
#'
#' @param key A character string containing an API Access Key. See \url{https://docs.safegraph.com/reference/access-and-authentication} to get one.
#' @param placekeys A character vector of Placekeys to look up data with. If this is more than 20 Placekeys long, batching will be performed automatically as long as \code{batch = TRUE}. Cannot be longer than 1000 entries. Exactly one of \code{placekeys}, \code{address}, or \code{search} must be specified.
#' @param location A named vector of location data, or one-row \code{data.frame} with appropriately-named columns, that specifies a single place of interest to look up data for. Available location variable names, and the different combinations that uniquely identify a single location, are available at \url{https://docs.safegraph.com/reference/lookup-name-address}.  Exactly one of \code{placekeys}, \code{address}, or \code{search} must be specified.
#' @param search A named list of filter settings that specifies a set of filter criteria for the SafeGraph POIs. Data will be returned for the first \code{first} matches.
#' @param first If using \code{search}, return only the first \code{first} matches found. If set to any number above \code{20}, batching will be performed automatically if \code{batch = TRUE}. Will not accept a value above \code{1000}.
#' @param after If using \code{search}, skip the first \code{after} matches before returning the next \code{first} matches.
#' @param dataset The SafeGraph response dataset(s) to get from. Can be \code{'core'} for the SafeGraph Core data, \code{'geometry'} for geometry files, or \code{'weekly_patterns'} or \code{'monthly_patterns'} for weekly/monthly patterns data. Weekly patterns data will be the week of your choosing; monthly patterns data will always be the most recent month. Or, if using the \code{location} or \code{search} options, set to \code{'placekey'} to return only the Placekeys and not actual data (note you'll get the Placekeys anyway with all the other options). Defaults to \code{'weekly_patterns'}. See \url{https://docs.safegraph.com/reference/safegraph-response-datasets} for more information.
#' @param date If \code{dataset = 'weekly_patterns'}, this option is required. A string in \code{'YYYY-MM-DD'} format specifying the week of data you want. Can be any day in that week. The \code{start_date} and \code{end_date} variant currently not supported.
#' @param select A character vector with the names of the variables you want returned. Defaults to all variables in the dataset. For the list of variables in each \code{dataset}, see the "Response Objects" section on \url{https://docs.safegraph.com/reference/safegraph-response-datasets}. For variables like \code{brands}, which has sub-variables \code{brand_id} and \code{brand_name}, putting \code{brands} will get all the sub-variables, or you can just get the sub-variables by themselves.
#' @param batch Set to \code{TRUE} to allow for batching of results if there are more than 20 POIs being returned. Batching may be quite slow if there are a lot of matches! See the rate limiting in the Placekey API docs. Also note the 1000-per-minute rate limit, so if you decide to run multiple of your own large \code{safegraph_api} calls you may want to space them out.
#' @param display_call Set to \code{TRUE} to print out the API call.
#' @param ... Currently unused
#' @examples
#'
#' \dontrun{
#'
#' # You can look up data for individual placekeys
#' mydat = safegraph_api('MY API KEY', placekeys = "222-223@5x4-4b6-mff", select = 'open_hours')
#' # Or a vector of them
#'
#'
#' # For specific addresses
#' address =   list('location_name' = "Taco Bell",'street_address' = "710 3rd St", 'city'="San Francisco",'region' =  "CA", 'iso_country_code' = "US")
#' mydat = safegraph_api('MY API KEY', location= address, select = 'open_hours')
#'
#' # Or for (a subset of) POIs that match a search
#' search <- list('city' = 'San Francisco', 'brand' = 'Starbucks')
#' mydat = safegraph_api('MY API KEY', search = search, select = 'raw_visit_counts',
#'                        dataset = 'weekly_patterns', date = '2021-01-01')
#' }
#'
#' @export

safegraph_api <- function(key,
                          placekeys = NULL,
                          location = NULL,
                          search = NULL,
                          first = 10,
                          after = NULL,
                          dataset = 'core',
                          date = NULL,
                          select = NULL,
                          batch = FALSE,
                          display_call = FALSE,
                          ...) {

  if ((1*!is.null(placekeys)) + (1*!is.null(location)) + (1*!is.null(search)) != 1) {
    stop('Exactly one of placekeys, location, or search must be specified')
  }

  if (first > 1000 | first < 1) {
    stop('first must be between 1 and 1000.')
  }
  if (length(placekeys) > 1000) {
    stop('placekeys cannot have more than 1000 entries.')
  }

  if (!dataset %in% c('core','geometry','weekly_patterns','monthly_patterns','placekey')) {
    stop("dataset must be one of 'core', 'geometry','weekly_patterns', 'monthly_patterns', or 'placekey'.")
  }
  if (dataset == 'placekey' & is.null(location) & is.null(search)) {
    stop("dataset = 'placekey' can only be used in conjunction with the location or search options.")
  }

  if (dataset == 'weekly_patterns' & is.null(date)) {
    stop("If dataset = 'weekly_patterns', then date must be specified.")
  }
  if (!(dataset == 'weekly_patterns') & !is.null(date)) {
    warning("date will be ignored since dataset is not 'weekly_patterns'.")
  }

  if (!is.null(search) & first > 20 & batch == FALSE) {
    stop('You have asked for more than 20 results, but the batch = FALSE setting will not allow batching.')
  }
  if (!is.null(placekeys) & length(placekeys) > 20 & batch == FALSE) {
    stop('You have asked for more than 20 results, but the batch = FALSE setting will not allow batching.')
  }
  if (!is.null(placekeys) & dataset == 'placekey') {
    stop('Cannot use placekeys to look up placekeys.')
  }
  if (!is.null(location) & is.data.frame(location)) {
    if (nrow(location) > 1) {
      stop('If using a data.frame for location, the data.frame can have only one row.')
    }
  }

  available_vars <- list('placekey' = 'placekey',
                         'core' = c('placekey', 'latitude', 'longitude', 'street_address',
                                    'city', 'region', 'postal_code', 'iso_country_code',
                                    'parent_placekey', 'location_name', 'brands', 'brand_id',
                                    'brand_name','top_category', 'sub_category', 'naics_code',
                                    'phone_number', 'open_hours', 'category_tags', 'opened_on',
                                    'closed_on', 'tracking_closed_since', 'geometry_type'),
                         'geometry' = c('placekey', 'latitude', 'longitude', 'street_address',
                                        'city','region','postal_code','iso_country_code',
                                        'parent_placekey','location_name','brands','brand_id',
                                        'brand_name','polygon_wkt','polygon_class',
                                        'includes_parking_lot','is_synthetic','enclosed'),
                         'weekly_patterns' = c('placekey', 'parent_placekey', 'location_name',
                                        'street_address','city','region','postal_code','iso_country_code',
                                        'brands','brand_id','brand_name', 'date_range_start', 'date_range_end',
                                        'raw_visit_counts','raw_visitor_counts','visits_by_day', 'visits_by_each_hour',
                                        'poi_cbg', 'visitor_home_cbgs', 'visitor_home_aggregation', 'visitor_daytime_cbgs',
                                        'visitor_country_of_origin', 'distance_from_home', 'median_dwell',
                                        'bucketed_dwell_times','related_same_day_brand', 'related_same_week_brand'),
                         'monthly_patterns' = c('placekey', 'parent_placekey', 'location_name',
                                                'street_address', 'city', 'region', 'postal_code',
                                                'iso_country_code', 'brands', 'brand_id','brand_name',
                                                'date_range_start', 'date_range_end', 'raw_visit_counts',
                                                'raw_visitor_counts', 'visits_by_day', 'poi_cbg',
                                                'popularity_by_hour', 'visitor_home_cbgs','visitor_home_aggregation',
                                                'visitor_daytime_cbgs', 'visitor_country_of_origin', 'distance_from_home',
                                                'bucketed_dwell_times', 'median_dwell', 'related_same_day_brand',
                                                'related_same_month_brand', 'popularity_by_day', 'device_type'))

  if (is.null(select)) {
    select <- available_vars[[dataset]]
  } else {
    if (sum(!(select %in% available_vars[[dataset]])) > 0) {
      stop(paste0('The following variables are in select but are not in the dataset you selected: ',paste(select[!(select %in% available_vars[[dataset]])], collapse = ', ')))
    }
  }
  # Always get placekeys anyway, deal with them separately
  select <- select[!(select == 'placekey')]

  # Treat brands special - they're nested
  brandreqs <- c('brand_id','brand_name')[c('brand_id','brand_name') %in% select]
  if ('brands' %in% select) {
    brandreqs <- c('brand_id','brand_name')
  }
  select <- select[!(select %in% c('brands','brand_id','brand_name'))]

  #https://ropensci.org/blog/2020/12/08/accessing-graphql-in-r/

  # Build that query!
  querylist <- list()
  if (!is.null(placekeys)) {
    numcalls <- floor((length(placekeys)-1)/20)+1
    for (i in 1:numcalls) {
      thiscall <- placekeys[(1+20*(i-1)):(min(length(placekeys),1+20*(i)))]
      querylist[[i]] <- build_placekeys_query(thiscall, dataset, date, select, brandreqs)
    }
  } else if (!is.null(location)) {
    numcalls <- 1
    querylist[[1]] <- build_location_query(location, dataset, date, select, brandreqs)
  } else if (!is.null(search)) {
    # How many calls will this take
    numcalls <- floor((length(first)-1)/20)+1
    # Do 20 at a time
    if (first > 20) {
      first <- 20
    }
    for (i in 1:numcalls) {
      querylist[[i]] <- build_search_query(search, dataset, date, select, brandreqs, after, first)
      if (is.null(after)) {
        after <- 0
      }
      after <- after + 20
    }
  }

  if (numcalls > 1) {
    warning(paste0('This will take ', numcalls, ' batch calls. To avoid rate limits, safegraph_api will wait one second between each call. So this will take at least ', numcalls, ' seconds.'))
  }

  # result <- conn$exec(new$link, variables = variable) %>%
  #   fromJSON(flatten = F)

  # Build a connection
  conn <- ghql::GraphqlClient$new(url = 'https://api.safegraph.com/v2/graphql',
                                  headers = list(apikey = key))

  # Run the queries
  fulldata <- list()
  count <- 1
  for (query in querylist) {
    if (count > 1) {
      Sys.sleep(1)
    }

    if (display_call) {
      cat(query[[2]])
    }

    # Pull data
    new <- ghql::Query$new()$query('link', query[[2]])
    result <- conn$exec(new$link, variables = query[[1]])

    resproc <- jsonlite::fromJSON(result)

    # Get out the actual data
    resdata <- resproc$data
    if (!is.null(location)) {
      resdata <- resdata[[1]]
    } else {
      while (!is.data.frame(resdata)) {
        resdata <- resdata[[1]]

        if (!is.list(resdata)) {
          stop('Unable to find the data inside the returned JSON object,')
        }
      }

      if (!is.null(resdata$node)) {
        resdata <- resdata$node
      }
    }
    resdata <- data.table::as.data.table(cbind(data.frame(placekey = resdata[[1]]),resdata[[2]]))

    data.table::setnames(resdata, gsub('edges\\.node\\.','',names(resdata)))
    data.table::setnames(resdata, gsub('batch_lookup\\.','',names(resdata)))
    data.table::setnames(resdata, gsub('node\\.','',names(resdata)))

    resdata[, version_date := resproc$extensions$version_date]

    fulldata[[count]] <- resdata
    count <- count + 1
  }

  fulldata <- data.table::rbindlist(fulldata)

  return(fulldata)
}


build_placekeys_query <- function(placekeys, dataset, date = NULL, select, brandreqs) {
  variables <- list(
    placekeys = placekeys
  )

  weeklydate <- ''
  if (dataset == 'weekly_patterns') {
    weeklydate <- paste0(' (date: "',date,'")')
  }

  query <- paste0('query($placekeys: [Placekey!]) {\n',
                  '  batch_lookup(placekeys: $placekeys) {\n',
                  '    placekey\n',
                  '    safegraph_',dataset, weeklydate, ' {\n',
                  variable_request_list(select, brandreqs),
                  '    }\n',
                  '  }\n',
                  '}'
                  )

  return(list(variables, query))
}

build_location_query <- function(location, dataset, date = NULL, select, brandreqs) {
  variables <- NULL

  weeklydate <- ''
  if (dataset == 'weekly_patterns') {
    weeklydate <- paste0(' (date: "',date,'")')
  }

  if (!is.data.frame(location)) {
    location <- as.data.frame(t(location))
  } else {
    location <- as.data.frame(lapply(location, as.character))
  }

  locstring <- ''
  for (v in names(location)) {
    locstring <- paste0(locstring, '\t', v, ': "', location[[v]][1], '"\n')
  }
  locstring <- paste0('{\n',locstring,'}')


  query <- paste0('query {\n',
                  '  lookup(\n',
                  '    query: ',
                  locstring,
                  '\n  ) {\n',
                  '    placekey\n',
                  '    safegraph_',dataset, weeklydate, ' {\n',
                  variable_request_list(select, brandreqs),
                  '    }\n',
                  '  }\n',
                  '}'
  )

  return(list(variables, query))
}

# https://ropensci.org/blog/2020/12/08/accessing-graphql-in-r/
# https://docs.safegraph.com/reference/search


build_search_query <- function(search, dataset, date, select,  brandreqs, after, first) {
  address <- list()
  for (adds in c('location_name','street_address','city','region','postal_code','iso_country_code')) {
    if (!is.null(search[[adds]])) {
      address[[adds]] <- search[[adds]]
      search[[adds]] <- NULL
    }
  }

  filterstr <- ''
  if (length(address) > 0) {
    filterstr <- paste0('\t\taddress: {\n',
                        search_filter_build(address, tabs = 3),
                        '\t\t}\n')
  }
  if (length(search) > 0) {
    filterstr <- paste0(filterstr,
                        search_filter_build(search, tabs = 2))
  }

  filterstr <- paste0('\tsearch(filter: {\n',
                      filterstr,
                      '\t}) {\n')

  weeklydate <- ''
  if (dataset == 'weekly_patterns') {
    weeklydate <- paste0(' (date: "',date,'")')
  }

  if (is.null(after)) {
    resline <- paste0('results(first: ', first, ') {\n')
  } else {
    resline <- paste0('results(first: ', first, ' after: ', after, ') {\n')
  }

  query <- paste0('query {\n',
                  filterstr,
                  '\t\tplaces {\n\t\t\t',
                  resline,
                  '\t\t\t\tedges {\n',
                  '\t\t\t\t\tnode {\n',
                  '\t\t\t\t\t\tplacekey\n',
                  '\t\t\t\t\t\tsafegraph_',dataset, weeklydate, ' {\n',
                  variable_request_list(select, brandreqs),
                  '\t\t\t\t\t}\n',
                  '\t\t\t\t}\n',
                  '\t\t\t}\n',
                  '\t\t}\n',
                  '\t}\n',
                  '}}')

  return(list(NULL, query))
}



search_filter_build <- function(jlist, tabs = 2) {
  vlist <- sapply(names(jlist), function(x) paste0(paste0(rep('\t',tabs), collapse = ''),x, ': "', jlist[[x]], '"\n'))
  vlist <- stringr::str_sub(paste(vlist, collapse = ''))
  return(vlist)
}

variable_request_list <- function(select, brandreqs) {
  reg_vars <- paste0('      ',paste(select, collapse = '\n      '), '\n')

  brand_add <- ''
  if (length(brandreqs) > 0) {
    brand_add <- paste0('      brands {\n',
                        paste0('        ',
                              paste(brandreqs, collapse = '\n        ')),
                        '\n      }\n')
  }
  reg_vars <- paste0(reg_vars, brand_add)

  return(reg_vars)
}

