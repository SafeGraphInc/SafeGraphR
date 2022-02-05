#' Produce a nice-looking graph of foot traffic growth over time
#'
#' Produces a line graph with labels at the end of the lines, with theming designed for the purpose. Returns a \code{ggplot} object that can be further modified as normal. Requires that the **ggplot2**, **ggrepel**, and **paletteer** packages be installed.
#'
#' @param dt A \code{data.table} (or something that can be coerced to \code{data.table}). There must be one observation per \code{date} per \code{by} in this data.
#' @param date Character variable indicating the date variable (x axis).
#' @param growth Character variable indicating the growth variable (y axis).
#' @param origin The value indicating no growth/initial value.
#' @param filter A character variable describing a subset of the data to include, for example \code{filter = 'state_fips == 6'} to only include California.
#' @param by A character variable of the variable name to assign to the \code{color} aesthetic in \code{ggplot2::geom_line()}. The values of this variable will also be sent to \code{ggrepel::geom_text_repel()}.
#' @param x_title Axis title for x-axis.
#' @param y_title Axis title for y-axis.
#' @param title Graph title.
#' @param caption Figure caption.
#' @param subtitle Graph subtitle.
#' @param label Should a text label be applied at the end of each line?
#' @param hline Should a horizontal line at the \code{origin} value be included?
#' @param expand_right Number of additional days to extend the x-axis by so as to fit the labels. Defaults to adding 33 percent more days so a quarter of the graph is reserved for labels.
#' @param palette Discrete color palette from the **paletteer** package to be sent to \code{paletteer::scale_color_paletteer_d()}. If you like, the default **ggplot2** color theme is \code{'basetheme::default'}.
#' @param manual_palette Manually-specified color palette to be sent to the \code{values} option of \code{ggplot2::scale_color_manual()}.
#' @param skip_theming Don't apply the template theming, so you can apply your own.
#' @param line_opts A named list of options to be sent to \code{ggplot2::geom_line()}.
#' @param label_opts A named list of options to be sent to \code{ggrepel::geom_text_repel()}. Only relevant if \code{label = TRUE}.
#' @param hline_opts A named list of options to be sent to \code{ggplot2::geom_hline()}, only relevant if \code{hline = TRUE}.
#' @examples
#'
#' # Generally you'd be doing this with data that comes from read_many_patterns()
#' # But here's an example using randomly generated data
#'
#' dt <- data.table::data.table(date = rep(lubridate::ymd('2020-01-01') + lubridate::days(0:300),2),
#'                              state_fips = c(rep(6, 301), rep(7,301)),
#'                              visits_by_day = rpois(602, lambda = 10))
#'
#' norm <- data.table::data.table(date = rep(lubridate::ymd('2020-01-01') + lubridate::days(0:300),2),
#'                                state_fips = c(rep(6, 301), rep(7,301)),
#'                                total_devices_seen = rpois(602, lambda = 10000))
#'
#' processed_data <- processing_template(dt, norm = norm, by = 'state_fips')
#'
#' p <- graph_template(processed_data, by = 'state_fips')
#'
#' p
#'
#' @export

graph_template <- function(dt,
                           date = 'date',
                           growth = 'growth',
                           origin = 0,
                           filter = NULL,
                           by = NULL,
                           x_title = 'Date',
                           y_title = 'Foot Traffic Growth',
                           title = ifelse(is.null(by), 'SafeGraph: Foot Traffic Growth',
                                          paste0('SafeGraph: Foot Traffic Growth by ',paste(by, collapse = ', '))),
                           caption = '7-day moving average applied.',
                           subtitle = NULL,
                           label = !is.null(by),
                           hline = TRUE,
                           expand_right = NULL,
                           palette = 'ggsci::category20_d3',
                           manual_palette = NULL,
                           skip_theming = FALSE,
                           line_opts = list(size = 1),
                           label_opts = list(size = 14/ggplot2::.pt,
                                             hjust = -.2,
                                             vjust = .5,
                                             direction = 'y'),
                           hline_opts = list(size = .5,
                                             linetype = 'dashed',
                                             color = 'black')) {

  if (length(by) > 1) {
    stop('Only one variable can be used in by for graph_template.')
  }

  # Avoid manipulating top-level data.table
  dt <- data.table::copy(data.table::as.data.table(dt))

  if((dt[, c(date, by), with = FALSE] %>%
    duplicated() %>%
    max()) == 1) {
    stop('There must be one row per date per by.')
  }

  if (!is.null(filter)) {
    dt <- dt[eval(parse(text = filter))]
  }

  # Create label only on the last day
  check_max_date_text <- paste0(
    'data.table::fifelse(', date, ' == max(', date, '), as.character(',
    by,
    '), "")'
  )
  dt[, bylabel := eval(parse(text = check_max_date_text)), by = by]

  # Ensure by is a factor
  if (!is.factor(dt[[by]])) {
    factext <- paste0(by, ' := as.factor(', by, ')')
    dt[, eval(parse(text = factext))]
  }

  p <- ggplot2::ggplot(dt,
                  ggplot2::aes_string(x = date, y = growth,
                                      color = by,
                                      label = 'bylabel'))

  if (hline) {
    p <- p +
      do.call(ggplot2::geom_hline, append(hline_opts, list(mapping = ggplot2::aes(yintercept = eval(origin)))))
  }

  p <- p +
    do.call(ggplot2::geom_line, line_opts)

  if (label) {
    p <- p +
      do.call(ggrepel::geom_text_repel, label_opts)

    if (is.null(expand_right)) {
      expand_right <- ceiling(length(unique(dt[[date]]))/3)
    }
    p <- p +
      ggplot2::scale_x_date(limits = c(min(dt[[date]]),
                                             max(dt[[date]]) + lubridate::days(expand_right)))
  }


  if (is.null(manual_palette)) {
    p <- p +
      paletteer::scale_color_paletteer_d(palette)
  } else {
    p <- p +
      ggplot2::scale_color_manual(values = manual_palette)
  }

  p <- p +
    ggplot2::labs(x = x_title,
                  y = y_title,
                  title = title,
                  caption = caption,
                  subtitle = subtitle)

  # Theming!
  if (!skip_theming) {
    p <- p +
      ggplot2::guides(color = FALSE) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     axis.line.x.top = ggplot2::element_blank(),
                     axis.line.y.right = ggplot2::element_blank(),
                     axis.line.x.bottom = ggplot2::element_line(size = 1),
                     axis.line.y.left = ggplot2::element_line(size = 1),
                     axis.text = ggplot2::element_text(size = 13),
                     axis.title = ggplot2::element_text(size = 14, hjust = 1))
  }

  return(p)
}

#' Perform basic processing and preparation of visits_by_day data
#'
#' This function takes data read in from SafeGraph patterns files that has had \code{expand_integer_json()} already applied to its \code{visits_by_day} variable (or used the \code{expand_int = 'visits_by_day'} option in \code{read_patterns()} or \code{read_many_patterns()}). It aggregates the data to the \code{date-by} level, normalizes according to the size of the sample, calculates a moving average, and also calculates growth since the \code{start_date} for each \code{by} category. The resulting \code{data.table}, with one row per \code{date} per combination of \code{by}, can be used for results and insight, or passed to \code{graph_template()} for a quick graph.
#'
#' The result is the same \code{data.table} that was passed in, with some modifications: the data will be aggregated (using \code{sum}) to the \code{date-by} level, with \code{visits_by_day} as the only other surviving column. Three new columns are added: The normalization variable (from \code{norm}, or just a variable \code{norm} equal to 1 if \code{norm = NULL}), \code{adj_visits}, which is \code{visits_by_day} adjusted for sample size and with a moving average applied, and \code{growth} which tracks the percentage change relative to the earliest value of \code{adj_visits} that is not missing.
#'
#' @param dt A \code{data.table} (or something that can be coerced to \code{data.table}).
#' @param norm A \code{data.table} containing columns for \code{date}, any number of the elements of \code{by}, and a final column containing a normalization factor. The \code{visits_by_day} values will be divided by that normalization factor after merging. \code{growth_over_time} will generate this internally for you, but you can make (a standard version of it) easily by just using \code{read_many_csvs(makedate = TRUE)} to load in all of the files in the \code{normalization_stats} or \code{normalization_stats_backfill} folders from AWS, limiting it to just the all-state rows, and then passing in just the \code{date} and \code{total_devices_seen} columns. If null, applies no normalization (if your analysis covers a reasonably long time span, you want normalization).
#' @param by A character vector of the variable names that indicate groups to calculate growth separately by.
#' @param date Character variable indicating the date variable.
#' @param visits_by_day Character variable indicating the variable containing the \code{visits_by_day} numbers.
#' @param origin The value indicating no growth/initial value. The first date for each group will have this value. Usually 0 (for "0 percent growth") or 1 ("100 percent of initial value").
#' @param filter A character variable describing a subset of the data to include, for example \code{filter = 'state_fips == 6'} to only include California.
#' @param single_by A character variable for the name of a new variable that combines all the different variables in \code{by} into one variable, handy for passing to \code{graph_template()}.
#' @param ma Number of days over which to take the moving average.
#' @param drop_ma Drop observations for which \code{adj_visits} is missing because of the moving-average adjustment.
#' @param first_date After implementing the moving-average, drop all values before this date and calculate growth starting from this date. If \code{NULL}, uses the first date that's not missing after the moving average.
#' @param silent Omit the warning and detailed report that occurs for values of \code{dt} that find no match in \code{norm}, as well as the one if you try not to normalize at all.
#' @examples
#'
#' # Generally you'd be doing this with data that comes from read_many_patterns()
#' # But here's an example using randomly generated data
#'
#' dt <- data.table::data.table(date = rep(lubridate::ymd('2020-01-01') + lubridate::days(0:300),2),
#' state_fips = c(rep(6, 301), rep(7,301)),
#' visits_by_day = rpois(602, lambda = 10))
#'
#' norm <- data.table::data.table(date = rep(lubridate::ymd('2020-01-01') + lubridate::days(0:300),2),
#'                                state_fips = c(rep(6, 301), rep(7,301)),
#'                                total_devices_seen = rpois(602, lambda = 10000))
#'
#' processed_data <- processing_template(dt, norm = norm, by = 'state_fips')
#'
#' @export


processing_template <- function(dt,
                                norm = NULL,
                                by = NULL,
                                date = 'date',
                                visits_by_day = 'visits_by_day',
                                origin = 0,
                                filter = NULL,
                                single_by = NULL,
                                ma = 7,
                                drop_ma = TRUE,
                                first_date = NULL,
                                silent = FALSE) {
  dt <- data.table::as.data.table(dt)

  if (!is.numeric(dt[[visits_by_day]])) {
    stop('The visits_by_day variable must be numeric. Did you try to pass in raw CSV data? Be sure to apply expand_integer_json first.')
  }
  if (is.null(norm) & !silent) {
    warning('Proceeding without normalizing based on the sample size. This is not advised, although you\'ll probably be fine if you\'re only covering a short time period.')
  }

  # filter
  if (!is.null(filter)) {
    dt <- dt[eval(parse(text = filter))]
  }

  if (sum(is.na(dt[[visits_by_day]])) > 0) {
    warning('There are missing values in the visits_by_day variable. Dropping these observations. This may lead to moving-average values being calculated incorrectly.')
    droptext <- paste0('!is.na(', visits_by_day, ')')
    dt <- dt[eval(parse(text = droptext))]
  }

  # re-aggregate
  # Switch to double for later ma stuff
  sumtext <- paste0('list(',
                    visits_by_day,
                    ' = as.double(sum(',
                    visits_by_day,
                    ', na.rm = TRUE)))')
  bytext <- paste0('list(', paste(c(date,by), collapse = ', '), ')')
  dt <- dt[, eval(parse(text = sumtext)), by = eval(parse(text = bytext))]

  # Bring in normalization
  if (is.null(norm)) {
    norm <- data.table::data.table(date = unique(dt[[date]]), norm = 1)
  }
  normname <- names(norm)[length(names(norm))]

  # Drop any variables in norm that aren't used for merging or normalizing
  keepnorm <- names(norm)[names(norm) %in% c(date, by, normname)]

  dt <- merge(dt, norm, by = keepnorm[1:(length(keepnorm) - 1)], all.x = TRUE)

  # Check for missing matches
  if (sum(is.na(dt[[normname]])) > 0) {
    misstext <- paste0('is.na(', normname, ')')
    dtmiss <- dt[eval(parse(text = misstext))]

    if (!silent) {
      warning('Some observations not matched to data in norm, and will be dropped. This may lead moving-average values to be calculated incorrectly. Set silent = TRUE to suppress this message.')
      warning('The following merging-variable values could not find matches in norm:')
      print(unique(dtmiss[, keepnorm[1:(length(keepnorm) - 1)], with = FALSE]))
    }

    misstext <- paste0('!is.na(', normname, ')')
    dt <- dt[eval(parse(text = misstext))]
  }

  # Create adj_visits
  adjtext <- paste0('adj_visits := ',
                    visits_by_day,
                    '/',
                    normname)
  dt[, eval(parse(text = adjtext))]

  # And do moving average
  data.table::setorderv(dt, c(by, date))
  dt[, adj_visits := SafeGraphR::ma(adj_visits, n = ma), by = by]

  # And growth
  # Split out the new NAs
  if (is.null(first_date)) {
    first_date <- as.Date('1900-01-01', origin = '1970-01-01')
  }

  droptext <- paste0('is.na(adj_visits) & ', date, ' >= first_date')
  newnas <- dt[eval(parse(text = droptext))]

  droptext <- paste0('!', droptext)
  dt <- dt[eval(parse(text = droptext))]
  dt[, growth := adj_visits/data.table::first(adj_visits) + (origin - 1), by = by]

  # Bring 'em back in
  if (!drop_ma) {
    dt <- rbind(dt, newnas)
  }

  data.table::setorderv(dt, c(by, date))

  if (!is.null(single_by) & length(by) > 0) {
    bypaste <- paste0('(single_by) := paste(', paste(by, collapse = ', '), ', sep = ", ")')
    dt[, eval(parse(text = bypaste))]
  }

  return(dt)
}


#' Track foot traffic by group over time
#'
#' A start-to-finish download and analysis! This function, given a range of dates, a subset of data, and a grouping set, will produce an estimate of how foot traffic to those groups has changed over that date range within that subset.
#'
#' This goes from start to finish, downloading any necessary files from AWS, reading them in and processing them, normalizing the data by sample size, calculating a moving average, and returning the processed data by group and date. It will even make you a nice graph if you want using \code{graph_template}.
#'
#' Returns a \code{data.table} with all the variables in \code{by}, the \code{date}, the raw \code{visits_by_day}, the \code{total_devices_seen} normalization variable, the \code{adj_visits} variable adjusted for sample size, and \code{growth_visits}, which calculates growth from the start of the \code{dates} range. If \code{make_graph} is \code{TRUE}, will instead return a list where the first element is that \code{data.table}, and the second is a \code{ggplot} graph object.
#'
#' Be aware:
#'
#'  1. This will only work with the \code{visits_by_day} variable. Or at least it's only designed to. Maybe you can get it to work with something else.
#'
#'  2. This uses \code{processing_template}, so all the caveats of that function apply here. No attempt will be made to handle outliers, oddities in the data, etc.. You get what you get. If you want anything more complex, you'll have to do it by hand! You might try mining this function's source code (just do \code{foot_traffic_growth} in the console) to get started.
#'
#'  3. Each week of included data means a roughly 1GB AWS download unless it's already on your system. Please don't ask for more than you need, and if you have already downloaded the data, please input the directory properly to avoid re-downloading.
#'
#'  4. This requires data to be downloaded from AWS, and will not work on Shop data. See \code{read_many_shop} followed by \code{processing_template} for that.
#'
#'  5. Be aware that very long time frames, for example crossing multiple years, will always be just a little suspect for this. The sample changed structure considerably from 2019 to 2020. Usually this is handled by normalization by year and then calculation of YOY change on top of that. This function doesn't do that, but you could take its output and do that yourself if you wanted.
#'
#'  TO BE ADDED SOON: Sample size adjustments to equalize sampling rates, and labeling.
#'
#' @param dates The range of dates to cover in analysis. Note that (1) analysis will track growth relative to the first date listed here, and (2) if additional, earlier dates are necessary for the \code{ma} moving-average, they will be added automatically, don't do it yourself.
#' @param by A character vector of variable names to calculate growth separately by. You will get back a data set with one observation per date in \code{dates} per combination of variables in \code{by}. Set to \code{NULL} to aggregate all traffic by \code{date} (within the \code{filter}). See the variable names in the [patterns documentation](http://docs.safegraph.com), and in addition you may use \code{state_fips} and/or \code{county_fips} for state and county FIPS codes.
#' @param ma Number of days over which to take the moving average.
#' @param dir The folder where the \code{patterns_backfill}/\code{patterns} folders of patterns data, as well as \code{normalization_stats}/\code{normalization_stats_backfill} are stored. This is also where any files that need to be downloaded from AWS will be stored.
#' @param old_dir Where "old" (pre-December 7, 2020) files go, if not the same as \code{dir}. This should be the folder that contains the \code{patterns_backfill} and the \code{normalization_stats_backfill} folder.
#' @param new_dir Where "new" (post-December 7, 2020) files go, if not the same as \code{dir}. This should be the folder that contains the \code{patterns} and the \code{normalization_stats} folder.
#' @param filelist If your data is not structured as downloaded from AWS, use this option to pass a vector of (full) filenames for patterns CSV.GZ data instead of looking in \code{dir} or on AWS. These will not be checked for date ranges until after opening them all, so be extra sure you have everything you need!
#' @param filelist_norm If your data is not structured as downloaded from AWS, use this option to pass a vector of (full) filenames for normalization CSV data instead of looking in \code{dir} or on AWS. These will not be checked for date ranges until after opening them all, so be extra sure you have everything you need!
#' @param start_dates If using the \code{filelist} argument, provide a vector of the first date present in each file. This should be the same length as \code{filelist}.
#' @param filter A character variable describing a subset of the data to include, for example \code{filter = 'state_fips == 6'} to only include California, or \code{brands == 'McDonald\'s'} to only include McDonald's. See the variable names in the [patterns documentation](http://docs.safegraph.com), and in addition you may use \code{state_fips} and/or \code{county_fips} for state and county FIPS codes.
#' @param naics_link Necessary only to \code{filter} or \code{by} on a NAICS code. A \code{data.table}, possibly produced by \code{link_poi_naics}, that links \code{placekey} and \code{naics_code}. This will allow you to include \code{'naics_code'} in the \code{by} argument. Technically you could have stuff other than \code{naics_code} in here and use that in \code{by} too, I won't stop ya.
#' @param origin The value indicating no growth/initial value. The first date for each group will have this value. Usually 0 (for "0 percent growth") or 1 ("100 percent of initial value").
#' @param key A character string containing an AWS Access Key ID, necessary if your range of dates extends beyond the files in \code{dir}.
#' @param secret A character string containing an AWS Secret Access Key, necessary if your range of dates extends beyond the files in \code{dir}.
#' @param make_graph Set to TRUE to produce (and return) a nicely-formatted graph showing growth over time with separate lines for each \code{by} group. If \code{by} produces more than, roughly, six combinations, then this won't look very good and you should also specify \code{graph_by}. Requires that **ggplot2** and **ggrepel** be installed. If this is \code{TRUE}, then instead of returning a \code{data.table}, will return a \code{list} where the first element is the normal \code{data.table}, and the second is the \code{ggplot} object.
#' @param graph_by A character vector, which must be a subset of \code{by}. Will produce a separate graph for each combination of \code{graph_by}, graphing separate lines on each for the remaining elements of \code{by} that aren't in \code{graph_by}. Now, the second element of the returned \code{list} will itself be a list containing each of the different graphs as elements, and no graph will be automatically printed. Only relevant if \code{make_graph = TRUE}.
#' @param line_labels A \code{data.table} (or object like a \code{data.frame} that can be coerced to a \code{data.table}). Contains columns for all the variables that are in \code{by} but not \code{graph_by}. Those columns should uniquely identify the rows. Contains exactly one other column, which is the label that will be put on the graph lines. For example, \code{data(naics_codes)} would work as an argument if \code{by = naics_code}. If \code{line_labels} is specified, any combination of \code{by}-but-not-\code{graph_by} values that is not present in \code{line_labels} will be dropped.
#' @param graph_by_titles A \code{data.table} (or object like a \code{data.frame} that can be coerced to a \code{data.table}). Contains columns for all the variables that are in \code{graph_by}. Those columns should uniquely identify the rows. Contains exactly one other column, which is the label that will be put in each graph's subtitle. If \code{graph_by_titles} is specified, any combination of \code{graph_by} values that are not in \code{graph_by_titles} will be dropped.
#' @param test_run Runs your analysis for only the first week of data, just to make sure it looks like you want. \code{TRUE} by default because this is a slow, data-hungry, and (if you haven't already downloaded the files) bandwidth-hungry command, and you should only run the full thing after being sure it's right!
#' @param read_opts A named list of options to be sent to \code{read_many_patterns}. Be careful using as there may be conflicts with options implied by other parameters. Including a \code{select} option in here will likely speed up the function considerably.
#' @param processing_opts A named list of options to be sent to \code{processing_template}. Be careful using as there may be conflicts with options implied by other parameters.
#' @param graph_opts A named list of options to be sent to \code{graph_template}.
#' @param patterns_backfill_date Character variable with the folder structure for the most recent \code{patterns_backfill} pull. i.e., the 2018, 2019, and 2020 folders containing backfill data in their subfolders should set in the \code{paste0(old_dir,'/patterns_backfill/',patterns_backfill_date)} folder.
#' @param norm_backfill_date A character string containing the series of dates that fills the X in \code{normalization_stats_backfill/X/} and in which the \code{2018}, \code{2019}, and \code{2020} folders sit.
#' @param ... Parameters to be passed on to \code{patterns_lookup()} (and, often, from there on to \code{safegraph_aws()}.)
#' @examples
#'
#'  \dontrun{
#'  data(state_info)
#'
#'  p <- growth_over_time(lubridate::ymd('2020-12-07') + lubridate::days(0:6),
#'                        by = c('region', 'brands'),
#'                        filter = 'brands %in% c("Macy\'s", "Target")',
#'                        make_graph = TRUE,
#'                        graph_by = 'region',
#'                        graph_by_titles = state_info[, .(region, statename)],
#'                        test_run = FALSE)
#'
#'  # The growth data overall for the growth of Target and Macy's in this week
#'  p[[1]]
#'
#'  # Look at the graph for the growth of Target and Macy's in this week in Colorado
#'  p[[2]][[6]]
#'  }
#'
#' @export

growth_over_time <- function(dates, by,
                             ma = 7,
                             dir = '.',
                             old_dir = NULL,
                             new_dir = NULL,
                             filelist = NULL,
                             filelist_norm = NULL,
                             start_dates = NULL,
                             filter = NULL,
                             naics_link = NULL,
                             origin = 0,
                             key = NULL,
                             secret = NULL,
                             make_graph = FALSE,
                             graph_by = NULL,
                             line_labels = NULL,
                             graph_by_titles = NULL,
                             test_run = TRUE,
                             read_opts = NULL,
                             processing_opts = NULL,
                             graph_opts = list(title =
                                                 data.table::fcase(
                                                   is.null(graph_by) & is.null(by), 'SafeGraph Foot Traffic Growth',
                                                   is.null(graph_by), paste('SafeGraph Foot Traffic Growth by', paste(by, collapse = ', ')),
                                                   min(by %in% graph_by) == 1, 'SafeGraph Foot Traffic Growth',
                                                   default = paste('SafeGraph Foot Traffic Growth by', paste(by[!(by %in% graph_by)], collapse = ', '))
                                                 )),
                             patterns_backfill_date = '2020/12/14/21/',
                             norm_backfill_date = '2020/12/14/21/',...) {

  if (test_run) {
    message('Running as a test run with one week of data. Surprised to see this message? Should have read the docs! This warning is in there because this function is slow and moves a lot of data, so be sure it\'s right before running.')
    resp <- readline(prompt = 'Continue anyway? Enter 1 or y or Y to continue, anything else to quit.')
    if (!(resp %in% c('1','y','Y'))) {
      stop('Exiting now.')
    }
    dates <- dates[1:7]
  }

  # Error checking
  if (is.null(filelist) != is.null(filelist_norm)) {
    stop('filelist and filelist_norm must either both be specified, or neither.')
  }
  if (!is.null(filelist)) {
    if (length(filelist) != length(start_dates)) {
      stop('If filelist is specified, start_dates must be a vector of the same length as filelist.')
    }
  }
  if (sum(!(graph_by %in% by)) > 0) {
    stop('graph_by must be a subset of by')
  }
  if (!is.null(graph_by_titles)) {
    graph_by_titles <- data.table::as.data.table(graph_by_titles)

    if (ncol(graph_by_titles) != length(graph_by) + 1) {
      stop('There must be exactly one variable in graph_by_titles that is not in graph_by.')
    }

    if (anyDuplicated(subset(graph_by_titles, select = graph_by))) {
      stop('The graph_by variables must uniquely identify the rows in graph_by_titles.')
    }
  }
  if (!is.null(line_labels)) {
    line_labels <- data.table::as.data.table(line_labels)

    b_sub <- by[!(by %in% graph_by)]

    if (length(b_sub) == 0) {
      stop('There must be at least one variable in by but not graph_by to specify line_labels.')
    }

    if (ncol(line_labels) != length(b_sub) + 1) {
      stop('There must be exactly one column in line_labels other than the ones that match the by-but-not-graph_by columns.')
    }

    if (anyDuplicated(subset(line_labels, select = b_sub))) {
      stop('The variables in by but not graph_by must uniquely identify the rows in line_labels.')
    }
  }

  # Make sure dir ends in a /
  if (stringr::str_sub(dir, -1) != '/') {
    dir <- paste0(dir, '/')
  }
  if (stringr::str_sub(norm_backfill_date, -1) != '/') {
    norm_backfill_date <- paste0(norm_backfill_date, '/')
  }

  if (is.null(old_dir)) {
    old_dir <- dir
  }
  if (is.null(new_dir)) {
    new_dir <- dir
  }

  # Add dates for ma
  dates2 <- dates

  for (d in 1:ma) {
    dates2 <- unique(c(dates2, dates - lubridate::days(d)))
  }

  dates <- sort(dates2)

  # Get our list of files
  if (is.null(filelist)) {
    olddates <- dates[dates <= lubridate::ymd('2020-12-06')]
    newdates <- dates[dates >= lubridate::ymd('2020-12-07')]

    # First the old!
    oldlist <- paste0(dir,patterns_lookup(olddates, ...))
    oldnorm <- stringr::str_replace_all(oldlist, 'patterns_backfill','normalization_stats_backfill')

    # And the new
    newlist <- paste0(dir, patterns_lookup(newdates, ...))
    newnorm <- stringr::str_replace_all(newlist, 'patterns', 'normalization_stats')

    # If there's no AWS access, make sure all files exist
    # We must check recursively!
    new_patterns_miss <- 0

    oldmiss <- c()
    for (n in oldlist) {
      found <- list.files(n, recursive = TRUE, pattern = 'patterns.*.csv.gz')
      if (length(found) < 4) {
        oldmiss <- c(oldmiss, n)
        new_patterns_miss <- new_patterns_miss + 1
      }
    }
    for (n in oldnorm) {
      found <- list.files(n, recursive = TRUE, pattern = 'normalization_stats.csv')
      if (length(found) < 1) {
        oldmiss <- c(oldmiss, n)
      }
    }

    # For new, we must check recursively!
    newmiss <- c()
    for (n in newlist) {
      found <- list.files(n, recursive = TRUE, pattern = 'patterns.*.csv.gz')
      if (length(found) < 4) {
        newmiss <- c(newmiss, n)
        new_patterns_miss <- new_patterns_miss + 1
      }
    }
    for (n in newnorm) {
      found <- list.files(n, recursive = TRUE, pattern = 'normalization_stats.csv')
      if (length(found) < 1) {
        newmiss <- c(newmiss, n)
      }
    }

    if (length(oldmiss) + length(newmiss) > 0) {
      if (is.null(key) | is.null(secret)) {
        message('AWS key and/or secret missing, which requires all necessary files to be downloaded already. ')
        message(paste0(c('\nThe following folders do not have the appropriate files in their subfolders:',
                           oldmiss, newmiss),
                         collapse = '\n'))
        stop('\nExiting now. Either provide key and secret, or download these files to the appropriate locations.')
      } else {
        oldmiss <- stringr::str_sub(oldmiss, nchar(old_dir) + 1)
        newmiss <- stringr::str_sub(newmiss, nchar(new_dir) + 1)

        if (new_patterns_miss > 11) {
          message('This will download more than 3GB of data from AWS. Do you want to proceed?')
          resp <- readline(prompt = 'Enter 1 or y or Y to continue, anything else to quit.')
          if (!(resp %in% c('1','y','Y'))) {
            stop('Exiting now.')
          }
        }

        for (f in oldmiss) {
          dirpath <- paste0(old_dir, f)
          if (!dir.exists(dirpath)) { dir.create(dirpath, recursive = TRUE) }

          safegraph_aws(dirpath,
                        'weekly-backfill',
                        key = key,
                        secret = secret,
                        prefix = f)
        }
        for (f in newmiss) {
          dirpath <- paste0(new_dir, f)
          if (!dir.exists(dirpath)) { dir.create(dirpath, recursive = TRUE) }

          safegraph_aws(dirpath,
                        'weekly-new',
                        key = key,
                        secret = secret,
                        prefix = f)
        }
      }
    }

    filelist <- c()
    start_dates <- c()
    for (f in c(oldlist,newlist)) {
      addf <- list.files(f, pattern = '.csv.gz', recursive = TRUE, full.names = TRUE)
      filelist <- c(filelist, addf)

      thisdate <- as.Date(find_date(f), origin = as.Date('1970-01-01'))
      if (stringr::str_detect(f, 'patterns/')) {
        thisdate <- thisdate - lubridate::days(9)
      }

      start_dates <-as.Date(c(start_dates, rep(thisdate, length(addf))), origin = as.Date('1970-01-01'))
    }

    if (is.null(filelist_norm)) {
      filelist_norm <- c()
      for (f in c(oldnorm,newnorm)) {
        filelist_norm <- c(filelist_norm, list.files(f, pattern = '.csv', recursive = TRUE, full.names = TRUE))
      }
    }
  } else {
    # Check if all files exist
    if (min(file.exists(filelist)) == 0) {
      stop(paste0(
        c('The following files in filelist not found: ',
          filelist[!file.exists(filelist)]),
        collapse = '\n'))
    }
  }

  # Read in norms
  norm <- read_many_csvs(filelist = filelist_norm,
                         makedate = TRUE)
  norm <- norm[is.na(region) | region == 'ALL_STATES',
               .(date, total_devices_seen)]

  # Read in downloaded files
  dt <- do.call(read_many_patterns,
                append(list(filelist = filelist,
                            by = by,
                            filter = filter,
                            naics_link = naics_link,
                            expand_int = 'visits_by_day',
                            start_date = start_dates), read_opts))


  # Process
  dt <- do.call(processing_template,
                append(list(dt = dt,
                            by = by,
                            norm = norm,
                            filter = filter,
                            single_by = 'group'),
                       processing_opts))

  # Graph!!
  if (make_graph) {
    # Bring in line labels and override group
    by_sub <- by[!(by %in% graph_by)]
    if (!is.null(line_labels)) {
      labelname <- names(line_labels)[!(names(line_labels) %in% by_sub)]

      data.table::setnames(line_labels, labelname, 'group')

      dt[, group := NULL]

      dt <- merge(dt, line_labels)
    }

    # If no graph_by, easy
    if (is.null(graph_by)) {
      p <- do.call(graph_template,
                   append(list(dt = dt,
                               by = 'group',
                               caption = paste0(ma, '-day moving average applied.')),
                          graph_opts))
    } else {
      # otherwise!

      # Get the list of by's that don't include graph_by

      if (length(by_sub) == 0) {
        by_sub <- NULL
      } else {
        if (is.null(line_labels)) {
          ptext <- dt[[by_sub[1]]]
          if (length(by_sub) > 1) {
            for (i in 2:length(by_sub)) {
              ptext <- paste(ptext, dt[[by_sub[i]]], sep = ', ')
            }
          }
          dt[, group := ptext]
        }
      }


      p <- list()

      # Get list of existing combinations to loop through
      data.table::setorderv(dt, graph_by)
      data.table::setkeyv(dt, graph_by)
      looper <- unique(dt[, graph_by, with = FALSE])

      # Bring in explicit labels if given
      if (!is.null(graph_by_titles)) {
        looper <- merge(looper, graph_by_titles, by = graph_by)
        titlename <- names(graph_by_titles)[!(names(graph_by_titles) %in% graph_by)]
      }

      data.table::setkeyv(looper, graph_by)

      for (i in 1:nrow(looper)) {
        # Find all the values corresponding to this row of looper
        dtsub <- merge(dt, looper[i], by = graph_by)

        subt <- data.table::fifelse(is.null(graph_by_titles),
                                    paste(looper[i], collapse = ', '),
                                    looper[[titlename]][i])

        p[[i]] <- do.call(graph_template,
                          append(list(dt = dtsub,
                                      by = 'group',
                                      subtitle = paste0('Data from ', subt),
                                      caption = paste0(ma, '-day moving average applied.')),
                                 graph_opts))
      }
    }

    return(list(dt, p))
  } else {
    return(dt)
  }
}
