# SafeGraphR

**SafeGraphR** is an R package designed to make it easy to read in and process data from [SafeGraph](safegraph.com), including data that comes through the Placekey consortium or the [catalog](catalog.safegraph.io). You may want to consult the [Quick Start Guide](https://docs.google.com/document/d/1Xx-nzOX1qF3WfOpg4D8aemwFrrAkQaJuT0-1-CbgxQs/edit), the [Awesome SafeGraph Data Science List](https://github.com/SafeGraphInc/awesome-safegraph-datascience), the [Normalization Best Practices](https://colab.research.google.com/drive/16BELpcum4TKoH-5wg8Xym_CGgIGgpu1I?usp=sharing), and especially the [SafeGraph Docs](docs.safegraph.com/).

You can install **SafeGraphR** directly from GitHub.

```r
# if necessary
# install.packages('remotes')
remotes::install_github('SafeGraphInc/SafeGraphR')
```

The other pages on this site will walk you through how you can use **SafeGraphR** to work with the data.


# Bugs and Help!

**SafeGraphR** is currently in *beta*. All of its functions work, but of course there may be bugs remaining. The code has also not been checked with every possible combination of options that you could pick. Lastly, the SafeGraph data itself changes format on occasion, which may break some **SafeGraphR** functionality.

If you run into an issue or bug in the code, please raise an Issue on the **SafeGraphR** Github [Issues page](https://github.com/SafeGraphInc/SafeGraphR/issues).

If you're just having trouble getting things to work, you can find help at the [Placekey Community Slack Channel](placekey-community.slack.com/) in the *r-troubleshooting* room.

Below is a list of what's in the package with a brief description.

## Data Reading Functions

`read_core()`: Read in a Core Places file, which you can then merge with patterns or other data to add information about each location. There is also the older `link_poi_naics()` which does the same thing but can only be used to create a link between POIs and NAICS codes.

`read_distancing()`: Given a list of dates, reads in and aggregates SafeGraph social-distancing v2 files.

`read_many_csvs()`: Reads a bunch of CSVs in the same folder and row-binds them all together. Useful for stuff like normalization data.

`read_many_patterns()` and `read_patterns()`: Reads a bunch of (or one, respectively) monthly or weekly patterns `.csv.gz` files all in the same folder, does appropriate processing, and row-binds the results together.

`read_many_shop()` and `read_shop()`: Reads a bunch of (or one, respectively) `.zip` files in the format they come in from the shop and combines the data sets inside the zip across zip-files appropriately.

`safegraph_aws()`: A thin wrapper for `aws.s3::s3sync()` that downloads data from the SafeGraph AWS buckets.


## Data Processing Functions

`expand_cat_json()` and `expand_integer_json()`: Take SafeGraph data with a column of categorical (named) or numeric (unnamed) JSON data and expand that column, pivot the data to long format, and then aggregate to the desired level.

`expand_open_hours()`: Expand the `open_hours` variable into something easy to use!

`fips_from_cbg()`: Take a census block group identifier and extract the state and/or county FIPS codes.

`rbind_by_list_pos()`: Take a list of lists of `data.table`s and row-binds them by their position in the sub-list. For example, `rbind_by_list_pos(list(A,B),list(C,D))` would return `list(rbind(A,C),rbind(B,D))`. Can be used after `read_` functions, which in some cases return a list of `data.table`s for each file they read.

## Final-Stages Processing Functions

`hb_shrink()`: Perform hierarchical Bayesian shrinkage on the CBG-to-county or county-to-state level.

`ma()`: Calculates a (by default) seven day moving average on pre-sorted data with no gaps.

`sample_size_adjust()`: Adjusts data for differences in sampling rates across geographic locations.

`scale_to_date()`: Adjusts data to be relative to a specific date.

`scale_yoy()`: Adjusts data to be relative to the same date the previous year.

## Data Sets

`cbg_pop`: Population data from the easy census file.

`county_pop`: Population aggregated to the county level.

`fips_to_names`: Data set linking state and county FIPS codes to state and county names, for merging in and labeling.

`naics_codes`: Data set linking NAICS codes to NAICS code titles, for merging in and labeling (or just knowing what you're looking at).
