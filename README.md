# SafeGraphR
Package for reading and analyzing SafeGraph foot traffic data

This package is now in BETA. Everything should be working, so please give it a shot and report any errors!

Below is a list of what's in the package with a brief description.

## Data Reading Functions

`link_poi_naics`: Read in a Core Places file and use it to create a crosswalk between SafeGraph POI codes and NAICS codes.

`read_distancing`: Given a list of dates, reads in and aggregates SafeGraph social-distancing v2 files.

`read_many_csvs`: Reads a bunch of CSVs in the same folder and row-binds them all together. Useful for stuff like normalization data.

`read_many_patterns` and `read_patterns`: Reads a bunch of (or one, respectively) monthly or weekly patterns `.csv.gz` files all in the same folder, does appropriate processing, and row-binds the results together.

`read_many_shop` and `read_shop`: Reads a bunch of (or one, respectively) `.zip` files in the format they come in from the shop and combines the data sets inside the zip across zip-files appropriately.

`safegraph_aws`: A thin wrapper for `aws.s3::s3sync` that downloads data from the SafeGraph AWS COVID response buckets.


## Data Processing Functions

`expand_cat_json` and `expand_integer_json`: Take SafeGraph data with a column of categorical (named) or numeric (unnamed) JSON data and expand that column, pivot the data to long format, and then aggregate to the desired level.

`fips_from_cbg`: Take a census block group identifier and extract the state and/or county FIPS codes.

`rbind_by_list_pos`: Take a list of lists of `data.table`s and row-binds them by their position in the sub-list. For example, `rbind_by_list_pos(list(A,B),list(C,D))` would return `list(rbind(A,C),rbind(B,D))`. Can be used after `read_` functions, which in some cases return a list of `data.table`s for each file they read.

## Final-Stages Processing Functions

`hb_shrink`: Perform hierarchical Bayesian shrinkage on the CBG-to-county or county-to-state level.

`ma`: Calculates a (by default) seven day moving average on pre-sorted data with no gaps.

`sample_size_adjust`: Adjusts data for differences in sampling rates across geographic locations.

`scale_to_date`: Adjusts data to be relative to a specific date.

`scale_yoy`: Adjusts data to be relative to the same date the previous year.

## Data Sets

`cbg_pop`: Population data from the easy census file.

`county_pop`: Population aggregated to the county level.

`fips_to_names`: Data set linking state and county FIPS codes to state and county names, for merging in and labeling.

`naics_codes`: Data set linking NAICS codes to NAICS code titles, for merging in and labeling (or just knowing what you're looking at).
