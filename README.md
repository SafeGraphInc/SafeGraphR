# SafeGraphR
Package for reading and analyzing SafeGraph foot traffic data

This package is under development.

It is fully featured but currently untested (some of the **BABY** functions I haven't even fully tried), and lacks examples in the documentation, or vignettes.

Please give it a shot and report any errors!

Below is a list of what's in the package with a brief description, and a testing status for if you'd like to help test.

## Data Reading Functions

`link_poi_naics`: Read in a Core Places file and use it to create a crosswalk between SafeGraph POI codes and NAICS codes.

Testing status: **BABY**
Documentation status: **NEEDS EXAMPLES**

`read_distancing`: Given a list of dates, reads in and aggregates SafeGraph social-distancing v2 files.

Testing status: **BABY**
Documentation status: **NEEDS EXAMPLES**

`read_many_csvs`: Reads a bunch of CSVs in the same folder and row-binds them all together. Useful for stuff like normalization data.

Testing status: **MODERATE**
Documentation status: **NEEDS EXAMPLES**

`read_many_patterns` and `read_patterns`: Reads a bunch of (or one, respectively) monthly or weekly patterns `.csv.gz` files all in the same folder, does appropriate processing, and row-binds the results together.

Testing status: **VERY LITTLE**
Documentation status: **EXAMPLES HAVE REVEALED BUG**

`read_many_shop` and `read_shop`: Reads a bunch of (or one, respectively) `.zip` files in the format they come in from the shop and combines the data sets inside the zip across zip-files appropriately.

Testing status: **BABY**
Documentation status: **NEEDS EXAMPLES**

`safegraph_aws`: A thin wrapper for `aws.s3::s3sync` that downloads data from the SafeGraph AWS COVID response buckets.

Testing status: **CURRENTLY NOT WORKING**
Documentation status: **NEEDS EXAMPLES**



## Data Processing Functions

`expand_cat_json` and `expand_integer_json`: Take SafeGraph data with a column of categorical (named) or numeric (unnamed) JSON data and expand that column, pivot the data to long format, and then aggregate to the desired level.

Testing status: **BABY**
Documentation status: **NEEDS EXAMPLES**

`fips_from_cbg`: Take a census block group identifier and extract the state and/or county FIPS codes.

Testing status: **COMPLETE**
Documentation status: **NEEDS EXAMPLES**

`rbind_by_list_pos`: Take a list of lists of `data.table`s and row-binds them by their position in the sub-list. For example, `rbind_by_list_pos(list(A,B),list(C,D))` would return `list(rbind(A,C),rbind(B,D))`. Can be used after `read_` functions, which in some cases return a list of `data.table`s for each file they read.

Testing status: **MODERATE**
Documentation status: **NEEDS EXAMPLES**

## Final-Stages Processing Functions

`hb_shrink`: Perform hierarchical Bayesian shrinkage on the CBG-to-county or county-to-state level.

Testing status: **BABY**
Documentation status: **NEEDS EXAMPLES**

`ma`: Calculates a (by default) seven day moving average on pre-sorted data with no gaps.

Testing status: **COMPLETE**
Documentation status: **NEEDS EXAMPLES**

`sample_size_adjust`: Adjusts data for differences in sampling rates across geographic locations.

Testing status: **BABY**
Documentation status: **NEEDS EXAMPLES**

`scale_to_date`: Adjusts data to be relative to a specific date.

Testing status: **BABY**
Documentation status: **NEEDS EXAMPLES**

`scale_yoy`: Adjusts data to be relative to the same date the previous year.

Testing status: **BABY**
Documentation status: **NEEDS EXAMPLES**

## Data Sets

`cbg_pop`: Population data from the easy census file.

Testing status: **COMPLETE**
Documentation status: **COMPLETE**

`county_pop`: Population aggregated to the county level.

Testing status: **COMPLETE**
Documentation status: **COMPLETE**

`fips_to_names`: Data set linking state and county FIPS codes to state and county names, for merging in and labeling.

Testing status: **COMPLETE**
Documentation status: **COMPLETE**

`naics_codes`: Data set linking NAICS codes to NAICS code titles, for merging in and labeling (or just knowing what you're looking at).

Testing status: **COMPLETE**
Documentation status: **COMPLETE**
