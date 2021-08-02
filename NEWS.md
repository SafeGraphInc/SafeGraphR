# SafeGraphR 0.3.0

* Added a `NEWS.md` file to track changes to the package.
* Updated package to work with AWS file structure as updated in December 2020.
* Added the ability to download the most recent Core files in `read_core()`.
* Added the ability to download the appropriate patterns files in `read_many_patterns`, and automated post-read re-aggregation.
* Added (and integrated) the `patterns_lookup()` function, which shows the proper weekly files to look in for data on a particular date, and also can download those files.
* Added the suite of "Volume Over Time" functions which can download, process, and graph `visits_by_day` weekly patterns data.

# SafeGraphR 0.4.0

* Switched reliance on `safegraph_place_id` to `placekey` since new files will no longer contain `safegraph_place_id`
* Fixed an issue in `read_many_patterns()` where it would error if `filter` left zero observations in some subfiles

# SafeGraphR 0.4.1

* Added considerable support for Canadian data, including place names in `state_info`, `fips_to_names`, `canada_cd_pop`, and `canada_cd_types`.
* Also changed `fips_to_cbg` to support Canada, and as a result the output is now character rather than numeric.

# SafeGraphR 0.4.2

* Updated buckets for `safegraph_aws()` (and `patterns_lookup` to match). NOTE THIS IS A BREAKING CHANGE. `safegraph_aws()` now only looks at the most updated versions of the data. Access to previous versions is discontinued, and you'll need to do it by hand using `aws.s3::s3sync`. There's no reason to use the old versions of the data anyway.

# SafeGraphR 0.4.3

* Changed data files to include leading zeroes for state and county FIPS
