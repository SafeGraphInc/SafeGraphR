# SafeGraphR 0.3.0

* Added a `NEWS.md` file to track changes to the package.
* Updated package to work with AWS file structure as updated in December 2020.
* Added the ability to download the most recent Core files in `read_core()`.
* Added the ability to download the appropriate patterns files in `read_many_patterns`, and automated post-read re-aggregation.
* Added (and integrated) the `patterns_lookup()` function, which shows the proper weekly files to look in for data on a particular date, and also can download those files.
* Added the suite of "Volume Over Time" functions which can download, process, and graph `visits_by_day` weekly patterns data.
