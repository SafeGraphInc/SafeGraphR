# SafeGraphR
Package for reading and analyzing SafeGraph foot traffic data

This package is under development.

Current to-do list:

Fixes for `read_monthly_shop` and `read_weekly_aws`:

1. Check if everything works with, say, monthly AWS files and weekly shop files, and then generalize the documentation to match
2. Automatically merge in NAICS titles for by-NAICS data
3. Automatically merge county and state names

Fixes for `read_distancing`:

1. Default to automatically read in all the distancing data you have
2. Automatically merge county and state names

Functions to add:
1. Run hierarchical Bayes (Nick H-K has code)
2. Add easy Census data internally
3. Do population weighting when aggregating (Nick H-K has code)
4. Add function to scale everything relative to a certain date, or to scale year-on-year in SafeGraph style.
5. Add NAICS titles internally (Nick H-K has a file)
6. Easy and attractive template **ggplot2** graph for "foot traffic in [brand/NAICS X, Y, Z] for range A-B relative to date C" (Nick H-K has code)
7. AWS downloader
8. Add county and state names
9. Add NAICS codes
