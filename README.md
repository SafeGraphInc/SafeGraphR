# SafeGraphR
Package for reading and analyzing SafeGraph foot traffic data

This package is under development.

Current to-do list:

Fixes for reading-in functions:

1. Automatically merge in NAICS titles for by-NAICS data
2. Automatically merge county and state names
3. Create read_shop for reading files as they come from the shop

Other functions to add:
1. Run hierarchical Bayes (Nick H-K has code)
2. Add easy Census data internally
3. Do population weighting when aggregating (Nick H-K has code)
4. Add function to scale everything relative to a certain date, or to scale year-on-year in SafeGraph style.
5. Add NAICS titles internally (Nick H-K has a file)
6. Easy and attractive template **ggplot2** graph for "foot traffic in [brand/NAICS X, Y, Z] for range A-B relative to date C" (Nick H-K has code)
7. AWS downloader
8. Add county and state names
9. Add NAICS codes
