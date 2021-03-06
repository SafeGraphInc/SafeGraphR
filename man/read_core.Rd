% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_core.R
\name{read_core}
\alias{read_core}
\title{Read SafeGraph Core}
\usage{
read_core(
  dir = "core_poi/",
  filter = NULL,
  select = NULL,
  key = NULL,
  secret = NULL,
  silent = FALSE,
  ...
)
}
\arguments{
\item{dir}{The directory that the CORE files are in. If this folder contains multiple months of Core files, it will use the most recent (this only works if you are using the standard AWS file structure).}

\item{filter}{A character string describing a logical statement for filtering the data, for example \code{filter = 'naics_code == 512131'} would give you only movie theater POIs. Will be used as an \code{i} argument in a \code{data.table}, see \code{help(data.table)}. Filtering here instead of afterwards can cut down on time and memory demands.}

\item{select}{Character vector of variables to get from the file. Set to \code{NULL} to get all variables. If you plan to link the results to a patterns file, you will probably want to include \code{'placekey'} or \code{'placekey'} in this vector. Note that any variables mentioned in \code{filter} MUST be in \code{select} unless \code{select = NULL}.}

\item{key}{A character string containing an AWS Access Key ID. If \code{key} and \code{secret} are both specified, \code{read_core} will download the most recent Core files and process them. This process assumes your system date is set correctly, and will only check this month's Core and last month's Core, since one of those shold exist.}

\item{secret}{A character string containing an AWS Secret Access Key.}

\item{silent}{Suppress timing messages.}

\item{...}{Other arguments to be passed to \code{data.table::fread} when reading in the \code{CSV} files inside of the \code{ZIP}. For example, \code{nrows} to only read in a certain number of rows.}
}
\description{
Be aware that the files this is designed to work with are large and this function may take a while to execute. This function takes folder of Core files and reads it them in. The output is a \code{data.table}.
}
\details{
AS OF SafeGraphR VERSION 0.3.0 THIS FUNCTION ONLY WORKS WITH NEW CORE FILE FORMATS. For old-format Core files, you can still use the less-flexible and otherwise deprecated  \code{link_poi_naics()} function.
}
\examples{

\dontrun{
# Location of our CORE file
# Note we probably don't have to specify 2020/10 if that's the most recent one
dir <- '../SafeGraph/core_poi/2020/10/'

# Let's only get retail POIs in California
# And
locations <- read_core(dir = dir,
                       filter = 'region == "CA" & floor(naics_code/10000) \%in\% 44:45')
}
}
