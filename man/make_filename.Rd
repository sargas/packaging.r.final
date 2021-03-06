% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_function.R
\name{make_filename}
\alias{make_filename}
\title{Format the file name used by the FARS datasets}
\usage{
make_filename(year)
}
\arguments{
\item{year}{a year which is coercable to an integer with the
\code{\link{as.integer}} function}
}
\value{
Returns a character string with the file name
}
\description{
This function forms the filename of the datasets for a particular year as
used by the Fatality Analysis Reporting System datasets provided by the US
National Highway Traffic Safety Administration
}
\examples{
make_filename(2013)
make_filename(2015)

}
