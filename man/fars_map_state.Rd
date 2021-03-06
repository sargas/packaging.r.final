% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_function.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Show reported fatalities on a state map}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{a single integer value identifying the state}

\item{year}{a year which is coercable to an integer with the
\code{\link{as.integer}} function}
}
\value{
There is no value returned
}
\description{
This function displays a map of the state using the maps package and plots
the locations for all records in the Fatality Analysis Reporting System
loccateed within that state. This function stops with an error if told to
plot a state number that is not in the dataset for that year. Additionally,
an error with be shown if a dataset for the specified year does not exist.
}
\examples{
fars_map_state(6, 2013) #  California traffic fatalities in 2013
fars_map_state(17, 2015) # Illinois traffic fatalities in 2015

}
