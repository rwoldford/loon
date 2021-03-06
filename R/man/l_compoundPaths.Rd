% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/l_loonWidgets.R
\name{l_compoundPaths}
\alias{l_compoundPaths}
\title{Get the set of basic path types for loon plots.}
\usage{
l_compoundPaths()
}
\value{
character vector of the compound path types.
}
\description{
Loon's plots are constructed in TCL and identified with
a path string appearing in the window containing the plot.
The path string begins with a unique identifier for the plot
and ends with a suffix describing the type of loon plot being displayed.

The path identifying the plot is the string concatenation of both the
identifier and the type.

This function returns the set of the loon
path types for compound loon plots.
}
\seealso{
\code{\link{l_basePaths}}\code{\link{l_loonWidgets}} \code{\link{l_getFromPath}}
}
