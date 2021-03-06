% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphutils.R
\name{linegraph}
\alias{linegraph}
\title{Create a linegraph}
\usage{
linegraph(x, ...)
}
\arguments{
\item{x}{graph of class graph or loongraph}

\item{...}{arguments passed on to method}
}
\value{
graph object
}
\description{
The line graph of G, here denoted L(G), is the graph whose nodes
  correspond to the edges of G and whose edges correspond to nodes of G such
  that nodes of L(G) are joined if and only if the corresponding edges of G
  are adjacent in G.
}
