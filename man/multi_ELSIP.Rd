% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_ELSIP.R
\name{multi_ELSIP}
\alias{multi_ELSIP}
\title{Compare ELSIP parameter combinations}
\usage{
multi_ELSIP(grid, data, label, classifiers, replications = 1)
}
\arguments{
\item{grid}{a data frame containing parameter combinations for \code{ELSIP}.}

\item{data}{a data frame containing observations}

\item{label}{the column name in \code{data} to use as the classification
outcome.}

\item{classifiers}{a named list of classifiers, with parameters for each
specified according to the scheme outlined in \code{\link{ELSIP}}.}

\item{replications}{the number of times to run the analysis.}
}
\description{
Compare ELSIP parameter combinations
}
