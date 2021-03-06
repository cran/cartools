% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge.R
\name{merge}
\alias{merge}
\title{\code{merge} produces \code{t-x} trajectories for a pair of lead and following vehicles.}
\usage{
merge(df1,df2,leff,step,ylim)
}
\arguments{
\item{df1}{leading vehicle, a matrix}

\item{df2}{following vehicle, a matrix}

\item{leff}{vehicle length, a number}

\item{step}{time-step size, a number}

\item{ylim}{for plot, a vector}
}
\value{
The \code{merge}, a wrapper function for \code{bmfree2}, \code{xabparam} and
\code{hsafe}, returns a smooth \code{hsafe} rule \code{t-x} trajectory.
for the following vehicle. The lead vehicle trajectory is not affected.
Incidentally, inputs \code{df1} and \code{df2} were created with  \code{bmfree2} and used
for testing.
}
\description{
\code{merge} produces \code{t-x} trajectories for a pair of lead and following vehicles.
}
