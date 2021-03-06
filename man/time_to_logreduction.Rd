% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_to_reduction.R
\name{time_to_logreduction}
\alias{time_to_logreduction}
\title{Time to reach X log reductions}
\usage{
time_to_logreduction(n_logs, my_prediction)
}
\arguments{
\item{n_logs}{Numeric of length one indicating the number of log recutions}

\item{my_prediction}{An object of class SimulInactivation}
}
\description{
Calculates the treatment time required to reach a given number of log reductions.
}
\details{
The treatement time is calculated by linear interpolation betweent the two points of
the simulation whose logS is closer to n_logs
}
