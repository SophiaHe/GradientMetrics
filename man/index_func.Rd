% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index_func.R
\name{index_func}
\alias{index_func}
\title{index_func}
\usage{
index_func(data, grouping_var, index_var)
}
\arguments{
\item{data}{input data containing both \code{grouping_var} and \code{index_var}}

\item{grouping_var}{Grouping variable}

\item{index_var}{Index variable}
}
\value{
Returns the value of \code{output}
}
\description{
\code{index_func} return the index of a level included in the \code{index_var} variable in contrast to groups provided by levels of the \code{grouping_var} variable.
}
\details{
A Index-Generating Function

This function allows: for a given variable, generates the index of a level included in this variable in contrast to groups provided by levels of another variable.
}
