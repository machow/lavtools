% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_partable.R
\name{make_resid}
\alias{make_resid}
\title{Create residual row(s) for lavaan partable}
\usage{
make_resid(lhs, label = "", value = "")
}
\arguments{
\item{lhs}{left-hand side of model equation}

\item{label}{label for parameter that weights rhs}

\item{value}{value for parameter}
}
\value{
data.frame with row for each new residual
}
\description{
Residuals have the form, "lhs ~~ label*lhs".
}

