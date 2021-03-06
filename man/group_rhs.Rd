% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_lav_syntax.R
\name{group_rhs}
\alias{group_rhs}
\title{Group all partable rows with same parent (left-hand side).}
\usage{
group_rhs(d, symbol)
}
\arguments{
\item{d}{data.frame specifying lavaan partable}

\item{symbol}{operator to subset data on first}
}
\description{
For each unique value of lhs column, make lavaan string syntax of form,
"lhs =~ label1*rhs1 + label2*rhs2 + ...", where symbol in this case is "=~".
}
\examples{
partable <- make_edge('A', '=~', c('A1', 'A2'), 'lam_A')
group_rhs(partable, '=~')
# "A =~ lam_A*A1 + lam_A*A2"
}

