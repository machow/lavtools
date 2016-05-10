#' Generic function to create row(s) for lavaan partable
#'
#' Curried convenience function which passes arguments straight into data.frame.
#' Each row in the partable specifies an equation, e.g.
#' "lhs ~~ label*rhs", where "~~" would be the operator.
#'
#' @param lhs left-hand side of model equation
#' @param op operator connecting both sides
#' @param rhs right-hand side of model equation
#' @param label label for parameter that weights rhs
#' @param value value for parameter
#' @export
make_edge <- curry(function(lhs, op, rhs, label, value=""){
  data.frame(lhs, op, rhs, label, value, stringsAsFactors=FALSE)
})

#' Create residual row(s) for lavaan partable
#'
#' Residuals have the form, "lhs ~~ label*lhs".
#'
#' @inheritParams make_edge
#' @export
#' @return data.frame with row for each new residual
make_resid <- curry(function(lhs, label="", value=""){
  data.frame(lhs, op = '~~', rhs=lhs, label, value, stringsAsFactors=FALSE)
})
