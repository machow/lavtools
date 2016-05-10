#' Mustache param labels, if they exist
#'
#' @param label array of parameter labels or empty strings "".
param <- function(label){
  ifelse(label != "", paste0("{{", label, "}}*"), label)
}

#' Group all partable rows with same parent (left-hand side).
#'
#' For each unique value of lhs column, make lavaan string syntax of form,
#' "lhs =~ label1*rhs1 + label2*rhs2 + ...", where symbol in this case is "=~".
#'
#' @param d data.frame specifying lavaan partable
#' @param symbol operator to subset data on first
#' @export
#' @examples
#' partable <- make_edge('A', '=~', c('A1', 'A2'), 'lam_A')
#' group_rhs(partable, '=~')
#' # "A =~ lam_A*A1 + lam_A*A2"
group_rhs <- function(d, symbol) {
  lv <- d[d$op == symbol,]
  sapply(split(lv, lv$lhs), function(rows){
    paste(rows$lhs[1], rows$op[1],
          paste0(param(rows$label), rows$rhs, collapse=" + ")
    )})
}

#' Group like-terms from left-hand side, and output lavaan syntax.
#'
#' Separates three kinds of equations
#'  * latent variable: uses "=~" operator
#'  * latent covariance: uses '~~' with lhs != rhs
#'  * residual variance: uses '~~' with lhs == rhs
#'
#' @param d data.frame specifying lavaan partable
#' @export
#' @examples
#' # Build partable
#' make_meas <- make_edge(op='=~')
#' make_cov <-  make_edge(op='~~')
#' partable <- rbind(
#'    make_meas('A', c('A1','A2'), 'lam_A'),
#'    make_meas('B', c('B1','B2'), 'lam_B'),
#'    make_cov( 'A', 'B', 'cov_AxB'),
#'    make_cov( 'A', 'A', ""),
#'    make_cov( 'B', 'B', "")
#' )
#' # Convert to lavaan string syntax
#' group_out(partable)
group_out <- function(d) {
  meas   <- group_rhs(d, '=~')
  lv_rho <- group_rhs(d[d$lhs != d$rhs,], '~~')
  resid  <- group_rhs(d[d$lhs == d$rhs,], '~~')
  GetoptLong::qq("
# Measurement Model
@{paste(meas, collapse='\n')}

# Latent Covariances
@{paste(lv_rho, collapse='\n')}

# Residual variance
@{paste(resid, collapse='\n')}
")
}
