context("lavtools")

test_that("make_edge does not return factors", {
  is_factor <- sapply(make_edge('A', '=~', 'A1', 'lam_A1'), inherits, what='factor')
  expect_false(all(is_factor))
})

test_that("make_edge can be curried", {
  make_meas <- make_edge(op='=~')
  expect_identical(
    make_meas('A', 'A1', 'lam_A1', value=.5),
    data.frame(lhs='A', op='=~', rhs='A1', label='lam_A1', value=.5, stringsAsFactors=FALSE)
  )
})

test_that("make_resid is equivalent to make_edge in simple case", {
  a <- make_edge('A', '~~', 'A', "")
  b <- make_resid('A')
  expect_identical(a, b)
})

test_that("group_out correctly groups latent variables", {})

test_that("remove_var removes single variable", {})

test_that("remove_var removes multiple variables", {})

test_that("expand_mod returns unique args", {})

test_that("expand_mod returned template allows mustache substitution", {})

test_that("expand_mod returned model instance can simulate data", {})

test_that("expand_mod returned model can fit simulated data", {})




#make_meas <- make_edge(op='=~')
#make_cov <-  make_edge(op='~~')
#partable <- rbind(
#  a <- make_meas('A', c('A1','A2'), 'lam_A', .3),
#  make_meas('B', c('B1','B2'), 'lam_B', .6),
#  make_cov( 'A', 'B', 'cov_AxB'),
#  make_cov( 'A', 'A', ""),
#  make_cov( 'B', 'B', "")
#)
#
#two_factor <- expand_mod(partable)

