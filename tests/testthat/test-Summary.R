### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Arith
###

context("Summary group generic")

test_that("DSArray,Summary-method returns identical result to array-based equivalent", {
  ops <- c(`max`, `min`, `range`, `prod`, `any`, `all`)
  d <- list(xx, zz)
  a <- list(x, z)
  na.rm <- rep_len(c(TRUE, FALSE), length(d))
  lapply(ops, function(op) {
    mapply(function(d, a, na.rm) {
      expect_identical(op(d), op(a))
    }, d = d, a = d, na.rm = na.rm)
  })
})

# NOTE: sum() may return numeric instead of integer because it calls inner
#       product (%*%) which returns numeric rather than integer regardless of
#       input
test_that("DSArray,Summary-method returns equal result to array-based equivalent", {
  ops <- c(`sum`)
  d <- list(xx, zz)
  a <- list(x, z)
  na.rm <- rep_len(c(TRUE, FALSE), length(d))
  lapply(ops, function(op) {
    mapply(function(d, a, na.rm) {
      expect_equal(op(d), op(a))
    }, d = d, a = d, na.rm = na.rm)
  })
})

# TODO: Special challenges for sum() and prod() since these implementations are
#       more complicated