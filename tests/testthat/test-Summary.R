### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Arith
###

context("Summary group generic")

# TODO: Have asked Jim Hester why these tests aren't reported in the
#       shine(package_coverage()) output;
#       https://github.com/jimhester/covr/issues/182
test_that("DSArray,Summary-method returns identical result to array-based equivalent", {
  ops <- c(`max`, `min`, `range`, `prod`, `any`, `all`)
  a <- sample(1:1000000, 1000000, replace = TRUE)
  m <- array(a, dim = c(100000, 2, 5))
  mm <- DSArray(m)
  d <- list(xx, zz, mm)
  a <- list(x, z, m)
  na.rm <- rep_len(c(TRUE, FALSE), length(d))
  lapply(ops, function(op) {
    mapply(function(dd, aa, na.rm) {
      expect_identical(op(dd), op(aa))
      storage.mode(dd@val) <- "double"
      storage.mode(aa) <- "double"
      expect_equal(op(dd), op(aa))
    }, dd = d, aa = a, na.rm = na.rm)
  })
})

# NOTE: sum() may return numeric instead of integer because it calls inner
#       product (%*%) which returns numeric rather than integer regardless of
#       input. Therefore use expect_equal() rather than expect_identical()
test_that("DSArray,Summary-method returns equal result to array-based equivalent", {
  ops <- c(`sum`)
  a <- sample(1:1000000, 1000000, replace = TRUE)
  m <- array(a, dim = c(100000, 2, 5))
  mm <- DSArray(m)
  d <- list(xx, zz)
  a <- list(x, z)
  na.rm <- rep_len(c(TRUE, FALSE), length(d))
  lapply(ops, function(op) {
    mapply(function(dd, aa, na.rm) {
      expect_equal(op(dd), op(aa))
      storage.mode(dd@val) <- "double"
      storage.mode(aa) <- "double"
      expect_equal(op(dd), op(aa))
    }, dd = d, aa = a, na.rm = na.rm)
  })
})

# TODO: Special challenges for sum() and prod() since these implementations are
#       more complicated

test_that("DSArray,sum-method works on more challenging input", {
  a <- seq(1L, .Machine$integer.max + 1, 1000000L)
  m <- array(a, dim = c(537, 2, 2))
  mm <- DSArray(m)
  expect_identical(sum(m), sum(mm))
})