### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Arith
###

context("Summary group generic")

# TODO: Have asked Jim Hester why these tests aren't reported in the
#       shine(package_coverage()) output;
#       https://github.com/jimhester/covr/issues/182
test_that("DSArray,Summary-method returns identical result to array-based equivalent", {
  ops <- c(`max`, `min`, `range`, `prod`, `any`, `all`, `sum`)
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
      if (identical(op, any) || identical(op, all)) {
        expect_identical(suppressWarnings(op(dd)),
                         suppressWarnings(op(aa)))
      } else {
        expect_identical(op(dd), op(aa))
      }
    }, dd = d, aa = a, na.rm = na.rm)
  })
})

# Special challenges for sum() and prod() since these implementations are
# more complicated
test_that("DSArray,sum-method works on more challenging input", {
  a <- seq(1L, .Machine$integer.max + 1, 1000000L)
  m <- array(a, dim = c(537, 2, 2))
  mm <- DSArray(m)
  expect_identical(sum(m), sum(mm))
  a <- as.array(dsa)
  expect_identical(sum(dsa, na.rm = TRUE), sum(a, na.rm = TRUE))
  expect_identical(sum(dsa, na.rm = FALSE), sum(a, na.rm = FALSE))
})

test_that("DSArray,prod-method works on mor challenging input", {
  a <- sample(1:1000000, 20, replace = TRUE)
  m <- array(a, dim = c(2, 2, 5))
  a <- IRanges::arbind(m[1, , , drop = FALSE], m, array(NA, dim = c(1, 2, 5)))
  d <- DSArray(a)
  expect_identical(prod(d, na.rm = FALSE), prod(a, na.rm = FALSE))
  expect_identical(prod(d, na.rm = TRUE), prod(a, na.rm = TRUE))
})
