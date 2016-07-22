### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Complex
###
### NOTE: DSArray won't support complex arrays until data.table can support
###       complex keys. So these tests are just to see if anything has changed
###       in that regard.

context("Complex group generic")

test_that("data.table supports complex keys", {
          expect_error(setkey(data.table(x = 3 + 1i), x),
                       paste0("Column 'x' is type 'complex' which is not ",
                              "supported as a key column type, currently."))
})

test_that("DSArray constructor errors on complex input", {
  xi <- x + 3i
  expect_error(DSArray(xi), "complex numbers not currently supported")
  expect_error(DSArray(as.matrix(xi[, 1, ])),
               "complex numbers not currently supported")
  expect_error(DSArray(list(as.matrix(xi[, 1, ]), as.matrix(xi[, 2, ]))),
               "complex numbers not currently supported")
})