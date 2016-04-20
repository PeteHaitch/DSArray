### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Utilities
###

context(".sparsify")

test_that(".sparsify methods give same output on differently classed input", {
  # NOTE: Need to be careful to preserve row.names in order for the output to
  #       be identical
  x_df <- as.data.frame(x[, 1, ], row.names = TRUE)
  x_dt <- as.data.table(x[, 1, ], keep.rownames = TRUE)
  expect_identical(.sparsify(x_df), .sparsify(x_dt))
  x_df <- as.data.frame(x[, 1, ], row.names = TRUE)
  x_matrix <- as.matrix(x[, 1, ])
  expect_identical(.sparsify(x_df), .sparsify(x_matrix))
  x_dt <- as.data.table(x[, 1, ], keep.rownames = TRUE)
  x_matrix <- as.matrix(x[, 1, ])
  expect_identical(.sparsify(x_dt), .sparsify(x_matrix))
})

test_that(".sparsify method works when input has no dimnames", {
  m <- x[, 1, ]
  dimnames(m) <- list(NULL, NULL)
  mm <- xx[, 1, ]
  dimnames(mm) <- NULL
  expect_identical(.sparsify(m), mm)
})


test_that(".sparsify errors on illegal column names", {
  m <- x[, , 1]
  colnames(m) <- c(paste0(".my", 1:4))
  expect_error(.sparsify(m), "'x' must not have colnames beginning with '.my'")
  colnames(m) <- paste0("..my", 1:4)
  expect_error(.sparsify(m), NA)
  colnames(m) <- c("rn", 1:3)
  expect_error(.sparsify(m), "'x' must not have a column named 'rn'")
  colnames(m) <- c("rnj", 1:3)
  expect_error(.sparsify(m), NA)
})

test_that(".sparsify returns something sensible on 0-row input", {
  # UP TO HERE: None of these are doing what I'd like; also, re-visit what
  #             DSArray() should return
  expect_identical(.sparsify(matrix()), DSArray(matrix()))
  expect_identical(.sparsify(data.frame()), DSArray(matrix()))
  expect_identical(.sparsify(data.table()), DSArray(matrix()))
})

# context(".validate_DSArray_value_dim")
#
# test_that("All the conditionals ")