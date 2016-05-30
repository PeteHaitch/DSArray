### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Utilities
###

context(".sparsify")

test_that(".sparsify methods give same output on differently classed input", {
  # NOTE: Need to be careful to preserve row.names in order for the output to
  #       be identical
  x_df <- as.data.frame(x[, 1, ])
  x_dt <- as.data.table(x[, 1, ], keep.rownames = TRUE)
  expect_identical(.sparsify(x_df), suppressWarnings(.sparsify(x_dt)))
  x_df <- as.data.frame(x[, 1, ])
  x_matrix <- as.matrix(x[, 1, ])
  expect_identical(.sparsify(x_df), .sparsify(x_matrix))
  x_dt <- as.data.table(x[, 1, ], keep.rownames = TRUE)
  x_matrix <- as.matrix(x[, 1, ])
  expect_identical(suppressWarnings(.sparsify(x_dt)), .sparsify(x_matrix))
})

test_that(".sparsify method works when input has partial or NULL dimnames", {
  # NOTE: x[, 1, ] returns a 2-dimensional array (strictly, a matrix), whereas
  #       xx[, 1, ] returns a 3-dimensional DSArray. This is due to
  #       drop = FALSE in the former.
  m <- x[, 1, ]
  dimnames(m) <- list(NULL, NULL)
  mm <- xx[, 1, ]
  dimnames(mm) <- NULL
  expect_identical(.sparsify(m), mm)
  m <- x[, 1, ]
  rownames(m) <- NULL
  mm <- xx[, 1, ]
  rownames(mm) <- NULL
  # NOTE: Need to also drop colnames() because these columns are implicitly
  #       removed in the array-subsetting
  colnames(mm) <- NULL
  expect_identical(.sparsify(m), mm)
  m <- x[, 1, ]
  colnames(m) <- NULL
  mm <- xx[, 1, ]
  slicenames(mm) <- NULL
  # NOTE: Need to also drop slicenames() because these columns are implicitly
  #       removed in the array-subsetting (and slices become columns as a
  #       result)
  colnames(mm) <- NULL
  expect_identical(.sparsify(m), mm)
})

test_that(".sparsify errors on illegal column names", {
  m <- x[, , 1]
  colnames(m) <- c(paste0(".my", seq_len(ncol(m))))
  expect_error(.sparsify(m), "'x' must not have colnames beginning with '.my'")
  colnames(m) <- paste0("..my", seq_len(ncol(m)))
  expect_error(.sparsify(m), NA)
  colnames(m) <- c("rn", seq_len(ncol(m) - 1L))
  expect_error(.sparsify(m), "'x' must not have a column named 'rn'")
  colnames(m) <- c("rnj", seq_len(ncol(m) - 1L))
  expect_error(.sparsify(m), NA)
})

test_that(".sparsify returns something sensible on 0-row input", {
  expect_identical(.sparsify(matrix()), DSArray(matrix()))
  expect_identical(.sparsify(data.frame()), DSArray(matrix()))
  expect_identical(.sparsify(data.table()), DSArray(matrix()))
})

test_that(".list_to_array works", {
  l <- lapply(seq_len(ncol(x)), function(j) x[ , j, ])
  expect_identical(.list_to_array(l, dim(x), dimnames(x)), x)
  # Default dim should work
  expect_identical(.list_to_array(l, dimnames = dimnames(x)), x)
})

test_that(".densify works", {
  expect_warning(.densify(xx),
                 "Densifying. This can cause a large increase in memory usage")
  expect_warning(.densify(xx, warn = FALSE), NA)
  expect_identical(.densify(xx, warn = FALSE), x)
  expect_identical(.densify(xx, simplify = FALSE, warn = FALSE),
                   mapply(function(j) x[, j, ], j = colnames(x),
                          SIMPLIFY = FALSE))
})

test_that("Guessing the size of array and DSArray is (approximately) correct", {
  x <- unname(x)
  xx <- unname(xx)
  nr <- nrow(xx)
  nc <- ncol(xx)
  sl <- nslice(xx)
  pus <- nrow(xx@val) / (nrow(x) * ncol(x))
  so <- 4L
  expect_equal(.sizeBaseArray(nr, nc, sl, pus, so),
               as.numeric(object.size(x)),
               tolerance = 5e-1)
  expect_equal(.sizeDSArray(nr, nc, sl, pus, so),
               as.numeric(object.size(xx)),
               tolerance = 5e-1)
  expect_equal(.sizeRatio(sl, pus, so),
               as.numeric(object.size(xx)) / as.numeric(object.size(x)),
               tolerance = 5e-1)
})
