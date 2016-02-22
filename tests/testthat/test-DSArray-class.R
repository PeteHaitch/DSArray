### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### DSArray-class
###

context("DSArray validity methods")

test_that(".valid.DSArray.key and validObject work", {
  expect_null(.valid.DSArray.key(xx), NULL)
  expect_that(validObject(xx), not(throws_error()))
  msg <- paste0("'key' slot of a DSArray must be a matrix of ",
                "positive integers \\(NAs not permitted\\)")
  yy <- xx
  yy@key <- -yy@key
  expect_output(.valid.DSArray.key(yy), msg)
  expect_error(validObject(yy), msg)
  yy <- xx
  yy@key[1] <- NA_integer_
  expect_output(.valid.DSArray.key(yy), msg)
  expect_error(validObject(yy), msg)
  yy <- xx
  slot(yy, "key", check = FALSE) <- array(yy@key, dim = c(100, 2, 2))
  expect_output(.valid.DSArray.key(yy), msg)
  expect_error(validObject(yy),
               "invalid object for slot \"key\" in class \"DSArray\"")
  yy <- xx
  storage.mode(yy@key) <- "double"
  expect_output(.valid.DSArray.key(yy), msg)
  expect_error(validObject(yy), msg)
  yy <- xx
  msg <- "Element\\(s\\) of key > nrow\\(val\\)"
  slot(yy, "key", check = FALSE) <- slot(yy, "key") + 1L
  expect_output(.valid.DSArray.key(yy), msg)
  expect_error(validObject(yy), msg)
})

test_that(".valid.DSArray.val and validObject work", {
  expect_null(.valid.DSArray.val(xx))
  expect_that(validObject(xx), not(throws_error()))
  msg <- "'val' slot of a DSArray must be a matrix"
  yy <- xx
  slot(yy, "val", check = FALSE) <- array(yy@val, dim = c(nrow(yy), 4, 2))
  expect_output(.valid.DSArray.val(yy), msg)
  expect_error(validObject(yy),
               "invalid object for slot \"val\" in class \"DSArray\"")
  yy <- xx
  msg <- "'complex' arrays not currently supported"
  slot(yy, "val", check = FALSE) <- slot(yy, "val") + 1i
  expect_output(.valid.DSArray.val(yy), msg)
  expect_error(validObject(yy), msg)
})

context("DSArray constructor")

test_that("DSArray,matrix-method works", {
  m <- matrix(1:10, ncol = 2, dimnames = list(letters[1:5], LETTERS[1:2]))
  expect_is(DSArray(m), "DSArray")
  expect_identical(dimnames(DSArray(m)), list(rownames(m), NULL, colnames(m)))
  expect_null(dimnames(DSArray(unname(m))))
  dn <- list(LETTERS[1:5], LETTERS[6], LETTERS[7:8])
  expect_error(DSArray(m, dimnames = dn[1:2]),
               "supplied 'dimnames' must have length 3")
  expect_identical(dimnames(DSArray(m, dimnames = dn)), dn)
  dn <- list(LETTERS[1:5], LETTERS[6], LETTERS[7:9])
  expect_error(DSArray(m, dimnames = dn))
})

test_that("DSArray,missing-method works", {
  expect_true(dsa_identical_to_array(DSArray(), array(dim = c(1, 1, 1))))
})

test_that("DSArray,list-method works", {
  l <- lapply(seq_len(dim(x)[3]), function(k) x[, , k, drop = TRUE])
  expect_is(DSArray(l), "DSArray")
  expect_identical(dimnames(DSArray(l)), list(rownames(x), NULL, colnames(x)))
  expect_null(dimnames(unname(DSArray(l))))
  dn <- list(as.character(100:1), LETTERS[1:8], LETTERS[7:10])
  expect_error(DSArray(l, dimnames = dn[1:2]),
               "supplied 'dimnames' must have length 3")
  expect_identical(dimnames(DSArray(l, dimnames = dn)), dn)
  dn <- list(as.character(100:1), LETTERS[1:8], LETTERS[7:11])
  expect_error(DSArray(m, dimnames = dn))
  l2 <- lapply(seq_len(dim(x)[3]), function(k) x[, , k, drop = FALSE])
  expect_error(DSArray(l2), "All elements of 'x' must be matrix objects")
  l3 <- list(matrix(1:10, ncol = 2), matrix(1:100, ncol = 4))
  expect_error(DSArray(l3), "All elements of 'x' must have same dimensions")
})

test_that("DSArray,array-method works", {
  # UP TO HERE
})
