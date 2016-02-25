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
  slot(yy, "key") <- -slot(yy, "key")
  expect_output(.valid.DSArray.key(yy), msg)
  expect_error(validObject(yy), msg)
  yy <- xx
  slot(yy, "key")[1] <- NA_integer_
  expect_output(.valid.DSArray.key(yy), msg)
  expect_error(validObject(yy), msg)
  yy <- xx
  slot(yy, "key", check = FALSE) <- array(slot(yy, "key"), dim = c(100, 2, 2))
  expect_output(.valid.DSArray.key(yy), msg)
  expect_error(validObject(yy),
               "invalid object for slot \"key\" in class \"DSArray\"")
  yy <- xx
  storage.mode(slot(yy, "key")) <- "double"
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
  slot(yy, "val", check = FALSE) <- array(slot(yy, "val"), dim = c(nrow(yy), 4, 2))
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

# NOTE: DSArray,missing-method not currently implemented
# test_that("DSArray,missing-method works", {
#   expect_true(dsa_identical_to_array(DSArray(), array(dim = c(1, 1, 1))))
# })

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
  expect_error(DSArray(x + 1i),
               "'complex' arrays not yet supported")
  expect_error(DSArray(array(1:16, dim = rep(2, 4))),
               "array must have 3 dimensions")
  expect_error(DSArray(x, MARGIN = c(1, 2)), "incorrect value for 'MARGIN'")
  expect_error(DSArray(x, MARGIN = 4), "incorrect value for 'MARGIN'")
  a1 <- array(c(1L, 5L, 2L, 6L, 3L, 7L, 4L, 8L), dim = rep(2, 3))
  a2 <- array(c(1L, 2L, 5L, 6L, 3L, 4L, 7L, 8L), dim = rep(2, 3))
  a3 <- array(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L), dim = rep(2, 3))
  expect_true(dsa_identical_to_array(DSArray(a1, MARGIN = 1L), a2))
  expect_true(dsa_identical_to_array(DSArray(a2, MARGIN = 2L), a2))
  expect_true(dsa_identical_to_array(DSArray(a3, MARGIN = 3L), a2))
  dn <- split(letters[1:6], rep(1:3, each = 2))
  A1 <- array(c(1L, 5L, 2L, 6L, 3L, 7L, 4L, 8L), dim = rep(2, 3), dn)
  A2 <- array(c(1L, 2L, 5L, 6L, 3L, 4L, 7L, 8L), dim = rep(2, 3), dn)
  A3 <- array(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L), dim = rep(2, 3), dn)
  expect_identical(DSArray(a1, MARGIN = 1L, dimnames = c(dn[2], dn[1], dn[3])),
                   DSArray(A1, MARGIN = 1L))
  expect_identical(DSArray(a2, MARGIN = 2L, dimnames = c(dn[1], dn[2], dn[3])),
                   DSArray(A2, MARGIN = 2L))
  expect_identical(DSArray(a3, MARGIN = 3L, dimnames = c(dn[1], dn[3], dn[2])),
                   DSArray(A3, MARGIN = 3L))
})

context("dimension-related getters and setters")

test_that("dim works", {
  expect_identical(dim(x), dim(xx))
})

test_that("nslice works", {
  expect_identical(nslice(xx), dim(x)[3])
})

test_that("length works", {
  expect_identical(length(x), length(xx))
})

test_that("dimnames works", {
  expect_identical(dimnames(x), dimnames(xx))
})

test_that("slicenames works", {
  expect_identical(slicenames(xx), dimnames(x)[[3]])
})

test_that("dimnames<- works", {
  dn <- lapply(list(100:1, 4:1, 8:1), as.character)
  dimnames(xx) <- dn
  expect_identical(dimnames(xx), dn)
  dimnames(xx) <- NULL
  expect_null(dimnames(xx))
})

test_that("slicenames<- works", {
  sn <- as.character(8:1)
  slicenames(xx) <- sn
  expect_identical(slicenames(xx), sn)
  slicenames(xx) <- NULL
  expect_null(slicenames(xx))
})

context("[,DSArray,ANY-method")


# test_that("subsetting by i works on a one-column DSArray", {
#   x1 <- x[, 1, , drop = FALSE]
#   xx1 <- DSArray(x1)
#   i <- list(1, 1:10, sample(nrow(x1)), rep(1:10, 10))
#   lapply(i, function(ii) {
#     expect_true(dsa_identical_to_array(xx1[ii, , ], x1[ii, , , drop = FALSE]))
#     expect_true(dsa_identical_to_array(xx1[as.character(ii), , ],
#                                        x1[as.character(ii), , , drop = FALSE]))
#   })
#   i_bad <- list(nrow(x1) + 1, 1:(nrow(x1) + 1))
#   msg <- "subscript i out of bounds"
#   lapply(i_bad, function(ii) {
#     expect_error(xx1[ii, , ], msg)
#     expect_error(xx1[as.character(ii), , ], msg)
#   })
# })

test_that("subsetting by i works on a one-column DSArray", {
  i <- list(1, 1:10, sample(nrow(x)), rep(1:10, 10))
  lapply(i, function(ii) {
    expect_true(dsa_identical_to_array(xx[ii, , ], x[ii, , , drop = FALSE]))
    expect_identical(.extract_DSArray_subset(xx, ii), xx[ii, , ])
    expect_true(dsa_identical_to_array(xx[as.character(ii), , ],
                                       x[as.character(ii), , , drop = FALSE]))
    expect_identical(.extract_DSArray_subset(xx, as.character(ii)),
                     xx[as.character(ii), , ])

  })
  i_bad <- list(nrow(x) + 1, 1:(nrow(x) + 1))
  msg <- "subscript i out of bounds"
  lapply(i_bad, function(ii) {
    expect_error(xx[ii, , ], msg)
    expect_error(.extract_DSArray_subset(xx, ii), msg)
    expect_error(xx[as.character(ii), , ], msg)
    expect_error(.extract_DSArray_subset(xx, as.character(ii)), msg)
  })
})

# UP TO HERE: subsetting by j, (i, j), (i, k), (j, k), (i, j, k), ()
