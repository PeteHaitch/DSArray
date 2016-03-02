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
  expect_true(dsa_identical_to_array(
    DSArray(array(10, dim = c(3, 1, 4)), MARGIN = 1L),
    array(10, dim = c(1, 3, 4))))
  expect_true(dsa_identical_to_array(
    DSArray(array(10, dim = c(1, 3, 4)), MARGIN = 2L),
    array(10, dim = c(1, 3, 4))))
  expect_true(dsa_identical_to_array(
    DSArray(array(10, dim = c(1, 4, 3)), MARGIN = 3L),
    array(10, dim = c(1, 3, 4))))
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

test_that("drop is ignored in [,DSArray-metho", {
  msg <- "'drop' ignored '\\[,DSArray,ANY-method'"
  expect_warning(xx[1:10, , , drop = TRUE], msg)
  expect_warning(.extract_DSArray_subset(x = xx, i = 1:10, drop = TRUE), msg)
})

test_that("subsetting a DSArray by i works or errors on bad input", {
  i <- list(1, 1:10, sample(nrow(xx)), rep(1:10, 10))
  lapply(i, function(ii) {
    expect_true(dsa_identical_to_array(xx[ii, , ], x[ii, , , drop = FALSE]))
    expect_identical(.extract_DSArray_subset(x = xx, i = ii), xx[ii, , ])
    expect_true(dsa_identical_to_array(xx[as.character(ii), , ],
                                       x[as.character(ii), , , drop = FALSE]))
    expect_identical(.extract_DSArray_subset(x = xx, i = as.character(ii)),
                     xx[as.character(ii), , ])

  })
  i_bad <- list(nrow(xx) + 1, 1:(nrow(xx) + 1))
  msg <- "subscript i out of bounds"
  lapply(i_bad, function(ii) {
    expect_error(xx[ii, , ], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = ii), msg)
    expect_error(xx[as.character(ii), , ], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = as.character(ii)), msg)
  })
})

test_that("subsetting a DSArray by j works or errors on bad input", {
  j <- list(1, 1:2, sample(ncol(xx)), rep(1:3, 10))
  lapply(j, function(jj) {
    expect_true(dsa_identical_to_array(xx[, jj, ], x[, jj, , drop = FALSE]))
    expect_identical(.extract_DSArray_subset(xx, j = jj), xx[, jj, ])
    expect_true(dsa_identical_to_array(xx[, letters[jj] , ],
                                       x[, letters[jj], , drop = FALSE]))
    expect_identical(.extract_DSArray_subset(xx, j = letters[jj]),
                     xx[, letters[jj], , ])

  })
  j_bad <- list(ncol(xx) + 1, 1:(ncol(x) + 1))
  msg <- "subscript j out of bounds"
  lapply(j_bad, function(jj) {
    expect_error(xx[, jj, , ], msg)
    expect_error(.extract_DSArray_subset(x = xx, j = jj), msg)
    expect_error(xx[, letters[jj], ], msg)
    expect_error(.extract_DSArray_subset(x = xx, j = letters[jj]), msg)
  })
})

test_that("subsetting a DSArray by k works or errors on bad input", {
  k <- list(1, 1:6, sample(nslice(xx)), rep(1:6, 10))
  lapply(k, function(kk) {
    expect_true(dsa_identical_to_array(xx[, , kk], x[, , kk, drop = FALSE]))
    expect_identical(.extract_DSArray_subset(xx, k = kk), xx[, , kk])
    expect_true(dsa_identical_to_array(xx[, , LETTERS[kk]],
                                       x[, , LETTERS[kk], drop = FALSE]))
    expect_identical(.extract_DSArray_subset(xx, k = LETTERS[kk]),
                     xx[, , LETTERS[kk], ])

  })
  k_bad <- list(nslice(xx) + 1, 1:(nslice(xx) + 1))
  msg <- "subscript k out of bounds"
  lapply(k_bad, function(kk) {
    expect_error(xx[, , kk], msg)
    expect_error(.extract_DSArray_subset(x = xx, k = kk), msg)
    expect_error(xx[, , LETTERS[kk]], msg)
    expect_error(.extract_DSArray_subset(x = xx, k = letters[kk]), msg)
  })
})

test_that("subsetting a DSArray by (i, j) works or errors on bad input", {
  i <- list(1, 1:10, sample(nrow(xx)), rep(1:10, 10))
  j <- list(1, 1:2, sample(ncol(xx)), rep(1:3, 10))
  Map(function(ii, jj) {
    expect_true(dsa_identical_to_array(xx[ii, jj, ], x[ii, jj, , drop = FALSE]))
    expect_identical(.extract_DSArray_subset(xx, i = ii, j = jj), xx[ii, jj, ])
    expect_true(dsa_identical_to_array(xx[as.character(ii), letters[jj], ],
                                       x[as.character(ii), letters[jj], ,
                                         drop = FALSE]))
    expect_identical(.extract_DSArray_subset(xx, i = as.character(ii),
                                             j = letters[jj]),
                     xx[as.character(ii), letters[jj], , ])
  }, ii = i, jj = j)
  i_bad <- list(nrow(xx) + 1, 1:(nrow(xx) + 1))
  j_bad <- list(ncol(xx) + 1, 1:(ncol(xx) + 1))
  msg <- "subscript j out of bounds"
  Map(function(ii, jj) {
    expect_error(xx[ii, jj, , ], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = ii, j = jj), msg)
    expect_error(xx[as.character(ii), letters[jj], ], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = as.character(ii),
                                         j = letters[jj]), msg)
  }, ii = i, jj = j_bad)
  msg <- "subscript i out of bounds"
  Map(function(ii, jj) {
    expect_error(xx[ii, jj, , ], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = ii, j = jj), msg)
    expect_error(xx[as.character(ii), letters[jj], ], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = as.character(ii),
                                         j = letters[jj]), msg)
  }, ii = c(i_bad, i_bad), jj = c(j[1:2], j_bad))
})

test_that("subsetting a DSArray by (i, k) works or errors on bad input", {
  i <- list(1, 1:10, sample(nrow(xx)), rep(1:10, 10))
  k <- list(1, 1:6, sample(nslice(xx)), rep(1:6, 10))
  Map(function(ii, kk) {
    expect_true(dsa_identical_to_array(xx[ii, , kk], x[ii, , kk, drop = FALSE]))
    expect_identical(.extract_DSArray_subset(xx, i = ii, k = kk), xx[ii, , kk])
    expect_true(dsa_identical_to_array(xx[as.character(ii), , LETTERS[kk]],
                                       x[as.character(ii), , LETTERS[kk],
                                         drop = FALSE]))
    expect_identical(.extract_DSArray_subset(xx, i = as.character(ii),
                                             k = LETTERS[kk]),
                     xx[as.character(ii), , LETTERS[kk], ])
  }, ii = i, kk = k)
  i_bad <- list(nrow(xx) + 1, 1:(nrow(xx) + 1))
  k_bad <- list(nslice(xx) + 1, 1:(nslice(xx) + 1))
  msg <- "subscript k out of bounds"
  Map(function(ii, kk) {
    expect_error(xx[ii, , kk, ], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = ii, k = kk), msg)
    expect_error(xx[as.character(ii), , LETTERS[kk]], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = as.character(ii),
                                         k = LETTERS[kk]), msg)
  }, ii = i, kk = k_bad)
  msg <- "subscript i out of bounds"
  Map(function(ii, kk) {
    expect_error(xx[ii, , kk, ], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = ii, k = kk), msg)
    expect_error(xx[as.character(ii), , LETTERS[kk]], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = as.character(ii),
                                         k = LETTERS[kk]), msg)
  }, ii = c(i_bad, i_bad), kk = c(k[1:2], k_bad))
})

test_that("subsetting a DSArray by (j, k) works or errors on bad input", {
  j <- list(1, 1:2, sample(ncol(xx)), rep(1:3, 10))
  k <- list(1, 1:6, sample(nslice(xx)), rep(1:6, 10))
  Map(function(jj, kk) {
    expect_true(dsa_identical_to_array(xx[, jj, kk], x[, jj, kk, drop = FALSE]))
    expect_identical(.extract_DSArray_subset(xx, j = jj, k = kk), xx[, jj, kk])
    expect_true(dsa_identical_to_array(xx[, letters[jj], LETTERS[kk]],
                                       x[, letters[jj], LETTERS[kk],
                                         drop = FALSE]))
    expect_identical(.extract_DSArray_subset(xx, j = letters[jj],
                                             k = LETTERS[kk]),
                     xx[, letters[jj], LETTERS[kk], ])
  }, jj = j, kk = k)
  j_bad <- list(ncol(xx) + 1, 1:(ncol(x) + 1))
  k_bad <- list(nslice(xx) + 1, 1:(nslice(xx) + 1))
  msg <- "subscript k out of bounds"
  Map(function(jj, kk) {
    expect_error(xx[, jj, kk, ], msg)
    expect_error(.extract_DSArray_subset(x = xx, j = jj, k = kk), msg)
    expect_error(xx[, letters[jj], LETTERS[kk]], msg)
    expect_error(.extract_DSArray_subset(x = xx, j = letters[jj],
                                         k = LETTERS[kk]), msg)
  }, jj = j, kk = k_bad)
  msg <- "subscript j out of bounds"
  Map(function(jj, kk) {
    expect_error(xx[, jj, kk, ], msg)
    expect_error(.extract_DSArray_subset(x = xx, j = jj, k = kk), msg)
    expect_error(xx[, letters[jj], LETTERS[kk]], msg)
    expect_error(.extract_DSArray_subset(x = xx, j = letters[jj],
                                         k = LETTERS[kk]), msg)
  }, jj = c(j_bad, j_bad), kk = c(k[1:2], k_bad))
})

test_that("subsetting a DSArray by (i, j, k) works or errors on bad input", {
  i <- list(1, 1:10, sample(nrow(xx)), rep(1:10, 10))
  j <- list(1, 1:2, sample(ncol(xx)), rep(1:3, 10))
  k <- list(1, 1:6, sample(nslice(xx)), rep(1:6, 10))
  Map(function(ii, jj, kk) {
    expect_true(dsa_identical_to_array(xx[ii, jj, kk],
                                       x[ii, jj, kk, drop = FALSE]))
    expect_identical(.extract_DSArray_subset(xx, i = ii, j = jj, k = kk),
                     xx[ii, jj, kk])
    expect_true(dsa_identical_to_array(
      xx[as.character(ii), letters[jj], LETTERS[kk]],
      x[as.character(ii), letters[jj], LETTERS[kk], drop = FALSE]))
    expect_identical(.extract_DSArray_subset(xx, i = as.character(ii),
                                             j = letters[jj],
                                             k = LETTERS[kk]),
                     xx[as.character(ii), letters[jj], LETTERS[kk], ])
  }, ii = i, jj = j, kk = k)
  i_bad <- list(nrow(xx) + 1, 1:(nrow(xx) + 1))
  j_bad <- list(ncol(xx) + 1, 1:(ncol(xx) + 1))
  k_bad <- list(nslice(xx) + 1, 1:(nslice(xx) + 1))
  msg <- "subscript i out of bounds"
  Map(function(ii, jj, kk) {
    expect_error(xx[ii, jj, kk, ], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = ii, j = jj, k = kk), msg)
    expect_error(xx[as.character(ii), letters[jj], LETTERS[kk]], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = as.character(ii),
                                         j = letters[jj],
                                         k = LETTERS[kk]),
                 msg)
  }, ii = c(i_bad, i_bad, i_bad, i_bad), jj = c(j[1:2], j_bad, j[1:2], j_bad),
  kk = c(k[1:2], k[1:2], k_bad, k_bad))
  msg <- "subscript j out of bounds"
  Map(function(ii, jj, kk) {
    expect_error(xx[ii, jj, kk, ], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = ii, j = jj, k = kk), msg)
    expect_error(xx[as.character(ii), letters[jj], LETTERS[kk]], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = as.character(ii),
                                         j = letters[jj],
                                         k = LETTERS[kk]),
                 msg)
  }, ii = c(i[1:2], i[1:2]), jj = c(j_bad, j_bad), kk = c(k[1:2], k_bad))
  msg <- "subscript k out of bounds"
  Map(function(ii, jj, kk) {
    expect_error(xx[ii, jj, kk, ], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = ii, j = jj, k = kk), msg)
    expect_error(xx[as.character(ii), letters[jj], LETTERS[kk]], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = as.character(ii),
                                         j = letters[jj],
                                         k = LETTERS[kk]),
                 msg)
  }, ii = i[1:2], jj = j[1:2], kk = k_bad)
})

context("[<-,DSArray-method")

test_that("Warning is emitted due to non-optimised method", {
  msg <- "Densifying. This can cause a large increase in memory usage"
  expect_warning(xx[1, , ] <- xx[2, , ], msg)
})

test_that("replacing a DSArray by i works or errors on bad input", {
  i <- list(1, 1:10, sample(nrow(xx)), rep(1:10, 10))
  y <- x
  yy <- xx
  lapply(i, function(ii) {
    v <- array(-99, dim = list(length(ii), ncol(xx), nslice(xx)))
    vv <- DSArray(v, MARGIN = 2L)
    x <- y
    x[ii, , ] <- v
    xx <- yy
    suppressWarnings(xx[ii, , ] <- vv)
    expect_true(dsa_identical_to_array(xx, x))
    xx <- yy
    suppressWarnings(xx[ii, , ] <- vv)
    xxx <- yy
    suppressWarnings(xxx <- .replace_DSArray_subset(x = xxx,
                                                    i = ii,
                                                    value = vv))
    expect_identical(xx, xxx)
    x <- y
    x[as.character(ii), , ] <- v
    xx <- yy
    suppressWarnings(xx[as.character(ii), , ] <- vv)
    expect_true(dsa_identical_to_array(xx, x))
    xx <- yy
    suppressWarnings(xx[as.character(ii), , ] <- vv)
    xxx <- yy
    suppressWarnings(xxx <- .replace_DSArray_subset(x = xx,
                                                    i = as.character(ii),
                                                    value = vv))
    expect_identical(xx, xxx)
  })
  i_bad <- list(nrow(xx) + 1, 1:(nrow(xx) + 1))
  msg <- "subscript i out of bounds"
  lapply(i_bad, function(ii) {
    expect_error(xx[ii, , ], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = ii), msg)
    expect_error(xx[as.character(ii), , ], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = as.character(ii)), msg)
  })
})

test_that("replacing a DSArray by j works or errors on bad input", {
  j <- list(1, 1:2, sample(ncol(xx)), rep(1:3, 10))
  y <- x
  yy <- xx
  lapply(j, function(jj) {
    v <- array(-99, dim = list(nrow(xx), length(jj), nslice(xx)))
    vv <- DSArray(v, MARGIN = 2L)
    x <- y
    x[, jj, ] <- v
    xx <- yy
    suppressWarnings(xx[, jj, ] <- vv)
    expect_true(dsa_identical_to_array(xx, x))
    xx <- yy
    suppressWarnings(xx[, jj, ] <- vv)
    xxx <- yy
    suppressWarnings(xxx <- .replace_DSArray_subset(x = xxx,
                                                    j = jj,
                                                    value = vv))
    expect_identical(xx, xxx)
    x <- y
    x[, letters[jj], ] <- v
    xx <- yy
    suppressWarnings(xx[, letters[jj], ] <- vv)
    expect_true(dsa_identical_to_array(xx, x))
    xx <- yy
    suppressWarnings(xx[, letters[jj], ] <- vv)
    xxx <- yy
    suppressWarnings(xxx <- .replace_DSArray_subset(x = xx,
                                                    j = letters[jj],
                                                    value = vv))
    expect_identical(xx, xxx)
  })
  j_bad <- list(ncol(xx) + 1, 1:(ncol(xx) + 1))
  msg <- "subscript j out of bounds"
  lapply(j_bad, function(jj) {
    expect_error(xx[, jj, ], msg)
    expect_error(.extract_DSArray_subset(x = xx, j = jj), msg)
    expect_error(xx[, letters[jj], ], msg)
    expect_error(.extract_DSArray_subset(x = xx, j = letters[jj]), msg)
  })
})

test_that("replacing a DSArray by k works or errors on bad input", {
  k <- list(1, 1:6, sample(nslice(xx)), rep(1:6, 10))
  y <- x
  yy <- xx
  lapply(k, function(kk) {
    v <- array(-99, dim = list(nrow(xx), ncol(xx), length(kk)))
    vv <- DSArray(v, MARGIN = 2L)
    x <- y
    x[, , kk] <- v
    xx <- yy
    suppressWarnings(xx[, , kk] <- vv)
    expect_true(dsa_identical_to_array(xx, x))
    xx <- yy
    suppressWarnings(xx[, , kk] <- vv)
    xxx <- yy
    suppressWarnings(xxx <- .replace_DSArray_subset(x = xxx,
                                                    k = kk,
                                                    value = vv))
    expect_identical(xx, xxx)
    x <- y
    x[, , LETTERS[kk]] <- v
    xx <- yy
    suppressWarnings(xx[, , LETTERS[kk]] <- vv)
    expect_true(dsa_identical_to_array(xx, x))
    xx <- yy
    suppressWarnings(xx[, , LETTERS[kk]] <- vv)
    xxx <- yy
    suppressWarnings(xxx <- .replace_DSArray_subset(x = xx,
                                                    k = LETTERS[kk],
                                                    value = vv))
    expect_identical(xx, xxx)
  })
  k_bad <- list(nslice(xx) + 1, 1:(nslice(xx) + 1))
  msg <- "subscript k out of bounds"
  lapply(k_bad, function(kk) {
    expect_error(xx[, , kk], msg)
    expect_error(.extract_DSArray_subset(x = xx, k = kk), msg)
    expect_error(xx[, , LETTERS[kk]], msg)
    expect_error(.extract_DSArray_subset(x = xx, k = LETTERS[kk]), msg)
  })
})

test_that("replacing a DSArray by (i, j) works or errors on bad input", {
  i <- list(1, 1:10, sample(nrow(xx)), rep(1:10, 10))
  j <- list(1, 1:2, sample(ncol(xx)), rep(1:3, 10))
  y <- x
  yy <- xx
  Map(function(ii, jj) {
    v <- array(-99, dim = list(length(ii), length(jj), nslice(xx)))
    vv <- DSArray(v, MARGIN = 2L)
    x <- y
    x[ii, jj, ] <- v
    xx <- yy
    suppressWarnings(xx[ii, jj, ] <- vv)
    expect_true(dsa_identical_to_array(xx, x))
    xx <- yy
    suppressWarnings(xx[ii, jj, ] <- vv)
    xxx <- yy
    suppressWarnings(xxx <- .replace_DSArray_subset(x = xxx,
                                                    i = ii,
                                                    j = jj,
                                                    value = vv))
    expect_identical(xx, xxx)
    x <- y
    x[as.character(ii), letters[jj], ] <- v
    xx <- yy
    suppressWarnings(xx[as.character(ii), letters[jj], ] <- vv)
    expect_true(dsa_identical_to_array(xx, x))
    xx <- yy
    suppressWarnings(xx[as.character(ii), letters[jj], ] <- vv)
    xxx <- yy
    suppressWarnings(xxx <- .replace_DSArray_subset(x = xx,
                                                    i = as.character(ii),
                                                    j = letters[jj],
                                                    value = vv))
    expect_identical(xx, xxx)
  }, ii = i, jj = j)
  i_bad <- list(nrow(xx) + 1, 1:(nrow(xx) + 1))
  j_bad <- list(ncol(xx) + 1, 1:(ncol(xx) + 1))
  msg <- "subscript i out of bounds"
  Map(function(ii, jj) {
    expect_error(xx[ii, jj, ], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = ii, j = jj), msg)
    expect_error(xx[as.character(ii), letters[jj], ], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = as.character(ii),
                                         j = letters[jj]), msg)
  }, ii = c(i_bad, i_bad), jj = c(j[1:2], j_bad))
  msg <- "subscript j out of bounds"
  Map(function(ii, jj) {
    expect_error(xx[ii, jj, ], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = ii, j = jj), msg)
    expect_error(xx[as.character(ii), letters[jj], ], msg)
    expect_error(.extract_DSArray_subset(x = xx,
                                         i = as.character(ii),
                                         j = letters[jj]), msg)
  }, ii = i, jj = j_bad)
})

test_that("replacing a DSArray by (i, k) works or errors on bad input", {
  i <- list(1, 1:10, sample(nrow(xx)), rep(1:10, 10))
  k <- list(1, 1:6, sample(nslice(xx)), rep(1:6, 10))
  y <- x
  yy <- xx
  Map(function(ii, kk) {
    v <- array(-99, dim = list(length(ii), ncol(xx), length(kk)))
    vv <- DSArray(v, MARGIN = 2L)
    x <- y
    x[ii, , kk] <- v
    xx <- yy
    suppressWarnings(xx[ii, , kk] <- vv)
    expect_true(dsa_identical_to_array(xx, x))
    xx <- yy
    suppressWarnings(xx[ii, , kk] <- vv)
    xxx <- yy
    suppressWarnings(xxx <- .replace_DSArray_subset(x = xxx,
                                                    i = ii,
                                                    k = kk,
                                                    value = vv))
    expect_identical(xx, xxx)
    x <- y
    x[as.character(ii), , LETTERS[kk]] <- v
    xx <- yy
    suppressWarnings(xx[as.character(ii), , LETTERS[kk]] <- vv)
    expect_true(dsa_identical_to_array(xx, x))
    xx <- yy
    suppressWarnings(xx[as.character(ii), , LETTERS[kk]] <- vv)
    xxx <- yy
    suppressWarnings(xxx <- .replace_DSArray_subset(x = xx,
                                                    i = as.character(ii),
                                                    k = LETTERS[kk],
                                                    value = vv))
    expect_identical(xx, xxx)
  }, ii = i, kk = k)
  i_bad <- list(nrow(xx) + 1, 1:(nrow(xx) + 1))
  k_bad <- list(nslice(xx) + 1, 1:(nslice(xx) + 1))
  msg <- "subscript i out of bounds"
  Map(function(ii, kk) {
    expect_error(xx[ii, , kk], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = ii, k = kk), msg)
    expect_error(xx[as.character(i), , LETTERS[kk]], msg)
    expect_error(.extract_DSArray_subset(x = xx,
                                         i = as.character(i),
                                         k = LETTERS[kk]), msg)
  }, ii = c(i_bad, i_bad), kk = c(k[1:2], k_bad))
  msg <- "subscript k out of bounds"
  Map(function(ii, kk) {
    expect_error(xx[ii, , kk], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = ii, k = kk), msg)
    expect_error(xx[as.character(ii), , LETTERS[kk]], msg)
    expect_error(.extract_DSArray_subset(x = xx,
                                         i = as.character(ii),
                                         k = LETTERS[kk]), msg)
  }, ii = i, kk = k_bad)
})

test_that("replacing a DSArray by (j, k) works or errors on bad input", {
  j <- list(1, 1:2, sample(ncol(xx)), rep(1:3, 10))
  k <- list(1, 1:6, sample(nslice(xx)), rep(1:6, 10))
  y <- x
  yy <- xx
  Map(function(jj, kk) {
    v <- array(-99, dim = list(nrow(xx), length(jj), length(kk)))
    vv <- DSArray(v, MARGIN = 2L)
    x <- y
    x[, jj, kk] <- v
    xx <- yy
    suppressWarnings(xx[, jj, kk] <- vv)
    expect_true(dsa_identical_to_array(xx, x))
    xx <- yy
    suppressWarnings(xx[, jj, kk] <- vv)
    xxx <- yy
    suppressWarnings(xxx <- .replace_DSArray_subset(x = xxx,
                                                    j = jj,
                                                    k = kk,
                                                    value = vv))
    expect_identical(xx, xxx)
    x <- y
    x[, letters[jj], LETTERS[kk]] <- v
    xx <- yy
    suppressWarnings(xx[, letters[jj], LETTERS[kk]] <- vv)
    expect_true(dsa_identical_to_array(xx, x))
    xx <- yy
    suppressWarnings(xx[, letters[jj], LETTERS[kk]] <- vv)
    xxx <- yy
    suppressWarnings(xxx <- .replace_DSArray_subset(x = xx,
                                                    j = letters[jj],
                                                    k = LETTERS[kk],
                                                    value = vv))
    expect_identical(xx, xxx)
  }, jj = j, kk = k)
  j_bad <- list(ncol(xx) + 1, 1:(ncol(xx) + 1))
  k_bad <- list(nslice(xx) + 1, 1:(nslice(xx) + 1))
  msg <- "subscript j out of bounds"
  Map(function(jj, kk) {
    expect_error(xx[, jj, kk], msg)
    expect_error(.extract_DSArray_subset(x = xx, j = jj, k = kk), msg)
    expect_error(xx[, letters[jj], LETTERS[kk]], msg)
    expect_error(.extract_DSArray_subset(x = xx,
                                         j = letters[jj],
                                         k = LETTERS[kk]), msg)
  }, jj = c(j_bad, j_bad), kk = c(k[1:2], k_bad))
  msg <- "subscript k out of bounds"
  Map(function(jj, kk) {
    expect_error(xx[, jj, kk], msg)
    expect_error(.extract_DSArray_subset(x = xx, j = jj, k = kk), msg)
    expect_error(xx[, letters[jj], LETTERS[kk]], msg)
    expect_error(.extract_DSArray_subset(x = xx,
                                         j = letters[jj],
                                         k = LETTERS[kk]), msg)
  }, jj = j, kk = k_bad)
})

# UP TO HERE
test_that("replacing a DSArray by (i, j, k) works or errors on bad input", {
  i <- list(1, 1:10, sample(nrow(xx)), rep(1:10, 10))
  j <- list(1, 1:2, sample(ncol(xx)), rep(1:3, 10))
  k <- list(1, 1:6, sample(nslice(xx)), rep(1:6, 10))
  y <- x
  yy <- xx
  Map(function(ii, jj, kk) {
    v <- array(-99, dim = list(length(ii), length(jj), length(kk)))
    vv <- DSArray(v, MARGIN = 2L)
    x <- y
    x[ii, jj, kk] <- v
    xx <- yy
    suppressWarnings(xx[ii, jj, kk] <- vv)
    expect_true(dsa_identical_to_array(xx, x))
    xx <- yy
    suppressWarnings(xx[ii, jj, kk] <- vv)
    xxx <- yy
    suppressWarnings(xxx <- .replace_DSArray_subset(x = xxx,
                                                    i = ii,
                                                    j = jj,
                                                    k = kk,
                                                    value = vv))
    expect_identical(xx, xxx)
    x <- y
    x[as.character(ii), letters[jj], LETTERS[kk]] <- v
    xx <- yy
    suppressWarnings(xx[as.character(ii), letters[jj], LETTERS[kk]] <- vv)
    expect_true(dsa_identical_to_array(xx, x))
    xx <- yy
    suppressWarnings(xx[as.character(ii), letters[jj], LETTERS[kk]] <- vv)
    xxx <- yy
    suppressWarnings(xxx <- .replace_DSArray_subset(x = xx,
                                                    i = as.character(ii),
                                                    j = letters[jj],
                                                    k = LETTERS[kk],
                                                    value = vv))
    expect_identical(xx, xxx)
  }, ii = i, jj = j, kk = k)
  i_bad <- list(nrow(xx) + 1, 1:(nrow(xx) + 1))
  j_bad <- list(ncol(xx) + 1, 1:(ncol(xx) + 1))
  k_bad <- list(nslice(xx) + 1, 1:(nslice(xx) + 1))
  msg <- "subscript i out of bounds"
  Map(function(ii, jj, kk) {
    expect_error(xx[ii, jj, kk], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = ii, j = jj, k = kk), msg)
    expect_error(xx[as.character(ii), letters[jj], LETTERS[kk]], msg)
    expect_error(.extract_DSArray_subset(x = xx,
                                         i = as.character(ii),
                                         j = letters[jj],
                                         k = LETTERS[kk]), msg)
  }, ii = c(i_bad, i_bad, i_bad, i_bad), jj = c(j[1:2], j_bad, j[1:2], j_bad),
  kk = c(k[1:2], k[1:2], k_bad, k_bad))
  msg <- "subscript j out of bounds"
  Map(function(ii, jj, kk) {
    expect_error(xx[ii, jj, kk], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = ii, j = jj, k = kk), msg)
    expect_error(xx[as.character(ii), letters[jj], LETTERS[kk]], msg)
    expect_error(.extract_DSArray_subset(x = xx,
                                         i = as.character(ii),
                                         j = letters[jj],
                                         k = LETTERS[kk]), msg)
  }, ii = c(i[1:2], i[1:2]), jj = c(j_bad, j_bad), kk = c(k[1:2], k_bad))
  msg <- "subscript k out of bounds"
  Map(function(ii, jj, kk) {
    expect_error(xx[ii, jj, kk], msg)
    expect_error(.extract_DSArray_subset(x = xx, i = ii, j = jj, k = kk), msg)
    expect_error(xx[as.character(ii), letters[jj], LETTERS[kk]], msg)
    expect_error(.extract_DSArray_subset(x = xx,
                                         i = as.character(ii),
                                         j = letters[jj],
                                         k = LETTERS[kk]), msg)
  }, ii = i, jj = j, kk = k_bad)
})

# UP TO HERE: Not working as expected
test_that("Errors if no subcript supplied", {
  msg <- "Please provide at least one 'i', 'j', or 'k'"
  yy <- xx
  expect_error(yy[] <- xx, msg)
})

