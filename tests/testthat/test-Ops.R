### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Arith
###

context("Arith group generic")

test_that("Arith,DSarray,vector-method works", {
  ops <- c(`+`, `-`, `*`, `^`, `%%`, `%/%`, `/`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(op(xx, 3), op(x, 3)))
  })
})

test_that("Arith,DSarray,vector-method warns of densifying if length(e2) > 1", {
  ops <- c(`+`, `-`, `*`, `^`, `%%`, `%/%`, `/`)
  lapply(ops, function(op) {
    expect_warning(op(xx, seq_along(xx)),
                   paste0("Densifying. This can cause a large increase in ",
                          "memory usage"))
    expect_true(dsa_identical_to_array(suppressWarnings(op(xx, seq_along(xx))),
                                       suppressWarnings(op(x, seq_along(x)))))
    expect_warning(op(xx, 1:7),
                   paste0("Densifying. This can cause a large increase in ",
                          "memory usage"))
    expect_warning(op(xx, 1:7),
                   paste0("longer object length is not a multiple of shorter ",
                          "object length"))
    expect_true(dsa_identical_to_array(suppressWarnings(op(xx, 1:7)),
                                       suppressWarnings(op(x, 1:7))))
  })
})

test_that("Arith,vector,DSarray-method works", {
  ops <- c(`+`, `-`, `*`, `^`, `%%`, `%/%`, `/`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(op(3, xx), op(3, x)))
    expect_true(dsa_identical_to_array(op(3, zz), op(3, z)))
  })
})

test_that("Arith,DSarray,vector-method warns if length(e2) > 1", {
  ops <- c(`+`, `-`, `*`, `^`, `%%`, `%/%`, `/`)
  lapply(ops, function(op) {
    expect_warning(op(seq_along(xx), xx),
                   paste0("Densifying. This can cause a large increase in ",
                          "memory usage"))
    expect_true(dsa_identical_to_array(suppressWarnings(op(seq_along(xx), xx)),
                                       suppressWarnings(op(seq_along(x), x))))
    expect_warning(op(1:7, xx),
                   paste0("Densifying. This can cause a large increase in ",
                          "memory usage"))
    expect_warning(op(1:7, xx),
                   paste0("longer object length is not a multiple of shorter ",
                          "object length"))
    expect_true(dsa_identical_to_array(suppressWarnings(op(1:7, xx)),
                                       suppressWarnings(op(1:7, x))))
  })
})

test_that("Arith,vector,DSArray-method signature works", {
  ops <- c(`+`, `-`, `*`, `^`, `%%`, `%/%`, `/`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(op(3, xx), op(3, x)))
  })
})

test_that("Arith,DSArray,DSArray-method signature not yet implemented", {
  ops <- c(`+`, `-`, `*`, `^`, `%%`, `%/%`, `/`)
  lapply(ops, function(op) {
    expect_error(op(xx, xx), "non-numeric argument to binary operator")
  })
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Compare
###

context("Compare group generic")

test_that("Compare,DSarray,vector-method works", {
  ops <- c(`==`, `>`, `<`, `!=`, `<=`, `>=`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(op(xx, 3), op(x, 3)))
    expect_true(dsa_identical_to_array(op(xx, "kraken"), op(x, "kraken")))
    expect_true(dsa_identical_to_array(op(xx, FALSE), op(x, FALSE)))
  })
})

test_that("Compare,DSarray,vector-method warns of densifying if length(e2) > 1", {
  ops <- c(`==`, `>`, `<`, `!=`, `<=`, `>=`)
  lapply(ops, function(op) {
    expect_warning(op(xx, seq_along(xx)),
                   paste0("Densifying. This can cause a large increase in ",
                          "memory usage"))
    expect_true(dsa_identical_to_array(suppressWarnings(op(xx, seq_along(xx))),
                                       suppressWarnings(op(x, seq_along(x)))))
    expect_warning(op(xx, 1:7),
                   paste0("Densifying. This can cause a large increase in ",
                          "memory usage"))
    expect_warning(op(xx, 1:7),
                   paste0("longer object length is not a multiple of shorter ",
                          "object length"))
    expect_true(dsa_identical_to_array(suppressWarnings(op(xx, 1:7)),
                                       suppressWarnings(op(x, 1:7))))
  })
})

test_that("Compare,vector,DSArray-method works", {
  ops <- c(`==`, `>`, `<`, `!=`, `<=`, `>=`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(op(3, xx), op(3, x)))
    expect_true(dsa_identical_to_array(op("kraken", xx), op("kraken", x)))
    expect_true(dsa_identical_to_array(op(FALSE, xx), op(FALSE, x)))
  })
})

test_that("Compare,DSarray,vector-method warns if length(e2) > 1", {
  ops <- c(`==`, `>`, `<`, `!=`, `<=`, `>=`)
  lapply(ops, function(op) {
    expect_warning(op(seq_along(xx), xx),
                   paste0("Densifying. This can cause a large increase in ",
                          "memory usage"))
    expect_true(dsa_identical_to_array(suppressWarnings(op(seq_along(xx), xx)),
                                       suppressWarnings(op(seq_along(x), x))))
    expect_warning(op(1:7, xx),
                   paste0("Densifying. This can cause a large increase in ",
                          "memory usage"))
    expect_warning(op(1:7, xx),
                   paste0("longer object length is not a multiple of shorter ",
                          "object length"))
    expect_true(dsa_identical_to_array(suppressWarnings(op(1:7, xx)),
                                       suppressWarnings(op(1:7, x))))
  })
})

test_that("Compare,DSArray,DSArrayn-method not yet implemented", {
  ops <- c(`==`, `>`, `<`, `!=`, `<=`, `>=`)
  lapply(ops, function(op) {
    expect_error(op(xx, xx),
                 paste0("comparison \\([0-9]\\) is possible only for atomic and ",
                        "list types"))
  })
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Logic
###

context("Logic group generic")

test_that("Logic,DSarraymvector-method works", {
  ops <- c(`&`, `|`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(op(xx, 3), op(x, 3)))
    expect_true(dsa_identical_to_array(op(xx, FALSE), op(x, FALSE)))
    expect_error(op(xx, "kraken"),
                 paste0("operations are possible only for numeric, logical or ",
                        "complex types"))
  })
})

test_that("Logic,DSarray,vector-method warns of densifying if length(e2) > 1", {
  ops <- c(`&`, `|`)
  lapply(ops, function(op) {
    expect_warning(op(xx, seq_along(xx)),
                   paste0("Densifying. This can cause a large increase in ",
                          "memory usage"))
    expect_true(dsa_identical_to_array(suppressWarnings(op(xx, seq_along(xx))),
                                       suppressWarnings(op(x, seq_along(x)))))
    expect_warning(op(xx, 1:7),
                   paste0("Densifying. This can cause a large increase in ",
                          "memory usage"))
    expect_warning(op(xx, 1:7),
                   paste0("longer object length is not a multiple of shorter ",
                          "object length"))
    expect_true(dsa_identical_to_array(suppressWarnings(op(xx, 1:7)),
                                       suppressWarnings(op(x, 1:7))))
  })
})

test_that("Logic,vector,DSArray-method works", {
  ops <- c(`&`, `|`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(op(3, xx), op(3, x)))
    expect_true(dsa_identical_to_array(op(FALSE, xx), op(FALSE, x)))
    expect_error(op("kraken", xx),
                 paste0("operations are possible only for numeric, logical or ",
                        "complex types"))
  })
})

test_that("Logic,DSarray,vector-method warns if length(e2) > 1", {
  ops <- c(`&`, `|`)
  lapply(ops, function(op) {
    expect_warning(op(seq_along(xx), xx),
                   paste0("Densifying. This can cause a large increase in ",
                          "memory usage"))
    expect_true(dsa_identical_to_array(suppressWarnings(op(seq_along(xx), xx)),
                                       suppressWarnings(op(seq_along(x), x))))
    expect_warning(op(1:7, xx),
                   paste0("Densifying. This can cause a large increase in ",
                          "memory usage"))
    expect_warning(op(1:7, xx),
                   paste0("longer object length is not a multiple of shorter ",
                          "object length"))
    expect_true(dsa_identical_to_array(suppressWarnings(op(1:7, xx)),
                                       suppressWarnings(op(1:7, x))))
  })
})


test_that("Logic,DSArray,DSArray-method not yet implemented", {
  ops <- c(`&`, `|`)
  lapply(ops, function(op) {
    expect_error(op(xx, xx),
                 paste0("operations are possible only for numeric, logical or ",
                        "complex types"))
  })
})