
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Arith
###

context("Arith group generic")

test_that("c(\"DSarray\", \"vector\") signature works", {
  ops <- c(`+`, `-`, `*`, `%%`, `%/%`, `/`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(op(xx, 3), op(x, 3)))
  })
  # NOTE: For some reason the above strategy doesn't work for `^`. I think it
  #       is because of how `^` is defined separately from the other Ops.
  expect_true(dsa_identical_to_array(xx ^ 3, x ^ 3))
})

test_that("c(\"vector\", \"DSArray\") signature works", {
  ops <- c(`+`, `-`, `*`, `%%`, `%/%`, `/`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(op(3, xx), op(3, x)))
  })
  # NOTE: For some reason the above strategy doesn't work for `^`. I think it
  #       is because of how `^` is defined separately from the other Ops.
  expect_true(dsa_identical_to_array(3 ^ xx, 3 ^ x))
})

test_that("Only 'scalar' Arith ops are currently implemented", {
  ops <- c(`+`, `-`, `*`, `%%`, `%/%`, `/`)
  lapply(ops, function(op) {
    expect_error(op(xx, 1:10),
                 paste0("length\\(e2\\) > 1 behaviour is complicated and not ",
                        "yet implemented"))
    expect_error(op(1:10, xx),
                 paste0("length\\(e1\\) > 1 behaviour is complicated and not ",
                        "yet implemented"))
  })
  # NOTE: For some reason the above strategy doesn't work for `^`. I think it
  #       is because of how `^` is defined separately from the other Ops.
  expect_error(xx ^ (1:10),
               paste0("length\\(e2\\) > 1 behaviour is complicated and not ",
                      "yet implemented"))
  expect_error((1:10) ^ xx,
               paste0("length\\(e1\\) > 1 behaviour is complicated and not ",
                      "yet implemented"))
})

test_that("c(\"DSArray\", \"DSArray\") signature not yet implemented", {
  ops <- c(`+`, `-`, `*`, `%%`, `%/%`, `/`)
  lapply(ops, function(op) {
    expect_error(op(xx, xx), "non-numeric argument to binary operator")
  })
  # NOTE: For some reason the above strategy doesn't work for `^`. I think it
  #       is because of how `^` is defined separately from the other Ops.
    expect_error(xx ^ xx, "non-numeric argument to binary operator")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Compare
###

context("Compare group generic")

test_that("c(\"DSarray\", \"vector\") signature works", {
  ops <- c(`==`, `>`, `<`, `!=`, `<=`, `>=`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(op(xx, 3), op(x, 3)))
    expect_true(dsa_identical_to_array(op(xx, "kraken"), op(x, "kraken")))
    expect_true(dsa_identical_to_array(op(xx, FALSE), op(x, FALSE)))
  })
})

test_that("c(\"vector\", \"DSArray\") signature works", {
  ops <- c(`==`, `>`, `<`, `!=`, `<=`, `>=`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(op(3, xx), op(3, x)))
    expect_true(dsa_identical_to_array(op("kraken", xx), op("kraken", x)))
    expect_true(dsa_identical_to_array(op(FALSE, xx), op(FALSE, x)))
  })
})

test_that("Only scalar Compare ops are currently implemented", {
  ops <- c(`==`, `>`, `<`, `!=`, `<=`, `>=`)
  lapply(ops, function(op) {
    expect_error(op(xx, 1:10),
                 paste0("length\\(e2\\) > 1 behaviour is complicated and not ",
                        "yet implemented"))
    expect_error(op(1:10, xx),
                 paste0("length\\(e1\\) > 1 behaviour is complicated and not ",
                        "yet implemented"))
  })
})

test_that("c(\"DSArray\", \"DSArray\") signature not yet implemented", {
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

test_that("c(\"DSarray\", \"vector\") signature works", {
  ops <- c(`&`, `|`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(op(xx, 3), op(x, 3)))
    expect_true(dsa_identical_to_array(op(xx, FALSE), op(x, FALSE)))
    expect_error(op(xx, "kraken"),
                 paste0("operations are possible only for numeric, logical or ",
                        "complex types"))
  })
})

test_that("c(\"vector\", \"DSArray\") signature works", {
  ops <- c(`&`, `|`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(op(3, xx), op(3, x)))
    expect_true(dsa_identical_to_array(op(FALSE, xx), op(FALSE, x)))
    expect_error(op("kraken", xx),
                 paste0("operations are possible only for numeric, logical or ",
                        "complex types"))
  })
})

test_that("Only scalar Logic ops are currently implemented", {
  ops <- c(`&`, `|`)
  lapply(ops, function(op) {
    expect_error(op(xx, 1:10),
                 paste0("length\\(e2\\) > 1 behaviour is complicated and not ",
                        "yet implemented"))
    expect_error(op(1:10, xx),
                 paste0("length\\(e1\\) > 1 behaviour is complicated and not ",
                        "yet implemented"))
  })
})

test_that("c(\"DSArray\", \"DSArray\") signature not yet implemented", {
  ops <- c(`&`, `|`)
  lapply(ops, function(op) {
    expect_error(op(xx, xx),
                 paste0("operations are possible only for numeric, logical or ",
                        "complex types"))
  })
})