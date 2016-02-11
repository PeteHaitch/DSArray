### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Math
###

context("Math group generic")

test_that("DSArray,Math ops that don't produce any warnings work", {
  ops <- c(`abs`, `sign`, `ceiling`, `floor`, `trunc`, `asinh`, `atan`,
           `exp`, `expm1`,  `cos`, `cosh`, `cospi`, `sin`, `sinh`, `sinpi`,
           `tan`, `tanh`, `tanpi`, `trigamma`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(op(xx), op(x)))
    expect_true(dsa_identical_to_array(op(xx * 0.666), op(x * 0.666)))
    expect_true(dsa_identical_to_array(op(-1 * xx), op(-1 * x)))
    expect_true(dsa_identical_to_array(op(-1 * xx * 0.666), op(-1 * x * 0.666)))
  })
})
test_that("DSArray,Math ops that produce warnings on negative input work", {
  ops <- c(`sqrt`, `log`, `log10`, `log2`, `log1p`, `lgamma`,
           `digamma`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(op(xx), op(x)))
    expect_true(dsa_identical_to_array(op(xx * 0.666), op(x * 0.666)))
    expect_true(dsa_identical_to_array(suppressWarnings(op(-1 * xx)),
                                       suppressWarnings(op(-1 * x))))
    expect_true(dsa_identical_to_array(suppressWarnings(op(-1 * xx * 0.666)),
                                       suppressWarnings(op(-1 * x * 0.666))))
    expect_warning(op(-1 * xx), "[NaNs produced | value out of range]")
  })
})

test_that("DSArray,Math ops that produce warnings on input outside of [1, Inf) work", {
  ops <- c(`acosh`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(op(xx), op(x)))
    expect_true(dsa_identical_to_array(suppressWarnings(op(xx * 0.666)),
                                       suppressWarnings(op(x * 0.666))))
    expect_true(dsa_identical_to_array(suppressWarnings(op(-1 * xx)),
                                       suppressWarnings(op(-1 * x))))
    expect_warning(op(xx * 0.666), "[NaNs produced | value out of range]")
    expect_warning(op(-1 * xx), "[NaNs produced | value out of range]")
  })
})


test_that("DSArray,Math ops that produce warnings on input outside of [-1, 1] work", {
  ops <- c(`acos`, `asin`, `atanh`)
  lapply(ops, function(op) {
    # TODO: Requires max,DSArray method
    # expect_true(dsa_identical_to_array(op(xx / max(xx)), op(x / max(x))))
    # expect_true(dsa_identical_to_array(op(-1 * xx / max(xx)),
    #                                    op(-1 * x / max(x))))
    expect_warning(op(xx), "NaNs produced")
    expect_warning(op(-1 * xx), "NaNs produced")
  })
})

test_that("DSArray,Math ops that produce warnings on negative input and on large-ish positive input work", {
  ops <- c(`gamma`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(suppressWarnings(op(xx)),
                                       suppressWarnings(op(x))))
    expect_warning(op(xx), "value out of range in 'gammafn'")
    expect_true(dsa_identical_to_array(suppressWarnings(op(-1 * xx)),
                                       suppressWarnings(op(-1 * x))))
    expect_warning(op(-1 * xx), "NaNs produced")
  })
})

test_that("DSArray,cum* ops work", {
  # NOTE: For some reason the above strategy doesn't work for `cummax`,
  #       `cummin`, `cumprod`, and `cumsum`. I think it is because of how these
  #       are defined separately from the other Math group methods.
  expect_identical(suppressWarnings(cummax(xx)), cummax(x))
  expect_identical(suppressWarnings(cummax(xx * 0.666)), cummax(x * 0.666))
  expect_identical(suppressWarnings(cummax(-1 * xx)), cummax(-1 * x))
  expect_identical(suppressWarnings(cummax(-1 * xx * 0.666)),
                   cummax(-1 * x * 0.666))
  expect_identical(suppressWarnings(cummin(xx)), cummin(x))
  expect_identical(suppressWarnings(cummin(xx * 0.666)), cummin(x * 0.666))
  expect_identical(suppressWarnings(cummin(-1 * xx)), cummin(-1 * x))
  expect_identical(suppressWarnings(cummin(-1 * xx * 0.666)),
                   cummin(-1 * x * 0.666))
  expect_identical(suppressWarnings(cumprod(xx)), cumprod(x))
  expect_identical(suppressWarnings(cumprod(xx * 0.666)), cumprod(x * 0.666))
  expect_identical(suppressWarnings(cumprod(-1 * xx)), cumprod(-1 * x))
  expect_identical(suppressWarnings(cumprod(-1 * xx * 0.666)),
                   cumprod(-1 * x * 0.666))
  expect_identical(suppressWarnings(cumsum(xx)), cumsum(x))
  expect_identical(suppressWarnings(cumsum(xx * 0.666)), cumsum(x * 0.666))
  expect_identical(suppressWarnings(cumsum(-1 * xx)), cumsum(-1 * x))
  expect_identical(suppressWarnings(cumsum(-1 * xx * 0.666)),
                   cumsum(-1 * x * 0.666))
  expect_warning(cummax(xx), paste0("Densifying 'x'. This can cause a large ",
                                    "increase in memory usage."))
  expect_warning(cummin(xx), paste0("Densifying 'x'. This can cause a large ",
                                    "increase in memory usage."))
  expect_warning(cumprod(xx), paste0("Densifying 'x'. This can cause a large ",
                                    "increase in memory usage."))
  expect_warning(cumsum(xx), paste0("Densifying 'x'. This can cause a large ",
                                    "increase in memory usage."))
})
