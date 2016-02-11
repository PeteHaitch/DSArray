### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Math2
###

context("Math2 group generic")

test_that("DSArray,Math2 work", {
  ops <- c(`round`, `signif`)
  lapply(ops, function(op) {
    expect_true(dsa_identical_to_array(op(xx), op(x)))
    expect_true(dsa_identical_to_array(op(xx * 0.666), op(x * 0.666)))
    expect_true(dsa_identical_to_array(op(-1 * xx), op(-1 * x)))
    expect_true(dsa_identical_to_array(op(-1 * xx + 0.666), op(-1 * x + 0.666)))
  })
})
