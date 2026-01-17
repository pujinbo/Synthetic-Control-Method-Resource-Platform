# tests/testthat/test-scm_core.R

test_that("scm returns correct structure", {
  # prepare the test data
  Y <- matrix(rnorm(50), nrow = 5, ncol = 10)

  # run functions
  result <- scm(Y)

  # verify
  expect_type(result, "list")
  expect_named(result, c("a", "b"))
  expect_length(result$b, 5)
  expect_equal(result$b[1], 0)  # treated unit weight should be 0
  expect_equal(sum(result$b[-1]), 1, tolerance = 1e-6)  # The sum of the weights is 1.
})

test_that("scm fails with insufficient units", {
  Y <- matrix(1:10, nrow = 1)  # only one unit
  expect_error(scm(Y), "Need at least 2 units")
})

test_that("scm_batch processes all units", {
  Y <- matrix(rnorm(100), nrow = 10, ncol = 10)
  result <- scm_batch(Y)

  expect_equal(dim(result$B), c(10, 10))
  expect_length(result$a, 10)
})


