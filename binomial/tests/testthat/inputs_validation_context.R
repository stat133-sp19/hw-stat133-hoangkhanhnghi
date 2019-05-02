context("Test for valid inputs") 

test_that("input probability works as expected", {
  expect_error(check_prob(2))
  expect_true(check_prob(0.5))
  expect_length(check_prob(0.3),1)
})

test_that("input trials works as expected", {
  expect_error(check_trials(-2))
  expect_true(check_trials(6))
  expect_length(check_trials(8),1)
})

test_that("input successes works as expected", {
  expect_error(check_success(-2,5))
  expect_true(check_success(4,5))
  expect_error(check_success(5,3))
})