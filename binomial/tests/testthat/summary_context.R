context("Test for summary measures")

test_that("mean works as expected", {
  expect_length(aux_mean(10,0.3),1)
  expect_equal(aux_mean(10,0.3),10*0.3)
  expect_error(aux_mean(10,0.3,0.8))
})

test_that("variance works as expected", {
  expect_length(aux_variance(10,0.3),1)
  expect_equal(aux_variance(10,0.3),10*0.3*(1-0.3))
  expect_error(aux_variance(10,0.3,0.8))
})

test_that("mode works as expected", {
  expect_length(aux_mode(10,0.3),1)
  expect_equal(aux_mode(9,0.3),2)
  expect_error(aux_mode(10,0.3,0.8))
})

test_that("skewness works as expected", {
  expect_length(aux_skewness(10,0.3),1)
  expect_equal(aux_skewness(10,0.3),(1-2*0.3)/sqrt(10*0.3*(1-0.3)))
  expect_error(aux_skewness(10,0.3,0.8))
})

test_that("kurtosis works as expected", {
  expect_length(aux_kurtosis(10,0.3),1)
  expect_equal(aux_kurtosis(10,0.3),(1-6*0.3*(1-0.3))/(10*0.3*(1-0.3)))
  expect_error(aux_kurtosis(10,0.3,0.8))
})
