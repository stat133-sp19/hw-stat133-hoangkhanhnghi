context("Test for binomial")

test_that("Binomial choose works as expected", {
  expect_length(bin_choose(10,5),1)
  expect_equal(bin_choose(10,5),factorial(10)/(factorial(5)*factorial(10-5)))
  expect_error(bin_choose(5,10))
})

test_that("Binomial probability works as expected", {
  expect_length(bin_probability(0:2,10,0.3),3)
  expect_equal(bin_probability(5,10,0.3),bin_choose(10,5)*0.3^5*(1-0.3)^5)
  expect_error(bin_probability(5,c(10,5),0.3))
})

test_that("Binomial distribution works as expected", {
  expect_is(bin_distribution(10,0.3),c("binvar","data.frame"))
  expect_length(bin_distribution(10,0.3),2)
  expect_error(bin_distribution(5,10,0.3))
})

test_that("Binomial cumulative works as expected", {
  expect_is(bin_cumulative(10,0.5),c("bincum","data.frame"))
  expect_length(bin_cumulative(10,0.5),3)
  expect_error(bin_cumulative(5,10,0.3))
})