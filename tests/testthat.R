library(testthat)

context("frecuentist known function")

library(serosurvey)

# test_check("serosurvey")

test_that(desc = "test rogan gladen estimator",code = {
  expect_equal(object = rogan_gladen_estimator(prev.obs = 0.50,Se = 0.60,Sp = 0.90),
               expected = 0.8)
})
