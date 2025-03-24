
test_that("myncurve returns correct structure", {
  result <- myncurve(mu = 50, sigma = 10, a = 60)

  # output is a list
  expect_type(result, "list")

  # list has the correct names
  expect_named(result, c("mu", "sigma", "a", "probability"))
})

