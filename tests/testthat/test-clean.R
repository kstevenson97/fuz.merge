context("clean function")

test_that("output correct structure", {
  data(congress)
  result <- clean(congress, name, selected = ",", prefixes = T, suffixes = T)
  expect_is(result, "data.frame", info = "data frame output")

})

test_that("output correct answer", {
  data(congress)
  hashsaved <- "e4cecadf03ada716b03833e6a99526a3"
  result <- clean(congress, name, selected = ",", prefixes = T, suffixes = T)
  hash <- digest::digest(result)
  expect_equal(hash, hashsaved)
})
