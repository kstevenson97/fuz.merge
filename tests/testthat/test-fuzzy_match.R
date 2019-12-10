context("fuzzy_match function")
 test_that("output correct structure", {
  data(congress)
  data(politwoops)
  result <- fuzzy_match(congress, politwoops, name, full_name, cutoff=.1, join_type = "inner")
  expect_is(result, "data.frame", info = "data frame output")
})

test_that("output correct answer", {
  data(congress)
  data(politwoops)
  hashsaved <- "10008d552cbc76d7c701a975f2abb007"
  result <-fuzzy_match(congress, politwoops, name, full_name, cutoff = .1, join_type = "inner")
  hash <- digest::digest(result)
  expect_equal(hash, hashsaved)
})
