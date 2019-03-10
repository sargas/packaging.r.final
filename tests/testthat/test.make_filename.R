context("Making filename")
library(packaging.r.final)

test_that("filename ends in name of data file", {
  expect_match(make_filename(2013), "accident_2013.csv.bz2$")
  expect_match(make_filename(2015), "accident_2015.csv.bz2$")
})