test_that("is_number works", {
  expect_true(is_number(1))
  expect_false(is_number(0))
  expect_false(is_number("1"))
})

test_that("try_silent works", {
  expect_equal(try_silent(log(1)), 0)
  expect_s3_class(try_silent(log("1")), "ao_fail")
})

test_that("timed works", {
  f <- function(x, t) {
    Sys.sleep(t)
    x
  }
  expect_equal(timed(f(1, 0.5), 1), 1)
})

test_that("euclidean works", {
  x <- 1:10
  y <- 1:10
  expect_equal(euclidean(x, y), 0)
})
