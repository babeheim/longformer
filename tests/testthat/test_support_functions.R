

test_that("reverse_month_day works", {
  expect_equal(reverse_month_day("2020-05-02"), "2020-02-05")
  expect_identical(reverse_month_day(c("2020-05-02", "2020-03-09")), c("2020-02-05", "2020-09-03"))
  expect_true(is.na(reverse_month_day("2020-05-31")))
  expect_identical(reverse_month_day(c("2020-05-02", "2020-03-09", NA)), c("2020-02-05", "2020-09-03", NA))
})

# make_ids should take a bag
# leading digit of make_id is never 0

test_that("make_ids always produces the correct number of ids, but never collides with itself", {
  for(i in 1:10){
    n <- sample(1:100000, 1)
    x <- make_ids(n = n, nchars = 5)
    expect_false(any(duplicated(x)))
    expect_true(length(x) == n)
  }
})
