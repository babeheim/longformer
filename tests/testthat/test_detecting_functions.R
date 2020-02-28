
test_that("collisions are detected", {

  data_pids <- c("1111", "1111", "2222", "2222")
  data_names <- c("john", "john", "mike", "mike")

  check <- flag_possible_collisions(data_pids, data_names, silent = TRUE)
  expect_true(length(check) == 0)

  data_pids <- c("1111", "1111", "2222", "2222")
  data_names <- c("john", "jon", "mike", "mike")

  check <- flag_possible_collisions(data_pids, data_names, silent = TRUE)
  expect_true(length(check) == 1)

  data_pids <- c("1111", "1111", "2222", "2222")
  data_names <- c("john", "jon", "mike", "mke")

  check <- flag_possible_collisions(data_pids, data_names, silent = TRUE)
  expect_true(length(check) == 2)

  data_pids <- c("1111", "1111", "2222", "2222")
  data_names <- c("john", "jon", "mike", "mke")

  check <- flag_possible_collisions(data_pids, data_names, threshold = 0.2, silent = TRUE)
  expect_true(length(check) == 0)

})

