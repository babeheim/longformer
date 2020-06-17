
test_that("collisions are detected", {

  data_pids <- c("1111", "1111", "2222", "2222")
  data_names <- c("john", "john", "mike", "mike")

  check <- detect_pid_collisions(data_pids, data_names, silent = TRUE)
  expect_true(length(check) == 0)

  data_pids <- c("1111", "1111", "2222", "2222")
  data_names <- c("john", "jon", "mike", "mike")

  check <- detect_pid_collisions(data_pids, data_names, silent = TRUE)
  expect_true(length(check) == 1)

  data_pids <- c("1111", "1111", "2222", "2222")
  data_names <- c("john", "jon", "mike", "mk")

  check <- detect_pid_collisions(data_pids, data_names, silent = TRUE)
  expect_true(length(check) == 2)

  check <- detect_pid_collisions(data_pids, data_names, threshold = 0.7, silent = TRUE)
  expect_true(length(check) == 1)

  check <- detect_pid_collisions(data_pids, data_names, threshold = 0.2, silent = TRUE)
  expect_true(length(check) == 0)

  data_pids <- c("1111", "1111", "2222", "2222")
  data_names <- c("john", "jon", "mike", "mke")
  reference_pids <- c("1111", "2222")
  reference_names <- c("john", "lisa")

  check <- detect_pid_collisions(data_pids, data_names, reference_pids, reference_names, threshold = 0.7, silent = TRUE)
  expect_true(check == "2222")

  # also check error states

  data_pids <- c("1111", "1111", "2222", "2222")
  data_names <- c("john", "jon", "mike", "mke")
  reference_pids <- c("1111", "2222", "2222") # duplicated pid
  reference_names <- c("john", "lisa", "tony")
  expect_error(detect_pid_collisions(data_pids, data_names, reference_pids, reference_names))

  data_pids <- c("1111", "1111", "2222", "2222")
  data_names <- c("john", "jon", "mike", "mke")
  reference_pids <- c("1111", "2222")
  reference_names <- c("john") # wrong length
  expect_error(detect_pid_collisions(data_pids, data_names, reference_pids, reference_names))

  data_pids <- c("1111", "1111", "2222", "2222")
  data_names <- c("john", "jon", "mike") # wrong length
  expect_error(detect_pid_collisions(data_pids, data_names))

})

