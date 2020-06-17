
test_that("match_vids works", {

  # for one visit
  bind_rows(
    list(
      list(
        date = "2019-02-28",
        pid = "A",
        name = "elly",
        male = 0,
        vid = "VVKKVD"
      )
    )
  ) %>% as.data.frame(stringsAsFactors = FALSE) -> visits

  dates <- c("2019-02-28")
  pids <- c("A")

  new_vids <- match_vids(dates, pids, visits, quiet = TRUE)
  expect_identical(new_vids, "VVKKVD")

  # for a new visit

  dates <- c("2012-01-05")
  pids <- c("A")

  new_vids <- match_vids(dates, pids, visits, quiet = TRUE)
  expect_true(is.na(new_vids))

  # different visit on same day with a new person
  bind_rows(
    list(
      list(
        date = "2019-02-28",
        pid = "B",
        name = "elly",
        male = 0,
        vid = "VVKKVD"
      )
    )
  ) %>% as.data.frame(stringsAsFactors = FALSE) -> visits

  dates <- c("2019-02-28")
  pids <- c("A")

  new_vids <- match_vids(dates, pids, visits, quiet = TRUE)
  expect_true(is.na(new_vids))

})