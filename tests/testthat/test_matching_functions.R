
test_that("match_vids works", {

  bind_rows(
    list(
      list(
        date = "2020-02-28",
        pid = "A",
        name = "elly",
        male = 0,
        vid = "AA"
      ),
      list(
        date = "2019-02-28",
        pid = "A",
        name = "elly",
        male = 0,
        vid = "BB"
      )
    )
  ) %>% as.data.frame(stringsAsFactors = FALSE) -> visits

  dates <- c("2019-02-28", "2019-02-28")

  pids <- c("A", "B")

  visits$date <- as.Date(visits$date)
  dates <- as.Date(dates)

  new_vids <- match_vids(dates, pids, visits, quiet = TRUE)
  expect_identical(new_vids, c("BB", NA))

})