
test_that("merge_people works", {

  bind_rows(
    list(
      list(
        pid = "AAAA",
        name = "elly",
        male = 0
      ),
      list(
        pid = "BBBB",
        name = "elly",
        male = 0
      )
    )
  ) %>% as.data.frame() -> reg

  reg2 <- merge_pids("AAAA", "BBBB", reg, verbose = FALSE)
  expect_true(nrow(reg2) == 1)

  bind_rows(
    list(
      list(
        pid = "A",
        name = "elly",
        male = 0
      ),
      list(
        pid = "B",
        name = "elly",
        male = 0
      ),
      list(
        pid = "C",
        name = "adrian",
        male = 1,
        mother_pid = "A",
        mother_name = "elly"
      )
    )
  ) %>% as.data.frame() -> reg

  reg2 <- merge_pids("A", "B", reg, verbose = FALSE)
  expect_true(nrow(reg2) == 2)
  expect_true(reg2$mother_pid[reg2$pid == "C"] == "B")


})