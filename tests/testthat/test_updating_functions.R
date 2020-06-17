

test_that("update_pid works", {

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

  reg2 <- update_pid("AAAA", "BBBB", reg, verbose = FALSE)
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

  reg2 <- update_pid("A", "B", reg, verbose = FALSE)
  expect_true(nrow(reg2) == 2)
  expect_true(reg2$mother_pid[reg2$pid == "C"] == "B")


})

# what happens if add is longer than 1?

test_that("update_id_changes works", {

  changes <- list(
    list(
      date = "2020-06-17",
      coder = "BAB",
      type = "pid",
      old_id = "A",
      active_id = "C",
      name = "Harry",
      reason = "duplication"
    ),
    list(
      date = "2020-06-17",
      coder = "NG",
      type = "pid",
      old_id = "B",
      active_id = "C",
      name = "Harry",
      reason = "duplication"
    ),
    list(
      date = "2020-06-17",
      coder = "BB",
      type = "pid",
      old_id = "F",
      active_id = "G",
      name = "Ron",
      reason = "duplication"
    )
  ) %>%
  bind_rows() %>%
  as.data.frame(stringsAsFactors = FALSE)

  add <- list(
    date = "2020-06-17",
    coder = "BAB",
    type = "pid",
    old_id = "C",
    active_id = "D",
    name = "Harry",
    reason = "duplication"
  )

  updated_changes <- update_id_changes(add, changes)

  expect_true(nrow(updated_changes) == 4)
  expect_true(!"C" %in% updated_changes$active_id)
  expect_true("C" %in% updated_changes$old_id)

})