


test_that("filter functions mockup works", {

  list(
    list(
      ego = 1,
      f = 2,
      m = 3
    ),
    list(
      ego = 2,
      m = 4
    ),
    list(
      ego = 3,
      f = 5,
      m = 6
    ),
    list(
      ego = 4,
      f = 7
    ),
    list(
      ego = 5
    ),
    list(
      ego = 6
    ),
    list(
      ego = 7
    )
  ) %>% bind_rows() %>% as.matrix() -> reg

  expect_silent(am <- filter_ancestors("1", reg))
  expect_true(nrow(am) == nrow(reg))

  expect_silent(am <- filter_ancestors("4", reg))
  expect_true(nrow(am) == 2)

  expect_silent(am <- filter_ancestors("7", reg))
  expect_true(nrow(am) == 1)

  expect_silent(am <- filter_ancestors(c("1", "4"), reg))
  expect_true(nrow(am) == 7)

  expect_silent(dm <- filter_descendants("1", reg))
  expect_true(nrow(dm) == 1)
  
  expect_silent(dm <- filter_descendants("4", reg))
  expect_true(nrow(dm) == 3)

  expect_silent(dm <- filter_descendants("7", reg))
  expect_true(nrow(dm) == 4)

  expect_silent(dm <- filter_descendants(c("6", "7"), reg))
  expect_true(nrow(dm) == 6)

})

test_that("filter functions works on all stored pedigrees", {

  ppl <- read.csv("pedigrees/snoob_ppl.csv", stringsAsFactors = FALSE)
  ppl <- select(ppl, id, dad, mom)
  ppl$mom[ppl$mom == 0] <- NA
  ppl$dad[ppl$dad == 0] <- NA

  for (i in 1:10) {
    focal <- as.character(sample(ppl[,1], 1))
    expect_silent(am <- filter_ancestors(focal, ppl))
    expect_silent(am <- filter_descendants(focal, ppl))
    expect_silent(am <- filter_common_descendants(focal, ppl))
  }

  for (i in 1:10) {
    focal <- as.character(sample(ppl[,1], 5))
    expect_silent(am <- filter_ancestors(focal, ppl))
    expect_silent(am <- filter_descendants(focal, ppl))
    expect_silent(am <- filter_common_descendants(focal, ppl))
  }

})

test_that("extend_pedigree mockup works", {

  list(
    list(
      ego = 1,
      f = 2,
      m = 3
    ),
    list(
      ego = 2,
      m = 4
    ),
    list(
      ego = 3,
      f = 5,
      m = 6
    ),
    list(
      ego = 4,
      f = 7
    ),
    list(
      ego = 5
    ),
    list(
      ego = 6
    ),
    list(
      ego = 7
    )
  ) %>% bind_rows() %>% as.matrix() -> reg

  # only one generation back is just father and mother
  expect_silent(am <- extend_pedigree(reg, verbose = FALSE, max_generations = 1))
  expect_true(setequal(colnames(am), c("ego", "f", "m")))
  expect_true(nrow(am) == nrow(reg))

  expect_silent(am <- extend_pedigree(reg, verbose = FALSE, max_generations = 2))
  expect_true(setequal(colnames(am), c("ego", "f", "m", "fm", "mf", "mm")))
  expect_true(nrow(am) == nrow(reg))

  expect_silent(am <- extend_pedigree(reg, verbose = FALSE, max_generations = 3))
  expect_true(setequal(colnames(am), c("ego", "f", "m", "fm", "mf", "mm", "fmf")))
  expect_true(nrow(am) == nrow(reg))

  expect_silent(am <- extend_pedigree(reg, verbose = FALSE))
  expect_true(setequal(colnames(am), c("ego", "f", "m", "fm", "mf", "mm", "fmf")))
  expect_true(nrow(am) == nrow(reg))

  expect_true(all(am[1,] == 1:7))

})


test_that("extend_pedigree works on all stored pedigrees", {

  ppl <- read.csv("pedigrees/snoob_ppl.csv", stringsAsFactors = FALSE)
  ppl <- select(ppl, id, dad, mom)
  ppl$mom[ppl$mom == 0] <- NA
  ppl$dad[ppl$dad == 0] <- NA
  expect_silent(am <- extend_pedigree(ppl, verbose = FALSE, max_generations = 5))
  # sloooooow after 10 generations back

  ppl <- read.csv("pedigrees/endow_ppl.csv", stringsAsFactors = FALSE)
  ppl <- select(ppl, person_id, father_id, mother_id)
  expect_silent(am <- extend_pedigree(ppl, verbose = FALSE, max_generations = 5))

  ppl <- read.csv("pedigrees/mayangna_ppl.csv", stringsAsFactors = FALSE)
  ppl <- select(ppl, person_id, father_id, mother_id)
  expect_silent(am <- extend_pedigree(ppl, verbose = FALSE, max_generations = 5))

  ppl <- read.csv("pedigrees/thlhp_ppl.csv", stringsAsFactors = FALSE)
  ppl <- select(ppl, pid, father_pid, mother_pid)
  expect_silent(am <- extend_pedigree(ppl, verbose = FALSE, max_generations = 5))

})