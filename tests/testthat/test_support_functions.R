

test_that("clean_text works properly", {

  expect_identical(clean_text(LETTERS), LETTERS)
  expect_identical(clean_text(letters), letters)
  expect_identical(clean_text(0:9), as.character(0:9))

  symbols <- list(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )

  expect_equal(clean_text(symbols$acute), "aeiouAEIOUyY")
  expect_equal(clean_text(symbols$grave), "aeiouAEIOU")
  expect_equal(clean_text(symbols$circunflex), "aeiouAEIOU")
  expect_equal(clean_text(symbols$umlaut), "aeiouAEIOUy")
  expect_equal(clean_text(symbols$cedil), "cC")

  expect_identical(clean_text(c("á", "b", "ç")), c("a", "b", "c"))
  expect_identical(clean_text(c(symbols$acute, symbols$grave)), c("aeiouAEIOUyY", "aeiouAEIOU"))

  expect_equal(clean_text("🔥"), "🔥")
  expect_equal(clean_text("☠"), "")
  expect_equal(clean_text("\u2620"), "")
  expect_equal(clean_text("gro\u00df"), "gross")
  expect_equal(clean_text("\u0104"), "A")
  expect_equal(clean_text(""), "")
  expect_equal(clean_text(""), "")
  expect_equal(clean_text("\x96"), "")
  expect_equal(clean_text("\xf1"), "")
  expect_equal(clean_text("\xf3"), "")
  expect_equal(clean_text("\xe9"), "")
  expect_equal(clean_text("\xfa"), "")
  expect_equal(clean_text("�"), "")
})

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
