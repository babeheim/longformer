

test_that("clean_text works properly", {

# clean_text

# scrub non-unicode crazy characters
# d$first_name <- clean_text(d$first_name)
# d$last_name_1 <- clean_text(d$last_name_1)
# d$last_name_2 <- clean_text(d$last_name_2)

# this is what a lot turn into unfortunately
# flag this character and scrub: �


# sgf_tag <- stringi::stri_trans_general(sgf_tag, "latin-ascii") # convert non-ASCII to closest ascii
# sgf_tag <- gsub("[\x01-\x1F]", "", sgf_tag) # takes care of non-printing ASCII

# findOffendingCharacter <- function(x, maxStringLength=256){  
#   # print(x)
#   for (c in 1:maxStringLength){
#     offendingChar <- substr(x,c,c)
#     # print(offendingChar) 
#     #uncomment if you want the indiv characters printed
#     #the next character is the offending multibyte Character
#   }    
# }

# string_vector <- c("test", "Se\x96ora", "works fine")

# string_vector <- c("test", "Se\x96ora", "works fine")

#  - this doens't even print in the terminal window
# yep thats it! 
# x96 is little endian 96 00

#   japanese <- c("にほんご")
# stringi::stri_trans_general(japanese, "latin-ascii") # still japanese! 

# y <- c("a", "�", "a �")
# stringi::stri_trans_general(y, "latin-ascii") # no change! 

# z <- c("🔥", "にほんご", "�", "a🔥�ご")
# stringi::stri_trans_general(z, "latin-ascii") # no change! 

# z <- c("\\n", "\\t", "\v", "\\v")
# stringi::stri_trans_general(z, "latin-ascii") # no change! 

# z <- c("a", "", "🔥a")
# stringi::stri_trans_general(z, "latin-ascii") # scrubs! 


# the e accented character becomes  In\xe9s


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
