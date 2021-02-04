library(testthat)
library(eat)

test_check("eat")

usethis::use_test("EAT_function")
usethis::use_test("Training_Test")
usethis::use_test("modelArgument")

devtools::test()

