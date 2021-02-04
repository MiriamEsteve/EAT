library(testthat)
library(eat)

test_check("eat")

usethis::use_test("EAT_function")
usethis::use_test("TrainingTest")
usethis::use_test("modelArgument")

devtools::test()

