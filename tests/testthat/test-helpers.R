# helper_addgapyears -----------------------------------------------------------

test_table <- function() {
  temp <- source(testthat::test_path("fixtures", "yearsumTable.R"))
  return(temp$value)
}



test_that("table contains columns `Year` and `yr`", {
  
  tt <- test_table()
  
  expected_names <- c("Year", "yr")
  
  expected_column_names_missing <- any(!expected_names %in% names(tt)) 
  
  expect_false(expected_column_names_missing)
})

test_that("output equals input when no years are missing", {
  input_missing <- test_table()
  
  input_no_missing <- 
    input_missing %>% 
    helper_addgapyears() %>% 
    mutate(BOW = replace_na(BOW, 0.5),
           LMB = replace_na(LMB, 0.2))
  
  expect_equal(pull(input_no_missing, yr), c(min(input_no_missing$yr):max(input_no_missing$yr)))
  
  expect_equal(input_no_missing, helper_addgapyears(input_no_missing))
})

# check_expected_columns -------------------------------------------------------

test_that("expected column test passes when no column missing", {
 expect_no_error(check_expected_columns(test_table(), c("Year", "yr"))) 
})

test_that("expected column test fails when column missing", {
  expect_error(check_expected_columns(test_table(), c("Year", "yr", "missingCol"))) 
})





