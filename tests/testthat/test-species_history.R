
load(testthat::test_path("fixtures", "newnans.rds"))
newn <- ltm.data.summary("Newn-Test", newnans)
sh <- species_history(newn)


test_that("species_history triggers no condition", {
  
  expect_no_warning(species_history(newn))
  
})

test_that("return_object argument returns proper argument", {
  
  expect_true(is_tibble(species_history(newn, return_object = "data")))
  
  expect_equal(class(species_history(newn, return_object = "ggplot")),
               c("gg", "ggplot"))
  
})
