test_that("ltm.data.summary runs successfully when passed a Rdata dataset", {
 
 load(testthat::test_path("fixtures", "newnans.rds"))
  #undebug(lds_ImpData)
  expect_no_error(ltm.data.summary("Newnans Test", newnans))

})

test_that("ltm.data.summary runs successfully when passed a string pointing to csv", {
  
  newnans_file <- testthat::test_path("fixtures", "newnans_subset.csv")
  #undebug(lds_ImpData)
  expect_no_error(ltm.data.summary("Newnans Test2", newnans_file))
  
})


