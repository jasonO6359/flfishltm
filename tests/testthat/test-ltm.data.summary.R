load(test_path('fixtures', 'newnans.rds'))
out_test_dir <- tempdir()
#newn <- ltm.data.summary("newnans-test", newnans, )

test_that("ltm.data.summary executes without error - no tables or figure export", {
  expect_no_error({
    ltm.data.summary("newnans-test",
                     newnans,
                     print_directory = out_test_dir)
  })
})

test_that("ltm.data.summary executes without error when printing CPUE by number figs", {
  expect_no_error({
    ltm.data.summary("newnans-test",
                     newnans,
                     printfigs = 2,
                     print_directory = out_test_dir)
  })
})

test_that("ltm.data.summary executes without error when printing CPUE by weight figs", {
  expect_no_error({
    ltm.data.summary("newnans-test",
                     newnans,
                     printfigs = 3,
                     print_directory = out_test_dir)
  })
})

test_that("ltm.data.summary executes without error when printing summary figs", {
  expect_no_error({
    ltm.data.summary("newnans-test",
                     newnans,
                     printfigs = 4,
                     print_directory = out_test_dir)
  })
})

test_that("data that was exported from SSRS to excel then converted to csv imports properly", {
  expect_no_error({
  ltm.data.summary("orange-test", testthat::test_path("fixtures/orange_excel.csv"))
  })
})

test_that("data that was exported from SSRS to csv imports properly", {
  expect_no_error({
    ltm.data.summary("orange-test", testthat::test_path("fixtures/orange_csv.csv"))
  })
})

