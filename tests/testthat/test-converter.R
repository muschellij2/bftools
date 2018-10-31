context("Just simple tryout")


testthat::test_that("Just trying a simple convert", {

  file = system.file("img", "Rlogo.png", package = "png")
  if (file.exists(file)) {
    res = bfconvert(file = file)
    testthat::expect_true(file.exists(res))
  }
  testthat::expect_true(file.exists(bf_cmd("bfconvert")))

})
