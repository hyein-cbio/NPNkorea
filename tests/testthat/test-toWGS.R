test_that("toWGS parses spaced and compact NPN strings", {
  expected = matrix(
    c(126.9787, 37.56684),
    nrow = 1,
    dimnames = list("다사 5396 5207", c("lon", "lat"))
  )

  expect_equal(toWGS("다사 5396 5207"), expected, tolerance = 1e-5)
  expect_equal(unname(toWGS("다사53965207")), unname(expected), tolerance = 1e-5)
  expect_equal(unname(toWGS("DaSa 5396 5207")), unname(expected), tolerance = 1e-5)
  expect_equal(unname(toWGS("dasa53965207")), unname(expected), tolerance = 1e-5)
})

test_that("toWGS vectorizes and returns named columns", {
  inputs = c("다사 5396 5207", "마라43478827")
  expected = matrix(
    c(126.9787, 37.56684, 129.0756, 35.17949),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(inputs, c("lon", "lat"))
  )

  expect_equal(toWGS(inputs), expected, tolerance = 1e-5)
})

test_that("toWGS validates input strings", {
  expect_error(toWGS(1), "`input.string` must be a character vector.")
  expect_error(
    toWGS(c("다사 5396 5207", NA_character_)),
    "`input.string` cannot contain missing values."
  )
  expect_error(toWGS("DS 5396 5207"), "must be valid NPN strings")
  expect_error(toWGS("다사 539 5207"), "must be valid NPN strings")
})
