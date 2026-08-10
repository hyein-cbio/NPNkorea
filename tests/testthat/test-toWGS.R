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
  expect_equal(unname(toWGS("  다사   5396  5207  ")), unname(expected), tolerance = 1e-5)
  expect_equal(unname(toWGS("  DaSa   5396  5207  ")), unname(expected), tolerance = 1e-5)
})

test_that("toWGS vectorizes and returns named columns", {
  inputs = c("다사 5396 5207", "마라43478827", "다다06940013")
  expected = matrix(
    c(126.9787, 37.56684, 129.0756, 35.17949, 126.49827, 33.48897),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(inputs, c("lon", "lat"))
  )

  expect_equal(toWGS(inputs), expected, tolerance = 1e-5)
})

test_that("toWGS returns an empty matrix for empty input", {
  expected = matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("lon", "lat")))

  expect_equal(toWGS(character(0)), expected)
})

test_that("toWGS validates input strings", {
  expect_error(toWGS(1), "`input.string` must be a character vector.")
  expect_error(
    toWGS(c("다사 5396 5207", NA_character_)),
    "`input.string` cannot contain missing values."
  )
  expect_error(toWGS("   "), "must be valid NPN strings")
  expect_error(toWGS("DS 5396 5207"), "must be valid NPN strings")
  expect_error(toWGS("다사 539 5207"), "must be valid NPN strings")
  expect_error(toWGS("갸사 5396 5207"), "must use valid NPN letter pairs")
})

test_that("CLI to-wgs prints tabular output", {
  output = capture.output(
    result <- NPNkorea:::npnkorea_cli(
      c("to-wgs", "--code", "다사 5396 5207", "--code", "MaRa43478827")
    )
  )

  expect_equal(names(result), c("lon", "lat"))
  expect_equal(result$lon, c(126.97866380715097, 129.0756266828708), tolerance = 1e-10)
  expect_equal(result$lat, c(37.56684471509263, 35.17949162224057), tolerance = 1e-10)
  expect_equal(output[1], "lon\tlat")
})

test_that("CLI help and command validation work", {
  help_output = capture.output(NPNkorea:::npnkorea_cli("--help"))
  expect_true(any(grepl("Usage:", help_output, fixed = TRUE)))
  expect_error(NPNkorea:::npnkorea_cli("unknown"), "Unknown command: unknown")
  expect_error(NPNkorea:::npnkorea_cli(c("to-wgs")), "`to-wgs` requires at least one `--code`.")
})
