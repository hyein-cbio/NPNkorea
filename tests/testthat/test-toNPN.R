test_that("toNPN converts single and multiple coordinates", {
  pts = data.frame(
    lon = c(126.978422, 129.075642, 126.705206, 126.4982702),
    lat = c(37.566742, 35.179554, 37.456255, 33.4889944)
  )

  expect_equal(
    toNPN(pts),
    c("다사 5393 5205", "마라 4347 8827", "다사 2970 3996", "다다 0694 0013")
  )

  expect_equal(
    toNPN(pts, output = "abc"),
    c("DaSa 5393 5205", "MaRa 4347 8827", "DaSa 2970 3996", "DaDa 0694 0013")
  )
})

test_that("toNPN round-trips coordinates generated from NPN cells", {
  npn_han = c("다사 5396 5207", "마라 4347 8827", "다다 0694 0013")
  coords = as.data.frame(toWGS(npn_han))

  expect_equal(toNPN(coords), npn_han)
  expect_equal(
    toNPN(coords, output = "abc"),
    c("DaSa 5396 5207", "MaRa 4347 8827", "DaDa 0694 0013")
  )
})

test_that("toNPN returns an empty character vector for empty input", {
  empty_pts = data.frame(lon = numeric(), lat = numeric())

  expect_equal(toNPN(empty_pts), character(0))
  expect_equal(toNPN(empty_pts, output = "abc"), character(0))
})

test_that("toNPN validates input", {
  expect_error(toNPN(list(lon = 126.9, lat = 37.5)), "`input.df` must be a data frame.")
  expect_error(
    toNPN(data.frame(x = 126.9, y = 37.5)),
    "`input.df` must contain numeric `lon` and `lat` columns."
  )
  expect_error(
    toNPN(data.frame(lon = "126.9", lat = 37.5)),
    "`lon` and `lat` columns must be numeric."
  )
  expect_error(toNPN(data.frame(lon = 140, lat = 37.5)), "`lon` must be between 124 and 133.")
  expect_error(toNPN(data.frame(lon = 126.9, lat = 32)), "`lat` must be between 32 and 39.")
  expect_error(toNPN(data.frame(lon = 126.9, lat = 39)), "`lat` must be between 32 and 39.")
  expect_error(
    toNPN(data.frame(lon = 126.9, lat = NA_real_)),
    "`lon` and `lat` must be finite."
  )
  expect_error(
    toNPN(data.frame(lon = Inf, lat = 37.5)),
    "`lon` and `lat` must be finite."
  )
  expect_error(
    toNPN(data.frame(lon = 126.9, lat = -Inf)),
    "`lon` and `lat` must be finite."
  )
  expect_error(toNPN(data.frame(lon = 126.9, lat = 37.5), output = "latin"), "should be one of")
})

test_that("toNPN rejects coordinates outside the supported NPN grid after projection", {
  expect_error(
    toNPN(data.frame(lon = 124.000001, lat = 33)),
    "outside the supported NPN grid after projection"
  )
  expect_error(
    toNPN(data.frame(lon = 124.2, lat = 32.1)),
    "outside the supported NPN grid after projection"
  )
})

test_that("toNPN never emits NA grid bands", {
  pts = data.frame(
    lon = c(124.4, 124.9, 125.5, 132.8),
    lat = c(33.0, 38.5, 35.0, 32.5)
  )

  out = toNPN(pts)

  expect_false(any(grepl("NA", out, fixed = TRUE)))
})

test_that("toNPN round-trips random NPN cells", {
  digits = get("digits.df", envir = asNamespace("NPNkorea"))

  set.seed(1)
  npn_han = paste0(
    sample(digits$han, 50, replace = TRUE),
    sample(digits$han, 50, replace = TRUE),
    " ",
    sprintf("%04d", sample(0:9999, 50, replace = TRUE)),
    " ",
    sprintf("%04d", sample(0:9999, 50, replace = TRUE))
  )

  coords = as.data.frame(toWGS(npn_han))
  keep = coords$lon > 124 & coords$lon < 133 & coords$lat > 32 & coords$lat < 39

  expect_gt(sum(keep), 0)
  expect_equal(toNPN(coords[keep, , drop = FALSE]), npn_han[keep])
})
