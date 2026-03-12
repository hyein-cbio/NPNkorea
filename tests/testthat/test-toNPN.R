test_that("toNPN converts single and multiple coordinates", {
  pts = data.frame(
    lon = c(126.978422, 129.075642, 126.705206),
    lat = c(37.566742, 35.179554, 37.456255)
  )

  expect_equal(
    toNPN(pts),
    c("다사 5393 5205", "마라 4347 8827", "다사 2970 3996")
  )

  expect_equal(
    toNPN(pts, output = "abc"),
    c("DaSa 5393 5205", "MaRa 4347 8827", "DaSa 2970 3996")
  )
})

test_that("toNPN round-trips coordinates generated from NPN cells", {
  npn_han = c("다사 5396 5207", "마라 4347 8827")
  coords = as.data.frame(toWGS(npn_han))

  expect_equal(toNPN(coords), npn_han)
  expect_equal(
    toNPN(coords, output = "abc"),
    c("DaSa 5396 5207", "MaRa 4347 8827")
  )
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
  expect_error(
    toNPN(data.frame(lon = 126.9, lat = NA_real_)),
    "`lon` and `lat` must be finite."
  )
  expect_error(toNPN(data.frame(lon = 126.9, lat = 37.5), output = "latin"), "should be one of")
})
