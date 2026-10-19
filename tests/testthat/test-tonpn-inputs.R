test_that("toNPN keeps positional data.frame output and accepts numeric vectors", {
  pts = data.frame(
    lon = c(126.978422, 129.075642),
    lat = c(37.566742, 35.179554)
  )
  expected_han = toNPN(pts)
  expected_abc = toNPN(pts, output = "abc")

  expect_equal(toNPN(pts, "abc"), expected_abc)
  expect_equal(toNPN(pts$lon, pts$lat), expected_han)
  expect_equal(toNPN(pts$lon, lat = pts$lat, output = "abc"), expected_abc)
  expect_equal(toNPN(pts$lon[[1]], pts$lat[[1]]), expected_han[[1]])
})

test_that("toNPN numeric input rejects a missing or mismatched lat", {
  expect_error(
    toNPN(126.978422, "abc"),
    "`lat` is required when the first argument is numeric."
  )
  expect_error(
    toNPN(126.978422, output = "abc"),
    "`lat` is required when the first argument is numeric."
  )
  expect_error(
    toNPN(c(126.978422, 129.075642), 37.566742),
    "`lon` and `lat` must have the same length."
  )
  expect_error(
    toNPN(126.978422, 37.566742, lat = 37.5),
    "`lat` was specified twice."
  )
  expect_error(
    toNPN(126.978422, 37.566742, "abc"),
    "pass `output` by name"
  )
})

test_that("toNPN accepts sf and sfc POINT geometries", {
  pts = data.frame(
    lon = c(126.978422, 129.075642, 126.705206),
    lat = c(37.566742, 35.179554, 37.456255)
  )
  expected = toNPN(pts)
  expected_abc = toNPN(pts, "abc")

  sf_wgs = sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)
  sf_wgs$lon = 0
  sf_wgs$lat = 0
  sf_korea = sf::st_transform(
    sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326),
    5179
  )

  expect_equal(toNPN(sf_wgs), expected)
  expect_equal(toNPN(sf_wgs, "abc"), expected_abc)
  expect_equal(toNPN(sf_korea, output = "han"), expected)
  expect_equal(toNPN(sf::st_geometry(sf_wgs)), expected)
  expect_equal(
    toNPN(sf::st_sf(geometry = sf::st_sfc(crs = 4326))),
    character(0)
  )
})

test_that("toNPN rejects invalid sf and sfc input", {
  expect_error(
    toNPN(sf::st_sfc(sf::st_point(c(126.978422, 37.566742)))),
    "must have a CRS"
  )
  expect_error(
    toNPN(sf::st_sfc(
      sf::st_linestring(matrix(c(126.9, 127, 37.5, 37.6), ncol = 2, byrow = TRUE)),
      crs = 4326
    )),
    "only POINT geometries"
  )
  expect_error(
    toNPN(sf::st_sfc(sf::st_point(), crs = 4326)),
    "empty geometries"
  )
  expect_error(
    toNPN(sf::st_sfc(sf::st_point(c(NA_real_, 37.566742)), crs = 4326)),
    "empty geometries"
  )
  expect_error(
    toNPN(data.frame(lon = 126.9, lat = 37.5), lat = 37.5),
    "`lat` is only valid when the first argument is numeric."
  )
})
