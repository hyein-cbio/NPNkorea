historical_points = function() {
  data.frame(
    lon = c(126.978422, 129.075642, 126.705206, 126.4982702),
    lat = c(37.566742, 35.179554, 37.456255, 33.4889944),
    label = c("seoul", "busan", "incheon", "jeju")
  )
}

historical_npn = c(
  "다사 5393 5205",
  "마라 4347 8827",
  "다사 2970 3996",
  "다다 0694 0013"
)

test_that("documented data.frame calls still return the historical NPN strings", {
  pts = historical_points()

  expect_equal(toNPN(pts), historical_npn)
  expect_equal(toNPN(pts, "abc"), c(
    "DaSa 5393 5205", "MaRa 4347 8827", "DaSa 2970 3996", "DaDa 0694 0013"
  ))
  expect_equal(toNPN(pts, output = "abc"), toNPN(pts, "abc"))
  expect_equal(toNPN(pts, output = "han"), historical_npn)
  expect_equal(toNPN(input.df = pts), historical_npn)
  expect_equal(toNPN(input.df = pts, output = "abc"), toNPN(pts, "abc"))
  expect_equal(do.call(toNPN, list(pts, output = "abc")), toNPN(pts, "abc"))
  expect_equal(toNPN(pts, out = "abc"), toNPN(pts, output = "abc"))
  expect_equal(toNPN(pts[1, ], o = "han"), historical_npn[[1]])
  expect_equal(toNPN(pts, outp = "abc"), toNPN(pts, "abc"))
})

test_that("data.frame shape does not change the historical conversion", {
  pts = historical_points()
  reordered = data.frame(label = pts$label, lat = pts$lat, lon = pts$lon, extra = 1:4)
  integers = data.frame(lon = c(127L, 129L), lat = c(37L, 35L))
  subclass = pts
  class(subclass) = c("custom_df", "data.frame")
  named = pts[1, c("lon", "lat")]
  rownames(named) = "seoul"

  expect_equal(toNPN(reordered), historical_npn)
  expect_equal(toNPN(integers), toNPN(data.frame(lon = c(127, 129), lat = c(37, 35))))
  expect_equal(toNPN(subclass), historical_npn)
  expect_equal(toNPN(named), historical_npn[[1]])
  expect_null(names(toNPN(named)))
  expect_equal(toNPN(pts[integer(0), c("lon", "lat")]), character(0))
  expect_equal(toNPN(data.frame(lon = numeric(), lat = numeric()), "abc"), character(0))
})

test_that("historical toNPN errors keep their messages", {
  expect_error(toNPN(list(lon = 126.9, lat = 37.5)), "`input.df` must be a data frame.")
  expect_error(toNPN(NULL), "`input.df` must be a data frame.")
  expect_error(toNPN(cbind(lon = 126.9, lat = 37.5)), "`input.df` must be a data frame.")
  expect_error(
    toNPN(data.frame(x = 126.9, y = 37.5)),
    "`input.df` must contain numeric `lon` and `lat` columns."
  )
  expect_error(
    toNPN(data.frame(lon = "126.9", lat = 37.5)),
    "`lon` and `lat` columns must be numeric."
  )
  expect_error(toNPN(data.frame(lon = 124, lat = 37.5)), "`lon` must be between 124 and 133.")
  expect_error(toNPN(data.frame(lon = 133, lat = 37.5)), "`lon` must be between 124 and 133.")
  expect_error(toNPN(data.frame(lon = 126.9, lat = 32)), "`lat` must be between 32 and 39.")
  expect_error(toNPN(data.frame(lon = 126.9, lat = 39)), "`lat` must be between 32 and 39.")
  expect_error(toNPN(data.frame(lon = 126.9, lat = NA_real_)), "`lon` and `lat` must be finite.")
  expect_error(toNPN(data.frame(lon = NaN, lat = 37.5)), "`lon` and `lat` must be finite.")
  expect_error(toNPN(data.frame(lon = -Inf, lat = 37.5)), "`lon` and `lat` must be finite.")
  expect_error(toNPN(data.frame(lon = 126.9, lat = 37.5), output = "latin"), "should be one of")
  expect_error(toNPN(data.frame(lon = 126.9, lat = 37.5), output = "ABC"), "should be one of")
  expect_error(toNPN(data.frame(lon = 126.9, lat = 37.5), l = 1), "Unused argument")
  expect_error(toNPN(data.frame(lon = 126.9, lat = 37.5), out = "abc", output = "han"), "`output` was specified twice.")
})

test_that("historical toWGS values, names, and errors are unchanged", {
  spaced = "다사 5396 5207"
  expected = matrix(
    c(126.97866380715097, 37.56684471509263),
    nrow = 1,
    dimnames = list(spaced, c("lon", "lat"))
  )

  expect_equal(toWGS(spaced), expected, tolerance = 1e-12)
  expect_equal(unname(toWGS("dasa53965207")), unname(expected), tolerance = 1e-12)
  expect_equal(unname(toWGS("  DaSa   5396  5207  ")), unname(expected), tolerance = 1e-12)
  expect_equal(
    toWGS(character(0)),
    matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("lon", "lat")))
  )

  inputs = c("다사 5396 5207", "마라43478827", "다다06940013")
  converted = toWGS(inputs)
  expect_equal(rownames(converted), inputs)
  expect_equal(colnames(converted), c("lon", "lat"))
  expect_equal(toNPN(as.data.frame(converted)), c("다사 5396 5207", "마라 4347 8827", "다다 0694 0013"))

  expect_error(toWGS(1), "`input.string` must be a character vector.")
  expect_error(toWGS(c("다사 5396 5207", NA_character_)), "`input.string` cannot contain missing values.")
  expect_error(toWGS("   "), "must be valid NPN strings")
  expect_error(toWGS("DS 5396 5207"), "must be valid NPN strings")
  expect_error(toWGS("다사 539 5207"), "must be valid NPN strings")
  expect_error(toWGS("갸사 5396 5207"), "must use valid NPN letter pairs")
})

test_that("historical CLI calls keep their tabular results", {
  npn_output = capture.output(
    npn_result <- NPNkorea:::npnkorea_cli(
      c("to-npn", "--lon", "126.978422", "--lat", "37.566742", "--output", "abc")
    )
  )
  expect_equal(npn_result, data.frame(npn = "DaSa 5393 5205", check.names = FALSE))
  expect_equal(npn_output, c("npn", "DaSa 5393 5205"))

  multi_output = capture.output(
    multi_result <- NPNkorea:::npnkorea_cli(c(
      "to-npn",
      "--lon", "126.978422", "--lat", "37.566742",
      "--lon", "129.075642", "--lat", "35.179554"
    ))
  )
  expect_equal(multi_result$npn, historical_npn[1:2])
  expect_equal(multi_output, c("npn", historical_npn[1], historical_npn[2]))

  wgs_output = capture.output(
    wgs_result <- NPNkorea:::npnkorea_cli(
      c("to-wgs", "--code", "다사 5396 5207", "--code", "MaRa43478827")
    )
  )
  expect_equal(wgs_result$lon, c(126.97866380715097, 129.0756266828708), tolerance = 1e-12)
  expect_equal(wgs_result$lat, c(37.56684471509263, 35.17949162224057), tolerance = 1e-12)
  expect_equal(wgs_output[1], "lon\tlat")

  help_output = capture.output(NPNkorea:::npnkorea_cli(c("-h")))
  expect_true(any(grepl("npnkorea to-npn", help_output, fixed = TRUE)))
  expect_error(NPNkorea:::npnkorea_cli("unknown"), "Unknown command: unknown")
  expect_error(NPNkorea:::npnkorea_cli("to-wgs"), "`to-wgs` requires at least one `--code`.")
  expect_error(
    NPNkorea:::npnkorea_cli(c("to-npn", "--lon", "126.978422")),
    "`to-npn` requires both `--lon` and `--lat`."
  )
  expect_error(
    NPNkorea:::npnkorea_cli(c("to-npn", "--lon", "126.9", "--lat", "37.5", "--lat", "35")),
    "`--lon` and `--lat` must be provided the same number of times."
  )
})

test_that("sf columns that agree with the geometry stay compatible", {
  pts = historical_points()[c("lon", "lat")]
  sf_pts = sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

  expect_equal(toNPN(sf_pts), historical_npn)
  expect_equal(toNPN(sf_pts, out = "abc"), toNPN(pts, output = "abc"))
})

test_that("sf geometry wins over conflicting lon/lat columns", {
  pts = historical_points()[c("lon", "lat")]
  sf_pts = sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  sf_pts$lon = 127
  sf_pts$lat = 37

  expect_equal(toNPN(sf_pts), historical_npn)
  expect_equal(
    toNPN(sf::st_drop_geometry(sf_pts)),
    toNPN(data.frame(lon = rep(127, 4), lat = rep(37, 4)))
  )
  expect_false(identical(toNPN(sf_pts), toNPN(sf::st_drop_geometry(sf_pts))))
})

test_that("numeric toNPN matches the data.frame path and keeps old output names", {
  pts = historical_points()
  expect_equal(toNPN(pts$lon, pts$lat), historical_npn)
  expect_equal(toNPN(pts$lon, pts$lat, out = "abc"), toNPN(pts, output = "abc"))
  expect_equal(toNPN(pts$lon, lat = pts$lat, o = "han"), historical_npn)
  expect_equal(toNPN(numeric(0), numeric(0)), character(0))
  expect_equal(toNPN(numeric(0), numeric(0), output = "abc"), character(0))
  expect_error(toNPN(pts$lon, "abc"), "`lat` is required when the first argument is numeric.")
  expect_error(toNPN(pts$lon, out = "abc"), "`lat` is required when the first argument is numeric.")
  expect_error(toNPN(pts$lon, pts$lat, "abc"), "pass `output` by name")
  expect_error(toNPN(c(126.9, 127), 37.5), "`lon` and `lat` must have the same length.")
  expect_error(toNPN(126.9, Inf), "`lon` and `lat` must be finite.")
})
