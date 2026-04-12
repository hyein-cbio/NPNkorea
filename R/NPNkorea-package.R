#' @section Main functions:
#' \code{\link{toNPN}()} converts WGS84 coordinates into NPN strings.
#'
#' \code{\link{toWGS}()} parses NPN strings and returns WGS84 coordinates.
#'
#' @section Supported labels:
#' The package accepts both Hangul grid-band labels, such as
#' \code{"다사 5396 5207"}, and romanized labels, such as
#' \code{"DaSa53965207"}. Romanized input is
#' case-insensitive, and output can be requested in either Hangul or romanized
#' form.
#'
#' @section Grid interpretation:
#' NPN strings identify a 10 m grid cell within a 100 km grid-band pair.
#' Internally, conversions use the Korean projected CRS EPSG:5179 and WGS84
#' EPSG:4326.
#'
#' @keywords internal
"_PACKAGE"
