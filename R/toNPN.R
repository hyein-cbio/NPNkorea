#' Convert WGS84 coordinates to National Point Numbers
#'
#' \code{toNPN()} converts WGS84 longitude/latitude coordinates into Korea's
#' National Point Number (NPN) notation.
#'
#' @param input.df A data frame with numeric \code{lon} and \code{lat} columns
#'   in WGS84 decimal degrees. Additional columns are allowed, but only
#'   \code{lon} and \code{lat}
#'   are used.
#' @param output Output alphabet for the leading two-letter grid band:
#'   \code{"han"} (default) returns Hangul labels such as \code{"다사"}, and
#'   \code{"abc"} returns romanized labels such as \code{"DaSa"}.
#'
#' @details
#' Coordinates are transformed from WGS84 (EPSG:4326) to EPSG:5179, then mapped
#' to the NPN 10 m grid. Longitude must fall between 124 and 133 degrees, and
#' latitude must fall between 32 and 39 degrees; non-finite values are rejected.
#'
#' The returned NPN strings are formatted as \code{XX 1234 5678}, where
#' \code{XX} is the
#' 100 km grid-band pair and each four-digit block is a 10 m offset within that
#' band.
#'
#' @return A character vector of NPN strings, one per row of \code{input.df}.
#'   The output always uses the spaced form \code{XX 1234 5678}.
#'
#' @seealso \code{\link{toWGS}()} for the reverse conversion.
#' @family NPN conversion
#'
#' @examples
#' pts = data.frame(
#'   lon = c(126.978422, 129.075642),
#'   lat = c(37.566742, 35.179554)
#' )
#'
#' toNPN(pts)
#' toNPN(pts, output = "abc")
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @export

toNPN = function(input.df, output="han") {
  input.df = validate_lonlat_df(input.df)
  output = match.arg(output, c("han", "abc"))

  if (nrow(input.df) == 0) {
    return(character(0))
  }

  sf_points = st_as_sf(
    input.df,
    coords=c("lon", "lat"),
    crs=4326
  )

  sf_utm = st_transform(sf_points, crs=5179)
  coords = st_coordinates(sf_utm)

  coords[, "X"] = coords[, "X"] - 1e6 + 3e5
  coords[, "Y"] = coords[, "Y"] - 2e6 + 7e5
  coords = round(coords, digits = 6)

  lookup_col = if (output == "han") "han" else "abc"
  Xletter = digits.df[[lookup_col]][match(floor(coords[, "X"] / 1e5) + 1, digits.df$digits)]
  Yletter = digits.df[[lookup_col]][match(floor(coords[, "Y"] / 1e5) + 1, digits.df$digits)]

  Xpos = floor((coords[, "X"] %% 1e5) / 10)
  Ypos = floor((coords[, "Y"] %% 1e5) / 10)

  return(paste0(Xletter, Yletter, " ", sprintf("%04d", Xpos), " ", sprintf("%04d", Ypos)))

}
