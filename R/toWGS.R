#' Convert National Point Numbers to WGS84 coordinates
#'
#' \code{toWGS()} parses Korea's National Point Number (NPN) strings and returns the
#' corresponding longitude/latitude coordinates in WGS84.
#'
#' @param input.string A character vector of NPN strings. Both Hangul labels
#'   (for example, \code{"다사 5396 5207"}) and romanized labels (for example,
#'   \code{"DaSa53965207"}) are supported. Whitespace is optional, and romanized
#'   input is matched case-insensitively.
#'
#' @details
#' Each NPN value is interpreted as a 100 km grid-band pair plus two four-digit
#' offsets that identify a 10 m grid cell. The returned coordinates correspond
#' to the southwest corner of that grid cell after transforming from EPSG:5179
#' to WGS84 (EPSG:4326).
#'
#' Empty input returns a \code{0 x 2} numeric matrix. Missing values and malformed
#' NPN strings raise an error.
#'
#' @return A numeric matrix with one row per input string. The matrix has row
#'   names equal to \code{input.string} and two columns named \code{lon} and
#'   \code{lat}.
#'
#' @seealso \code{\link{toNPN}()} for the reverse conversion.
#' @family NPN conversion
#'
#' @examples
#' toWGS("다사 5396 5207")
#'
#' toWGS(c("다사53965207", "MaRa43478827"))
#'
#' toWGS(c("DaSa 5396 5207", "dada06940013"))
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @export

toWGS = function(input.string) {
  if (!is.character(input.string)) {
    stop("`input.string` must be a character vector.")
  }

  if (anyNA(input.string)) {
    stop("`input.string` cannot contain missing values.")
  }

  if (length(input.string) == 0) {
    return(matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("lon", "lat"))))
  }

  lonlat = t(vapply(input.string, toWGSindividual, FUN.VALUE = c(lon = 0, lat = 0)))
  rownames(lonlat) = input.string
  colnames(lonlat) = c("lon", "lat")

  lonlat
}

toWGSindividual = function(input.s) {
  parsed = parse_npn_string(input.s)

  coords = data.frame(
    X=(parsed$x_digit - 1) * 1e5 + 10 * parsed$x_pos,
    Y=(parsed$y_digit - 1) * 1e5 + 10 * parsed$y_pos
  )

  coords = data.frame(X=coords$X + 1e6 - 3e5, Y=coords$Y + 2e6 - 7e5)

  sf_points = st_as_sf(
    coords,
    coords=c("X", "Y"),
    crs=5179
  )

  sf_wgs = st_transform(sf_points, crs=4326)
  lonlat = as.numeric(st_coordinates(sf_wgs)[1, c("X", "Y")])
  names(lonlat) = c("lon", "lat")

  return(lonlat)

}
