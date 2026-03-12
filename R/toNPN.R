#' Convert WGS84 coordinates to Korea's National Point Number
#' @param input.df A data frame with numeric `lon` and `lat` columns.
#' @param output Either `"han"` (Hangul, default) or `"abc"` (romanized syllables).
#' @return A character vector of NPN values in `XX 1234 5678` form.
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
