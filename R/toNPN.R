#' Convert the GPS coordinate in WGS84 to NPN
#' @param input.df Input data frame containing longitude and latitude as columns
#' @return NPN format (4 significant digits)
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @export

toNPN = function(input.df, output="han") {

  stopifnot(class(input.df) == "data.frame")
  stopifnot(is.numeric(as.numeric(input.df)))
  stopifnot(colnames(input.df) == c("lon", "lat"))

  stopifnot(all(is.finite(as.matrix(input.df))))
  stopifnot(all(+124 < input.df$lon) & all(input.df$lon < +133))
  stopifnot(all(+32 < input.df$lat) & all(input.df$lat < +39))

  sf_points = st_as_sf(
    input.df,
    coords=c("lon", "lat"),
    crs=4326
  )

  sf_utm = st_transform(sf_points, crs=5179)
  coords = st_coordinates(sf_utm)

  coords[, "X"] = coords[, "X"] - 1e6 + 3e5
  coords[, "Y"] = coords[, "Y"] - 2e6 + 7e5

  if (output == "han") {
    Xletter = digits.df$han[match(floor(coords[, "X"] / 1e5) + 1, digits.df$digits)]
    Yletter = digits.df$han[match(floor(coords[, "Y"] / 1e5) + 1, digits.df$digits)]
  }
  if (output == "abc") {
    Xletter = digits.df$abc[match(floor(coords[, "X"] / 1e5) + 1, digits.df$digits)]
    Yletter = digits.df$abc[match(floor(coords[, "Y"] / 1e5) + 1, digits.df$digits)]
  }

  Xpos = floor((coords[, "X"] %% 1e5) / 10)
  Ypos = floor((coords[, "Y"] %% 1e5) / 10)

  return(paste0(Xletter, Yletter, " ", sprintf("%04d", Xpos), " ", sprintf("%04d", Ypos)))

}
