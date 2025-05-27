#' Convert NPN to the GPS coordinate in WGS84
#' @param input.string Input character vector containing NPN properly spaced
#' @return longitude and latitude
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @export

toWGS = function(input.string) {
  return(t(sapply(input.string, toWGSindividual)))
}

toWGSindividual = function(input.s) {
  input.s.1 = unlist(strsplit(input.s, " "))

  han = grepl("[\uAC00-\uD7AF]", input.s.1[1])

  if (han) {
    input.s.3 = unlist(strsplit(input.s.1[1], ""))

    input.s.string = c(x=input.s.3[1], y=input.s.3[2])

    input.s.digit = c(
      x=digits.df$digits[match(input.s.string["x"], digits.df$han)],
      y=digits.df$digits[match(input.s.string["y"], digits.df$han)]
    )

  }
  if (!han) {
    input.s.2 = trimws(gsub("([A-Z])", " \\1", input.s.1[1]))
    input.s.3 = unlist(strsplit(input.s.2, " "))

    input.s.string = c(x=input.s.3[1], y=input.s.3[2])

    input.s.digit = c(
      x=digits.df$digits[match(input.s.string["x"], digits.df$abc)],
      y=digits.df$digits[match(input.s.string["y"], digits.df$abc)]
    )
  }

  coords = data.frame(
    X=(input.s.digit["x"] - 1) * 1e5 + 10 * as.numeric(input.s.1[2]),
    Y=(input.s.digit["y"] - 1) * 1e5 + 10 * as.numeric(input.s.1[3])
  )

  coords = data.frame(X=coords$X + 1e6 - 3e5, Y=coords$Y + 2e6 - 7e5)

  sf_points = st_as_sf(
    coords,
    coords=c("X", "Y"),
    crs=5179
  )

  sf_wgs = st_transform(sf_points, crs=4326)
  lonlat = st_coordinates(sf_wgs)

  return(lonlat)

}
