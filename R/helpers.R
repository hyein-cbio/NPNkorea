canonicalize_tonpn_arg_name = function(name) {
  if (!nzchar(name) || name %in% c("lat", "output")) {
    return(name)
  }

  # `output` used to be a formal argument, so prefixes such as `out` and `o`
  # still match it. `lat` stays exact: it was not part of the old signature.
  if (!is.na(pmatch(name, "output"))) {
    return("output")
  }

  name
}

resolve_tonpn_args = function(input.df, ...) {
  args = list(...)
  arg_names = names(args)
  if (is.null(arg_names)) {
    arg_names = rep("", length(args))
  }

  arg_names = vapply(arg_names, canonicalize_tonpn_arg_name, character(1), USE.NAMES = FALSE)
  names(args) = arg_names

  if (sum(arg_names == "output") > 1) {
    stop("`output` was specified twice.")
  }

  if (sum(arg_names == "lat") > 1) {
    stop("`lat` was specified twice.")
  }

  unknown = arg_names[arg_names != "" & !arg_names %in% c("lat", "output")]
  if (length(unknown) > 0) {
    stop(
      "Unused argument(s) in `toNPN()`: ",
      paste(unknown, collapse = ", "),
      "."
    )
  }

  positional = args[arg_names == ""]
  has_output = "output" %in% arg_names
  has_lat = "lat" %in% arg_names
  output = if (has_output) args$output else "han"
  lat = if (has_lat) args$lat else NULL

  if (is.numeric(input.df) && is.null(dim(input.df))) {
    if (length(positional) > 0) {
      if (!is.null(lat)) {
        stop("`lat` was specified twice.")
      }
      if (!is.numeric(positional[[1]]) || !is.null(dim(positional[[1]]))) {
        stop("`lat` is required when the first argument is numeric.")
      }
      lat = positional[[1]]
      positional = positional[-1]
    }

    if (is.null(lat)) {
      stop("`lat` is required when the first argument is numeric.")
    }

    if (!is.numeric(lat) || !is.null(dim(lat))) {
      stop("`lat` must be numeric.")
    }

    if (length(positional) > 0) {
      stop("For numeric input, pass `output` by name.")
    }
  } else {
    if (!is.null(lat)) {
      stop("`lat` is only valid when the first argument is numeric.")
    }

    if (length(positional) > 1) {
      stop("Unexpected arguments to `toNPN()`.")
    }

    if (length(positional) == 1) {
      if (has_output) {
        stop("`output` was specified twice.")
      }
      output = positional[[1]]
    }
  }

  list(output = output, lat = lat)
}

coerce_tonpn_input = function(input.df, lat = NULL) {
  if (is.numeric(input.df) && is.null(dim(input.df))) {
    if (length(input.df) != length(lat)) {
      stop("`lon` and `lat` must have the same length.")
    }

    return(validate_lonlat_df(data.frame(lon = input.df, lat = lat)))
  }

  if (inherits(input.df, "sf") || inherits(input.df, "sfc")) {
    return(lonlat_df_from_points(input.df))
  }

  validate_lonlat_df(input.df)
}

lonlat_df_from_points = function(x) {
  geom = if (inherits(x, "sfc")) x else st_geometry(x)

  if (is.na(st_crs(geom))) {
    stop("`input.df` sf/sfc objects must have a CRS.")
  }

  if (length(geom) == 0) {
    return(data.frame(lon = numeric(), lat = numeric()))
  }

  geometry_types = as.character(st_geometry_type(geom, by_geometry = TRUE))
  if (any(geometry_types != "POINT")) {
    stop("`input.df` sf/sfc objects must contain only POINT geometries.")
  }

  if (any(st_is_empty(geom))) {
    stop("`input.df` sf/sfc objects must not contain empty geometries.")
  }

  geom = st_transform(geom, 4326)
  coords = st_coordinates(geom)

  if (any(!is.finite(coords[, "X"])) || any(!is.finite(coords[, "Y"]))) {
    stop("`lon` and `lat` must be finite.")
  }

  validate_lonlat_df(data.frame(lon = coords[, "X"], lat = coords[, "Y"]))
}

format_npn_parts = function(x_digit, y_digit, x_pos, y_pos, output, spaced = TRUE) {
  lookup_col = if (output == "han") "han" else "abc"
  x_letter = digits.df[[lookup_col]][match(x_digit, digits.df$digits)]
  y_letter = digits.df[[lookup_col]][match(y_digit, digits.df$digits)]
  x_text = sprintf("%04d", x_pos)
  y_text = sprintf("%04d", y_pos)

  if (spaced) {
    paste0(x_letter, y_letter, " ", x_text, " ", y_text)
  } else {
    paste0(x_letter, y_letter, x_text, y_text)
  }
}

validate_lonlat_df = function(input.df) {
  if (!inherits(input.df, "data.frame")) {
    stop("`input.df` must be a data frame.")
  }

  if (!all(c("lon", "lat") %in% names(input.df))) {
    stop("`input.df` must contain numeric `lon` and `lat` columns.")
  }

  coords = input.df[, c("lon", "lat"), drop = FALSE]

  if (!is.numeric(coords$lon) || !is.numeric(coords$lat)) {
    stop("`lon` and `lat` columns must be numeric.")
  }

  if (any(!is.finite(coords$lon)) || any(!is.finite(coords$lat))) {
    stop("`lon` and `lat` must be finite.")
  }

  if (any(coords$lon <= 124 | coords$lon >= 133)) {
    stop("`lon` must be between 124 and 133.")
  }

  if (any(coords$lat <= 32 | coords$lat >= 39)) {
    stop("`lat` must be between 32 and 39.")
  }

  coords
}

project_to_npn_grid = function(input.df) {
  sf_points = st_as_sf(
    input.df,
    coords = c("lon", "lat"),
    crs = 4326
  )

  sf_utm = st_transform(sf_points, crs = 5179)
  coords = st_coordinates(sf_utm)

  coords[, "X"] = coords[, "X"] - 1e6 + 3e5
  coords[, "Y"] = coords[, "Y"] - 2e6 + 7e5

  round(coords, digits = 6)
}

validate_npn_grid_coords = function(coords) {
  if (any(!is.finite(coords[, "X"])) || any(!is.finite(coords[, "Y"]))) {
    stop("`input.df` contains coordinates that could not be projected to the NPN grid.")
  }

  x_digit = floor(coords[, "X"] / 1e5) + 1
  y_digit = floor(coords[, "Y"] / 1e5) + 1

  if (any(!x_digit %in% digits.df$digits) || any(!y_digit %in% digits.df$digits)) {
    stop("`input.df` contains coordinates outside the supported NPN grid after projection.")
  }

  x_pos = floor((coords[, "X"] %% 1e5) / 10)
  y_pos = floor((coords[, "Y"] %% 1e5) / 10)

  if (any(x_pos < 0 | x_pos > 9999) || any(y_pos < 0 | y_pos > 9999)) {
    stop("`input.df` contains coordinates outside the supported NPN grid after projection.")
  }

  list(
    x_digit = x_digit,
    y_digit = y_digit,
    x_pos = x_pos,
    y_pos = y_pos
  )
}

parse_npn_string = function(input.s) {
  input.compact = gsub("\\s+", "", trimws(input.s))

  if (!nzchar(input.compact)) {
    stop("`input.string` entries must be valid NPN strings like '\uB2E4\uC0AC 5396 5207' or 'DaSa53965207'.")
  }

  if (grepl("[\uAC00-\uD7AF]", input.compact)) {
    parts = regmatches(
      input.compact,
      regexec("^([\uAC00-\uD7AF])([\uAC00-\uD7AF])(\\d{4})(\\d{4})$", input.compact, perl = TRUE)
    )[[1]]

    if (length(parts) == 0) {
      stop("`input.string` entries must be valid NPN strings like '\uB2E4\uC0AC 5396 5207' or 'DaSa53965207'.")
    }

    x_digit = digits.df$digits[match(parts[2], digits.df$han)]
    y_digit = digits.df$digits[match(parts[3], digits.df$han)]
  } else {
    abc_codes = digits.df$abc[order(nchar(digits.df$abc), decreasing = TRUE)]
    abc_pattern = paste(toupper(abc_codes), collapse = "|")
    input.upper = toupper(input.compact)

    parts = regmatches(
      input.upper,
      regexec(
        paste0("^(", abc_pattern, ")(", abc_pattern, ")(\\d{4})(\\d{4})$"),
        input.upper,
        perl = TRUE
      )
    )[[1]]

    if (length(parts) == 0) {
      stop("`input.string` entries must be valid NPN strings like '\uB2E4\uC0AC 5396 5207' or 'DaSa53965207'.")
    }

    x_digit = digits.df$digits[match(parts[2], toupper(digits.df$abc))]
    y_digit = digits.df$digits[match(parts[3], toupper(digits.df$abc))]
  }

  if (is.na(x_digit) || is.na(y_digit)) {
    stop("`input.string` entries must use valid NPN letter pairs.")
  }

  list(
    x_digit = x_digit,
    y_digit = y_digit,
    x_pos = as.numeric(parts[4]),
    y_pos = as.numeric(parts[5])
  )
}
