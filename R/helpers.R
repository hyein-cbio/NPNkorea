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
