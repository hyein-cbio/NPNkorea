cli_usage = function() {
  c(
    "NPNkorea command line interface",
    "",
    "Usage:",
    "  npnkorea to-npn --lon <value> --lat <value> [--lon <value> --lat <value> ...] [--output han|abc]",
    "  npnkorea to-wgs --code <value> [--code <value> ...]",
    "  npnkorea --help",
    "",
    "Examples:",
    "  npnkorea to-npn --lon 126.978422 --lat 37.566742",
    "  npnkorea to-npn --lon 126.978422 --lat 37.566742 --lon 129.075642 --lat 35.179554 --output abc",
    "  npnkorea to-wgs --code '\uB2E4\uC0AC 5396 5207'",
    "  npnkorea to-wgs --code '\uB2E4\uC0AC 5396 5207' --code 'MaRa43478827'"
  )
}

npnkorea_cli = function(args = commandArgs(TRUE)) {
  if (length(args) == 0 || any(args %in% c("-h", "--help"))) {
    writeLines(cli_usage())
    return(invisible(NULL))
  }

  command = args[[1]]
  parsed = parse_cli_options(args[-1])

  if (command == "to-npn") {
    return(invisible(run_cli_to_npn(parsed)))
  }

  if (command == "to-wgs") {
    return(invisible(run_cli_to_wgs(parsed)))
  }

  stop("Unknown command: ", command, call. = FALSE)
}

parse_cli_options = function(args) {
  options = list()
  i = 1

  while (i <= length(args)) {
    key = args[[i]]
    if (!startsWith(key, "--")) {
      stop("Unexpected positional argument: ", key, call. = FALSE)
    }

    if (i == length(args)) {
      stop("Missing value for option: ", key, call. = FALSE)
    }

    value = args[[i + 1]]
    name = substring(key, 3)
    options[[name]] = c(options[[name]], value)
    i = i + 2
  }

  options
}

run_cli_to_npn = function(options) {
  lon = options$lon
  lat = options$lat

  if (is.null(lon) || is.null(lat)) {
    stop("`to-npn` requires both `--lon` and `--lat`.", call. = FALSE)
  }

  if (length(lon) != length(lat)) {
    stop("`--lon` and `--lat` must be provided the same number of times.", call. = FALSE)
  }

  output = if (is.null(options$output)) "han" else options$output[[1]]

  coords = data.frame(
    lon = as.numeric(lon),
    lat = as.numeric(lat)
  )

  if (anyNA(coords$lon) || anyNA(coords$lat)) {
    stop("`--lon` and `--lat` values must be numeric.", call. = FALSE)
  }

  result = data.frame(npn = toNPN(coords, output = output), check.names = FALSE)
  utils::write.table(result, sep = "\t", row.names = FALSE, quote = FALSE)

  result
}

run_cli_to_wgs = function(options) {
  codes = options$code

  if (is.null(codes)) {
    stop("`to-wgs` requires at least one `--code`.", call. = FALSE)
  }

  result = as.data.frame(toWGS(codes))
  utils::write.table(result, sep = "\t", row.names = FALSE, quote = FALSE)

  result
}
