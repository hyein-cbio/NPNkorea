#' Test whether strings are valid National Point Numbers
#'
#' \code{is_npn()} checks NPN strings without raising an error for malformed
#' values. It accepts the same Hangul and romanized forms as \code{toWGS()}.
#'
#' @param x A character vector. Missing values stay missing. Any other input
#'   type raises an error.
#'
#' @return A logical vector the same length as \code{x}. \code{TRUE} means the
#'   entry is a valid NPN string, \code{FALSE} means it is not, and \code{NA}
#'   means the entry was missing.
#'
#' @seealso \code{\link{format_npn}()} to normalize valid strings, and
#'   \code{\link{toWGS}()} to convert them to coordinates.
#' @family NPN conversion
#'
#' @examples
#' is_npn(c("다사 5396 5207", "DaSa53965207", "not an npn", NA))
#' @export

is_npn = function(x) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.")
  }

  vapply(x, function(value) {
    if (is.na(value)) {
      return(NA)
    }

    !inherits(try(parse_npn_string(value), silent = TRUE), "try-error")
  }, logical(1), USE.NAMES = FALSE)
}

#' Normalize National Point Number strings
#'
#' \code{format_npn()} parses NPN strings and reprints them in a canonical
#' Hangul or romanized form.
#'
#' @param x A character vector of NPN strings. Hangul and romanized input are
#'   accepted, romanized input is case-insensitive, and whitespace is optional.
#' @param output Output alphabet. \code{"han"} (default) returns Hangul labels
#'   such as \code{"다사"}, and \code{"abc"} returns the internal romanized
#'   casing such as \code{"DaSa"}.
#' @param spaced Whether to print the spaced form \code{XX 1234 5678}.
#'   \code{FALSE} returns the compact form \code{XX12345678}.
#'
#' @details
#' Invalid strings and missing values raise an error, matching \code{toWGS()}.
#' Use \code{is_npn()} to test values without stopping. An empty character
#' vector returns \code{character(0)}.
#'
#' @return A character vector the same length as \code{x}.
#'
#' @seealso \code{\link{is_npn}()} and \code{\link{toNPN}()}.
#' @family NPN conversion
#'
#' @examples
#' format_npn("dasa53965207")
#' format_npn("다사 5396 5207", output = "abc", spaced = FALSE)
#' @export

format_npn = function(x, output = "han", spaced = TRUE) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.")
  }

  output = match.arg(output, c("han", "abc"))

  if (!is.logical(spaced) || length(spaced) != 1 || is.na(spaced)) {
    stop("`spaced` must be TRUE or FALSE.")
  }

  if (anyNA(x)) {
    stop("`x` cannot contain missing values.")
  }

  if (length(x) == 0) {
    return(character(0))
  }

  parsed = lapply(x, parse_npn_string)

  format_npn_parts(
    vapply(parsed, function(item) item$x_digit, numeric(1)),
    vapply(parsed, function(item) item$y_digit, numeric(1)),
    vapply(parsed, function(item) item$x_pos, numeric(1)),
    vapply(parsed, function(item) item$y_pos, numeric(1)),
    output,
    spaced
  )
}
