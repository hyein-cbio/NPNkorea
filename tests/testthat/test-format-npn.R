test_that("is_npn accepts equivalent spellings and rejects malformed strings", {
  expect_equal(
    is_npn(c(
      "다사 5396 5207",
      "다사53965207",
      "DaSa 5396 5207",
      "dasa53965207",
      "  DaSa   5396  5207  ",
      "not an npn",
      "   ",
      "갸사 5396 5207",
      NA_character_
    )),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA)
  )
  expect_equal(is_npn(character(0)), logical(0))
  expect_error(is_npn(1), "`x` must be a character vector.")
  expect_error(is_npn(factor("다사 5396 5207")), "`x` must be a character vector.")
})

test_that("format_npn canonicalizes Hangul and romanized labels", {
  expect_equal(format_npn("dasa53965207"), "다사 5396 5207")
  expect_equal(
    format_npn(c("다사53965207", "  MaRa 4347 8827 "), output = "abc"),
    c("DaSa 5396 5207", "MaRa 4347 8827")
  )
  expect_equal(
    format_npn("다사 5396 5207", output = "abc", spaced = FALSE),
    "DaSa53965207"
  )
  expect_equal(format_npn("DaDa06940013", spaced = FALSE), "다다06940013")
  expect_equal(format_npn(character(0)), character(0))
  expect_equal(format_npn(character(0), output = "abc", spaced = FALSE), character(0))
})

test_that("every grid-band pair can be recognized and canonicalized", {
  digits = get("digits.df", envir = asNamespace("NPNkorea"))
  han = paste0(rep(digits$han, each = nrow(digits)), rep(digits$han, nrow(digits)))
  abc = paste0(rep(digits$abc, each = nrow(digits)), rep(digits$abc, nrow(digits)))
  compact_han = paste0(han, "00009999")
  spaced_abc = paste0(abc, " 1234 0001")
  messy_abc = paste0("  ", tolower(abc), "1234", "0001  ")

  expect_true(all(is_npn(compact_han)))
  expect_true(all(is_npn(spaced_abc)))
  expect_true(all(is_npn(messy_abc)))
  expect_equal(format_npn(compact_han), paste0(han, " 0000 9999"))
  expect_equal(format_npn(spaced_abc, output = "han"), paste0(han, " 1234 0001"))
  expect_equal(format_npn(messy_abc, out = "abc", spaced = FALSE), paste0(abc, "12340001"))
  expect_equal(format_npn("AGA00010002"), "아가 0001 0002")
  expect_equal(format_npn("gaa00010002", output = "abc"), "GaA 0001 0002")
  expect_equal(format_npn("aa00000001", spaced = FALSE), "아아00000001")
})

test_that("is_npn rejects incomplete, mixed, and junk strings", {
  invalid = c(
    "",
    " ",
    "다",
    "다사",
    "다사 5396",
    "다사 5396 207",
    "다사 5396 52070",
    "다사5396520",
    "다Sa53965207",
    "DS53965207",
    "Da53965207",
    "다사 5396 5207 extra",
    "x다사53965207",
    "12345678",
    "다사 ---- ----",
    "갸갸 0000 0000"
  )

  expect_equal(is_npn(invalid), rep(FALSE, length(invalid)))
  expect_equal(is_npn(NA_character_), NA)
  expect_equal(is_npn(c(NA_character_, "다사 0000 0000", NA_character_)), c(NA, TRUE, NA))
  expect_error(is_npn(NULL), "`x` must be a character vector.")
  expect_error(is_npn(list("다사 5396 5207")), "`x` must be a character vector.")
})

test_that("format_npn rejects missing and malformed strings", {
  expect_error(
    format_npn(c("다사 5396 5207", NA_character_)),
    "`x` cannot contain missing values."
  )
  expect_error(format_npn("not an npn"), "must be valid NPN strings")
  expect_error(format_npn(1), "`x` must be a character vector.")
  expect_error(format_npn("다사 5396 5207", spaced = NA), "`spaced` must be TRUE or FALSE.")
  expect_error(format_npn("다사 5396 5207", output = "latin"), "should be one of")
})
