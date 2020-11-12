test_that("gs4_format_number constructors return length-0 vector when called with no arguments", {
  expect_length(new_gs4_format_number(), 0)
  expect_length(gs4_format_number(), 0)
})

test_that("gs4_format_number low-level constructor errors for non-double input", {
  expect_error(new_gs4_format_number(letters[1:3]), class = "vctrs_error_assert_ptype")
})

test_that("common type of googlesheets4_format_number and double is double", {
  expect_identical(
    vctrs::vec_ptype2(double(), gs4_format_number()),
    double()
  )
  expect_identical(
    vctrs::vec_ptype2(gs4_format_number(), double()),
    double()
  )
})

test_that("googlesheets4_format_number and double are coercible", {
  expect_identical(
    vctrs::vec_cast(1, gs4_format_number()),
    gs4_format_number(1)
  )
  expect_identical(
    vctrs::vec_cast(gs4_format_number(1), double()),
    1
  )
  expect_identical(
    vctrs::vec_cast(gs4_format_number(1), gs4_format_number()),
    gs4_format_number(1)
  )
})

test_that("can concatenate googlesheets4_format_number", {
  expect_identical(
    vctrs::vec_c(
      gs4_format_number(1),
      gs4_format_number(2)
    ),
    gs4_format_number(c(1, 2))
  )
})

test_that("googlesheets4_format_number can have missing elements", {
  out <- vctrs::vec_c(
    gs4_format_number(1),
    NA,
    gs4_format_number(2),
    NA
  )
  expect_s3_class(out, "googlesheets4_format_number")
  expect_true(all(is.na(out[c(2, 4)])))
})
