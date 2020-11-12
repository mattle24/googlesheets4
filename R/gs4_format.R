new_gs4_format_number <- function(x = double(), pattern = "0.00") {
  vec_assert(x, double())
  vec_assert(pattern, ptype = character(), size = 1)
  new_vctr(x, pattern = pattern, class = "googlesheets4_format_number")
}

#' Class for Google Sheets number formats
#'
#' Often, we want to see our Google Sheets data in a "pretty" format, for
#' example 0.45 as 45%, without changing the underlying data. In order to format
#' numbers into Google Sheets, you need to store it as an object of class
#' `googlesheets4_format_number`. This is how we know how to format a given
#' numeric vector. `googlesheets4_format_number` is an S3 class implemented
#' using the [vctrs package](https://vctrs.r-lib.org/articles/s3-vector.html).
#'
#' @param x Double.
#' @param pattern Character. Defaults to "0.00", ie round with two significant
#'   digits. For options see the [Google
#'   Documentation](https://developers.google.com/sheets/api/guides/formats).
#'
#' @return An S3 vector of class `googlesheets4_format_number`.
#' @export
#' @family write functions
#'
#' @examples
#' if (gs4_has_token()) {
#'   dat <- data.frame(small_number = runif(10), big_number = runif(10) * 1e6)
#'   # explicitly declare columns as `googlesheets4_format_number`
#'   dat$small_number <- gs4_format_number(dat$small_number, "0.0%")
#'   # from https://webapps.stackexchange.com/questions/77974
#'   dat$big_number <- gs4_format_number(dat$big_number, "[>999999]0.0,,\\M;[>999]0.0,\\K;0")
#'
#'   # make the sheet
#'   ss <- gs4_create("gs4-number-formats-demo", sheets = dat)
#'   ss
#'
#'   # clean up
#'   gs4_find("gs4-formula-demo") %>%
#'     googledrive::drive_trash()
#' }
gs4_format_number <- function(x = double(), pattern = "0.00") {
  x <- vec_cast(x, double())
  pattern <- vec_recycle(vec_cast(pattern, character()), 1)

  new_gs4_format_number(x, pattern = pattern)
}

#' @importFrom methods setOldClass
setOldClass(c("googlesheets4_format_number", "vctrs_vctr"))

#' @export
vec_ptype_abbr.googlesheets4_format_number <- function(x, ...) {
  "fmt_num"
}

#' @method vec_ptype2 googlesheets4_format_number
#' @export vec_ptype2.googlesheets4_format_number
#' @export
#' @rdname googlesheets4-vctrs
vec_ptype2.googlesheets4_format_number <- function(x, y, ...) {
  UseMethod("vec_ptype2.googlesheets4_format_number", y)
}

#' @method vec_ptype2.googlesheets4_format_number default
#' @export
vec_ptype2.googlesheets4_format_number.default <- function(x, y,
                                                     ...,
                                                     x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.googlesheets4_format_number googlesheets4_format_number
#' @export
vec_ptype2.googlesheets4_format_number.googlesheets4_format_number <- function(x, y, ...) new_gs4_format_number()

#' @method vec_ptype2.googlesheets4_format_number double
#' @export
vec_ptype2.googlesheets4_format_number.double <- function(x, y, ...) double()

#' @method vec_ptype2.double googlesheets4_format_number
#' @export
vec_ptype2.double.googlesheets4_format_number <- function(x, y, ...) double()

# casting

#' @method vec_cast googlesheets4_format_number
#' @export vec_cast.googlesheets4_format_number
#' @export
#' @rdname googlesheets4-vctrs
vec_cast.googlesheets4_format_number<- function(x, to, ...) {
  UseMethod("vec_cast.googlesheets4_format_number")
}

#' @method vec_cast.googlesheets4_format_number default
#' @export
vec_cast.googlesheets4_format_number.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.googlesheets4_format_number googlesheets4_format_number
#' @export
vec_cast.googlesheets4_format_number.googlesheets4_format_number <- function(x, to, ...) {
  x
}

#' @method vec_cast.googlesheets4_format_number double
#' @export
vec_cast.googlesheets4_format_number.double <- function(x, to, ...) {
  gs4_format_number(x)
}

#' @method vec_cast.double googlesheets4_format_number
#' @export
vec_cast.double.googlesheets4_format_number <- function(x, to, ...) {
  vec_data(x)
}
