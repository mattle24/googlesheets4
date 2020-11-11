new_gs4_format_number <- function(x = double(), pattern = character()) {
  vec_assert(x, double())
  vec_assert(pattern, ptype = character())
  new_vctr(x, type = type, pattern = pattern, class = "googlesheets4_format_number")
}

#' Class for Google Sheets number formats
#'
#' TODO
#' In order to write a formula into Google Sheets, you need to store it as an
#' object of class `googlesheets4_formula`. This is how we distinguish a
#' "regular" character string from a string that should be interpreted as a
#' formula. `googlesheets4_formula` is an S3 class implemented using the [vctrs
#' package](https://vctrs.r-lib.org/articles/s3-vector.html).
#'
#' @param x Double.
#' @param pattern Character. See \link{https://developers.google.com/sheets/api/guides/formats}
#'
#' @return An S3 vector of class `googlesheets4_format_number`.
#' @export
#' @family write functions
#'
#' @examples
#' if (gs4_has_token()) {
#'   dat <- data.frame(small_number = runif(12), big_number = runif(12) * 1e6)
#'   # explicitly declare columns as `googlesheets4_format_number`
#'   dat$small_number <- gs4_format_number(dat$small_number, "0.0%")
#'   # from https://webapps.stackexchange.com/questions/77974/short-number-format-in-google-sheets-1-024-%E2%86%92-1k-1-816-724-%E2%86%92-1-8m
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
gs4_format_number <- function(x = double(), pattern = character()) {
  x <- vec_cast(x, double())
  pattern <- vec_cast(pattern, character())

  new_gs4_format_number(x, pattern = pattern)
}

vec_ptype2.gs4_format.gs4_format_number <- function(x, y, ...) new_gs4_format()


