new_gs4_format <- function(x = double(), type = character(), pattern = character()) {
  vec_assert(x, double())
  vec_assert(type, ptype = character())
  vec_assert(pattern, ptype = character())
  new_vctr(x, type = type, pattern = pattern, class = "googlesheets4_format")
}

gs4_format <- function(x = double(), type = character(), pattern = character()) {
  x <- vec_cast(x, double())
  type <- vec_cast(type, character())
  pattern <- vec_cast(pattern, character())

  new_gs4_format(x, type = type, pattern = pattern)
}

vec_ptype2.gs4_format.gs4_format <- function(x, y, ...) new_gs4_format()


