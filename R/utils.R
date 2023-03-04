## Various utils function

# Negate
`%notin%` <- Negate(`%in%`)

# function to capitalize first letter
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


asymm_index = function(m) {
  which(m != t(m), arr.ind = TRUE)
}

map_progress <- function(.x, .f, ..., .id = NULL) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = length(.x), format = " [:bar] :current/:total (:percent) eta: :eta", force = TRUE)

  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map(.x, f, ..., .id = .id)
}

