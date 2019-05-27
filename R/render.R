render <- function(.data) {
  render_cube(.data)
}

render_cube <- function(.data) {
  paste0("[", find_cube(.data), "]")
}

#' Return Name of Cube
find_cube <- function(.data) {
  find_(.data, "cube")$info
}

#' Return First Operator with Name
find_ <- function(.data, name) {

  i      <- 1
  i_op   <- NULL
  i_name <- ""
  while (i_name != name) {
    i_op   <- op(.data, i)
    i_name <- i_op$name
    i <- i + 1
    if (i > 20) break
  }

  i_op
}

#' Return Operator at Level
op <- function(.data, level) {

  op <- .data$ops
  for (l in seq(1, level)) {
    if (l == 1) next
    op <- op$x
  }

  op
}


