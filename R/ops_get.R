#' Return Cube in .data
find_cube <- function(.data) {
  find_(.data, "cube")$info
}

#' Return Values in .data
find_values <- function(.data) {
  find_(.data, "values")$info
}

#' Return Rows in .data
find_rows <- function(.data) {
  find_(.data, "rows")$info
}

#' Return Filters in .data
find_filters <- function(.data) {
  find_(.data, "filters")$info
}

#' Return First Operator with Name
#' Returns NULL if Operator not found
find_ <- function(.data, name) {

  i      <- 1
  i_op   <- NULL
  i_name <- ""
  while (i_name != name) {
    i_op   <- op(.data, i)
    if (is.null(i_op)) return(NULL)
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
