add_op <- function(.data, name, info) {
  prev_ops <- .data$ops

  .data$ops      <- NULL
  .data$ops$x    <- prev_ops
  .data$ops$name <- name
  .data$ops$info <- info

  .data
}

#' Specify Cube
#' @export
cube <- function(src, cube) {
  arg <- rlang::enquo(cube)

  .data     <- mdx_tbl()
  .data$src <- src

  add_op(.data, 'cube', cube)
}

#' Specify Values
#' @export
values <- function(.data, ...) {
  arg <- rlang::enquos(...)
  add_op(.data, 'values', arg)
}

#' Specify Rows
#' @export
rows <- function(.data, ...) {
  arg <- rlang::enquos(...)
  add_op(.data, 'rows', arg)
}

#' Specify Filters
#' @export
filters <- function(.data, ...) {
  arg <- rlang::enquos(...)
  add_op(.data, 'filters', arg)
}
