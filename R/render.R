render <- function(.data) {
  paste0("SELECT ", render_values(.data), " ON 0 FROM ", render_cube(.data))
}

render_cube <- function(.data) {
  paste0("[", find_cube(.data), "]")
}

render_values <- function(.data) {
  values_str <- find_values(.data) %>% lapply(rlang::quo_name)
  paste0("[Measures].[", values_str, "]") %>%
    paste0(collapse = ", ") %>%
    paste0("{ ", ., " }")
}

#' Return Cube in .data
find_cube <- function(.data) {
  find_(.data, "cube")$info
}

#' Return Values in .data
find_values <- function(.data) {
  find_(.data, "values")$info
}

#' Return Rows in .data
find_values <- function(.data) {
  find_(.data, "values")$info
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


