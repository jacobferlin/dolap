render <- function(.data) {

  query_cols <- paste0(render_values(.data), " ON 0")

  query_rows <- ifelse(
    is.null(find_rows(.data)),
    " ",
    paste0(", ", render_rows(.data), " ON 1 ")
  )

  query_filters <- ifelse(
    is.null(find_filters(.data)),
    paste0("FROM ", render_cube(.data)),
    render_filters(.data)
  )

  paste0(
    "SELECT ",
    query_cols,
    query_rows,
    query_filters
  )
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

render_rows <- function(.data) {
  rows_str <- find_rows(.data) %>% lapply(rlang::quo_name)
  dims     <- stringr::str_extract(rows_str, "[:alpha:]+(?=\\.)")
  hiers    <- stringr::str_extract(rows_str, "(?<=\\.)[:alpha:]+")
  paste0("[", dims, "].[", hiers, "].CHILDREN") %>%
    paste0(collapse = ", ") %>%
    paste0("( ", ., " )")
}

render_filters <- function(.data) {
  # Split quos into tbl, a quo for each row.
  # Convert op to string.
  # Get a string representation of the filter, depending on the specific operator.
  quos <- find_filters(.data)
  tbl  <- tibble::tibble(quo = quos)
  tbl  <- dplyr::mutate(tbl,
    op  = quo_op_str(quo),
    ren = purrr::map2_chr(quo, op, render_filters_meta)
  )

  filters <- tbl$ren

  # Nest every filter in a 'FROM ( SELECT [] ON 0 )' statement
  # Most nestled is cube
  filters_nest <- paste0("FROM ", render_cube(.data))
  no_nestles   <- length(filters)
  for (i in seq(1, no_nestles, 1)) {
    filters_nest <- paste0("FROM ( SELECT ( ", filters[i], " ) ON 0 ", filters_nest, " ) ")
  }

  filters_nest
}

render_filters_meta <- function(quo, op) {
  switch(op,
    "=="   = render_filters_equal(quo),
    "%in%" = render_filters_in(quo),
    "%:%"  = render_filters_range(quo))
}

render_filters_equal <- function(quo) {
  left  <- rlang::quo_name(quo[[2]][[2]])
  dims  <- stringr::str_extract(left, "[:alpha:]+(?=\\.)")
  hiers <- stringr::str_extract(left, "(?<=\\.)[:alpha:]+")

  right <- rlang::eval_tidy(quo[[2]][[3]])

  paste0("[", dims, "].[", hiers, "].[", right, "]")
}

render_filters_in <- function(quo) {
  left  <- rlang::quo_name(quo[[2]][[2]])
  dims  <- stringr::str_extract(left, "[:alpha:]+(?=\\.)")
  hiers <- stringr::str_extract(left, "(?<=\\.)[:alpha:]+")

  right <- rlang::eval_tidy(quo[[2]][[3]])

  paste0("[", dims, "].[", hiers, "].[", right, "]") %>%
    paste0(collapse = ", ") %>%
    paste0("{ ", ., " }")
}

render_filters_range <- function(quo) {
  left  <- rlang::quo_name(quo[[2]][[2]])
  dims  <- stringr::str_extract(left, "[:alpha:]+(?=\\.)")
  hiers <- stringr::str_extract(left, "(?<=\\.)[:alpha:]+")

  right <- rlang::eval_tidy(quo[[2]][[3]])
  right_first <- right[[1]]
  right_last  <- right[[length(right)]]

  right_first <- paste0("[", dims, "].[", hiers, "].[", right_first, "]")
  right_last  <- paste0("[", dims, "].[", hiers, "].[", right_last,  "]")

  paste0(right_first, ":", right_last)
}

quo_op_str <- function(quos) {
  purrr::map_chr(quos, ~rlang::quo_name(.[[2]][[1]]))
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


