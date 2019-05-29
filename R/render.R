#' Show MDX Query
#' @export
show_query <- function(.data) {
  render(.data)
}

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
