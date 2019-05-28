#' Execute Query
execute_query <- function(.data) {
  tbl <- tibble::as_tibble(olapR::execute2D(.data$src, render(.data)))
  colnames(tbl) <- clean_colnames(colnames(tbl))
  tbl
}

#' Clean Column Names
#'
#' Extract [A-Za-z0-9_]* before 'MEMBER_CAPTION' or after 'Measures'
clean_colnames <- function(names) {
  stringr::str_extract(names, "[A-Za-z0-9_]*(?=\\]\\.\\[MEMBER_CAPTION)|(?<=Measures\\]\\.\\[)[A-Za-z0-9_]*")
}
