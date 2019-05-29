mdx_qry <- function() {
  structure(
    list(
      qry = NULL
    ),
    class = c("mdx_qry")
  )
}

#' Show MDX Query
#' @export
show_query <- function(.data) {

  obj     <- mdx_qry()
  obj$qry <- render(.data)

  obj
}

print.mdx_qry <- function(qry) {

  qry_format <- qry %>%
    stringr::str_replace_all("SELECT ", "SELECT\n  ") %>%
    stringr::str_replace_all("ON 0,", "ON 0,\n ") %>%
    stringr::str_replace_all("(?<=\\]),", ",\n   ") %>%
    stringr::str_replace_all("(?<=\\.CHILDREN),", ",\n   ") %>%
    stringr::str_replace_all("FROM", "\nFROM")

  cat(qry_format)
  invisible(qry)
}
