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

print.mdx_qry <- function(obj) {

  qry_format <- obj$qry %>%
    stringr::str_replace_all("SELECT ", "SELECT\n  ") %>%
    stringr::str_replace_all("ON 0,", "ON 0,\n") %>%
    stringr::str_replace_all("(?<=\\]),", ",\n") %>%
    stringr::str_replace_all("(?<=\\.CHILDREN),", ",\n") %>%
    stringr::str_replace_all("FROM", "\nFROM") %>%
    stringr::str_replace_all("FROM \\(", "FROM\n  (")

  qry_format <- indent(qry_format)

  cat(qry_format)

  invisible(obj$qry)
}

# Insert Spaces for Indentation
indent <- function(str) {
  # First '[' should be at same char pos

  # Split str at '\n'
  str_vec <- stringr::str_split(str, "\n") %>% .[[1]]

  str_vec_new <- vector(mode = "character", length = length(str_vec))
  str_vec_new[[1]] <- str_vec[[1]]
  for (i in seq(2, length(str_vec), 1)) {

    # Look at previous line
    # If no '[' present, skip to next
    pos_prev_line    <- str_vec_new[[i - 1]] %>% stringr::str_locate('\\[') %>% .[1, 1]
    if (is.na(pos_prev_line)) {
      str_vec_new[i] <- str_vec[i]
      next
    }

    # Look at current line
    # If no '[' present, skip to next
    pos_current_line <- str_vec[[i    ]] %>% stringr::str_locate('\\[') %>% .[1, 1]
    if (is.na(pos_current_line)) {
      str_vec_new[i] <- str_vec[i]
      next
    }

    # Calc diff (how many spaces to insert)
    diff <- pos_prev_line - pos_current_line
    if (diff <= 0) {
      str_vec_new[i] <- str_vec[i]
      next
    }

    # Insert spaces
    spaces <- rep(" ", diff) %>% paste0(collapse = "")
    str_vec_new[[i]] <- paste0(spaces, str_vec[[i]])
  }

  str_vec_new %>% paste0(collapse = "\n")


}
