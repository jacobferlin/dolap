#' MDX_TBL Constructor
mdx_tbl <- function() {
  structure(
    list(
      src = NULL,
      ops = NULL
    ),
    class = "mdx_tbl"
  )
}

collect.mdx_tbl <- function() {

}

#' @importFrom pillar style_subtle
print.mdx_tbl <- function(.data) {

  cat(style_subtle("# Cube:   "))
  cat(style_subtle(find_cube(.data)))
  cat(style_subtle("\n"))

  if (has_filters(.data)) {
    filters_quos <- find_filters(.data)
    filters_str  <- quos_expr_str(filters_quos)

    cat(style_subtle("# Filter: "))

    print_filter_seq <- seq(1, length(filters_str), 1)
    for (print_filter_row in print_filter_seq) {
      if (print_filter_row != 1) cat(style_subtle("#         "))
      cat(style_subtle(filters_str[print_filter_row]))
      cat(style_subtle("\n"))
    }

  }

  cat(style_subtle("# Waiting for query..."))

  tbl <- execute_query(.data)

  cat(style_subtle("\r"))
  print(tbl)
}

quos_expr_str <- function(quos) {
  lapply(quos, rlang::quo_name) %>%
    unlist(use.names = FALSE) %>%
    stringr::str_replace_all("\\\"", "'")
}

misc <- function() {
  con_mdx <- jtxpkg::con_mdx()

  con_jf <- jtxpkg::con_jf()
  tbl_0 <- dplyr::tbl(con_jf, dplyr::sql("SELECT * FROM dbo.TEST_ROP (NOLOCK)"))
  tbl_1 <- dplyr::tbl(con_jf, dplyr::sql("SELECT * FROM dbo.TEST_ROP (NOLOCK)")) %>% dplyr::select(ROP)
}
