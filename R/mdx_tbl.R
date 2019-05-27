mdx_tbl <- function() {
  structure(
    list(
      src = NULL,
      ops = NULL
    ),
    class = "mdx_tbl"
  )
}

tbl.OlapConnection <- function(src, cube) {
  arg <- rlang::enquo(cube)

  .data     <- mdx_tbl()
  .data$src <- src

  add_op(.data, 'cube', cube)
}

collect.mdx_tbl <- function() {

}

#' @importFrom pillar style_subtle
print.mdx_tbl <- function(x) {
  cat(style_subtle("# Cube:   "))
  cat(style_subtle("ProductCube"))
  cat(style_subtle("\n"))

  cat(style_subtle("# Filter: "))
  cat(style_subtle("ProductBusinessGroup == 'Jotex'"))
  cat(style_subtle("\n"))
}

misc <- function() {
  con_mdx <- jtxpkg::con_mdx()

  con_jf <- jtxpkg::con_jf()
  tbl_0 <- dplyr::tbl(con_jf, dplyr::sql("SELECT * FROM dbo.TEST_ROP (NOLOCK)"))
  tbl_1 <- dplyr::tbl(con_jf, dplyr::sql("SELECT * FROM dbo.TEST_ROP (NOLOCK)")) %>% dplyr::select(ROP)
}
