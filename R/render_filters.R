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
