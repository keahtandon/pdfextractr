extract_details <- function(node) {
  # node is an xml_node (one annotation)
  if (missing(node) || !inherits(node, "xml_node")) {
    stop("extract_details: expected an xml_node as input")
  }

  elem_name <- xml_name(node)      # e.g., "highlight"
  local_nm  <- xml_name(node)      # xml2's xml_name returns local name if no prefix shown
  page  <- xml_attr(node, "page")
  rect  <- xml_attr(node, "rect")
  coords <- xml_attr(node, "coords") %||% xml_attr(node, "quadPoints")  # handle either attr
  color <- xml_attr(node, "color")
  title <- xml_attr(node, "title")  # this is the author in your snippet
  date  <- xml_attr(node, "date")

  # extract textual content (richtext or fallback)
  txt <- extract_text_from_node(n)

  cc <- coords_to_center(coords)
  tibble::tibble(
    element = elem_name,
    page = dplyr::if_else(is.null(page) | page == "", NA, as.integer(page)),
    author = dplyr::if_else(is.null(title) | title == "", NA, title),
    date = lubridate::ymd(str_sub(date, 3, 10)),
    color = dplyr::if_else(is.null(color) | color == "", NA, color),
    rect = dplyr::if_else(is.null(rect) | rect == "", NA, rect),
    coords = dplyr::if_else(is.null(coords) | coords == "", NA, coords),
    x_center = cc$x_center,
    y_center = cc$y_center,
    text_raw = dplyr::if_else(is.na(txt), "", txt)
  )
}
