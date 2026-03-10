extract_details <- function(i) {
  n <- ann_nodes[[i]]
  # element name (qualified) and local name
  elem_name <- xml_name(n)      # e.g., "highlight"
  local_nm  <- xml_name(n)      # xml2's xml_name returns local name if no prefix shown

  # attributes from highlight element
  page  <- xml_attr(n, "page")
  rect  <- xml_attr(n, "rect")
  coords <- xml_attr(n, "coords") %||% xml_attr(n, "quadPoints")  # handle either attr
  color <- xml_attr(n, "color")
  title <- xml_attr(n, "title")  # this is the author in your snippet

  # extract textual content (richtext or fallback)
  txt <- extract_text_from_node(n)

  # compute numeric centers for sorting
  cc <- coords_to_center(coords)
  tibble::tibble(
    element = elem_name,
    page = ifelse(is.null(page) || page == "", NA, as.integer(page)),
    author = ifelse(is.null(title) || title == "", NA, title),
    color = ifelse(is.null(color) || color == "", NA, color),
    rect = ifelse(is.null(rect) || rect == "", NA, rect),
    coords = ifelse(is.null(coords) || coords == "", NA, coords),
    x_center = cc$x_center,
    y_center = cc$y_center,
    text_raw = ifelse(is.na(txt), "", txt)
  )
}
