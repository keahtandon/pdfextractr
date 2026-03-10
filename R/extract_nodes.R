extract_nodes <- function (xfdf) {

  raw <- read_xml(xfdf)

  ann_nodes <- xml_find_all(raw,
                            "//*[local-name() = 'highlight' or local-name() = 'underline' or local-name() = 'text']")

  rows <- purrr::map_df(ann_nodes, extract_details)

  df <- rows %>%
    mutate(page = as.integer(page) + 1,
    text = str_squish(text_raw)) %>%
    compute_reading_order() %>%
    rename(hex = color) %>%
    mutate(text = clean_pdf_text(text)) %>%
    select(!c(rect:text_raw, x_center:read_order))

  df2 <- df %>%
    color_rename()

  return(df2)

}
