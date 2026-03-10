extract_text_from_node <- function(node) {
  # prefer contents-richtext -> body -> all text inside (keeps order)
  rich <- xml_find_first(node, ".//*[local-name() = 'contents-richtext']")
  if (!is.na(rich)) {
    # typically contains <body> with <p>/<span> children; xml_text collapses child text in order
    txt <- xml_text(rich)
  } else {
    # fallback: plain <contents> element or attribute
    cnd <- xml_find_first(node, ".//*[local-name() = 'contents']")
    if (!is.na(cnd)) txt <- xml_text(cnd) else {
      # if no element, try attribute 'contents' (rare)
      txt <- xml_attr(node, "contents")
      if (is.null(txt)) txt <- NA_character_
    }
  }
  # xml2 will translate character entities like &#13; to actual \r; normalize them:
  if (!is.na(txt) && nzchar(txt)) {
    # replace CR/LF sequences with single space, and collapse multiple spaces
    txt <- gsub("[\r\n]+", " ", txt)
    txt <- gsub("\\s{2,}", " ", txt)
    txt <- str_trim(txt)
  }
  txt
}
