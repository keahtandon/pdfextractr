clean_pdf_text <- function(x, transliterate = FALSE) {
  out <- x %>%
    str_replace_all("[\r\n]+", " ") %>%
    str_replace_all("\u00A0", " ") %>%
    str_replace_all("[\u200B-\u200D\uFEFF]", "") %>%
    str_replace_all("\u2022", "- ") %>%
    str_replace_all("\u2026", "...") %>%
    str_replace_all("\u201C|\u201D", '"') %>%
    str_replace_all("\u2018|\u2019", "'") %>%
    str_replace_all("\u2013", "-") %>%
    str_replace_all("\u2014", " - ") %>%
    str_replace_all("\\p{C}", "") %>%
    str_squish()

  if (transliterate) {
    # optional: convert accented characters to ASCII (requires stringi)
    if (!requireNamespace("stringi", quietly = TRUE)) {
      warning("stringi not installed; install to enable transliteration")
    } else {
      out <- stringi::stri_trans_general(out, "Latin-ASCII")
    }
  }
  out
}
