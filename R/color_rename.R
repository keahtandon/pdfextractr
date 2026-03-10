color_rename <- function(df) {

  hex_to_check <- df %>%
    select(hex) %>%
    filter(!is.na(hex)) %>%
    distinct()

  colors <- hex_to_check %>%
    mutate(color = hex_to_color(hex))

  df2 <- df %>%
    left_join(colors, by = "hex") %>%
    relocate(color, .after = hex)

  return(df2)

}
