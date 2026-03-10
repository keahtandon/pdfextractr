compute_reading_order <- function(df, n_columns = NULL, line_tol_mult = 0.6, verbose = TRUE) {
  # safety
  if (!("page" %in% names(df))) stop("df must contain a 'page' column")
  if (!("coords" %in% names(df))) df$coords <- NA_character_
  if (!("rect" %in% names(df))) df$rect <- NA_character_

  # drop old aux cols if present
  drop_cols <- c("x_left","x_center","y_top","y_center","y_bottom","col_index","band","read_order")
  df <- df %>% select(-any_of(intersect(drop_cols, names(df))), everything())

  # compute geom temp table
  geom_list <- lapply(seq_len(nrow(df)), function(i) {
    parse_coords_one(df$coords[i] %||% NA_character_, df$rect[i] %||% NA_character_)
  })
  geom_mat <- do.call(rbind, geom_list)
  geom_temp <- as.data.frame(geom_mat, stringsAsFactors = FALSE)
  names(geom_temp) <- paste0(".__tmp__", names(geom_temp))

  # attach temp columns
  df_temp <- bind_cols(df, geom_temp)

  # ensure canonical columns exist (create as NA if missing) BEFORE mutate/coalesce
  canonical_cols <- c("x_left","x_center","y_top","y_center","y_bottom")
  for (cname in canonical_cols) {
    if (!cname %in% names(df_temp)) df_temp[[cname]] <- NA_real_
  }

  # now coalesce safely (any existing canonical values preferred, then temp)
  df_temp <- df_temp %>%
    mutate(
      x_left   = as.numeric(dplyr::coalesce(.data[["x_left"]], .data[[".__tmp__x_left"]])),
      x_center = as.numeric(dplyr::coalesce(.data[["x_center"]], .data[[".__tmp__x_center"]])),
      y_top    = as.numeric(dplyr::coalesce(.data[["y_top"]], .data[[".__tmp__y_top"]])),
      y_center = as.numeric(dplyr::coalesce(.data[["y_center"]], .data[[".__tmp__y_center"]])),
      y_bottom = as.numeric(dplyr::coalesce(.data[["y_bottom"]], .data[[".__tmp__y_bottom"]]))
    )

  # remove temp columns
  df_temp <- df_temp %>% select(-starts_with(".__tmp__"))

  # detect n_columns if needed
  if (is.null(n_columns)) {
    med_ncols <- df_temp %>%
      group_by(page) %>%
      summarize(nuniq = n_distinct(na_if(round(x_left, 0), NA)), .groups = "drop") %>%
      summarize(median_n = median(nuniq[nuniq > 0], na.rm = TRUE)) %>%
      pull(median_n)
    if (is.na(med_ncols) || med_ncols <= 1) {
      n_columns <- 1L
    } else {
      n_columns <- min(3L, max(1L, as.integer(round(med_ncols))))
    }
    if (verbose) message("Auto-detected n_columns = ", n_columns)
  }

  # per-page processing (group_modify must NOT return the grouping var)
  df_out <- df_temp %>%
    group_by(page) %>%
    group_modify(~ {
      page_df_full <- .x
      page_df <- page_df_full %>% select(-page)

      # clustering
      xs <- page_df$x_left
      if (n_columns <= 1 || sum(!is.na(xs)) < 2) {
        page_df$col_index <- 1L
      } else {
        clustering <- safe_kmeans_clustering(xs, n_columns)
        if (is.null(clustering)) {
          non_na_idx <- which(!is.na(xs))
          if (length(non_na_idx) < 2) {
            page_df$col_index <- 1L
          } else {
            probs <- seq(0,1,length.out = (n_columns+1))
            brks <- unique(quantile(xs[non_na_idx], probs = probs, na.rm = TRUE))
            if (length(brks) <= 1) {
              page_df$col_index <- 1L
            } else {
              qcuts <- as.integer(cut(xs[non_na_idx], breaks = brks, include.lowest = TRUE))
              assign_vec <- rep(NA_integer_, length(xs))
              assign_vec[non_na_idx] <- qcuts
              assign_vec[is.na(assign_vec)] <- 1L
              page_df$col_index <- assign_vec
            }
          }
        } else {
          assign_vec <- rep(NA_integer_, length(xs))
          non_na_idx <- which(!is.na(xs))
          assign_vec[non_na_idx] <- clustering$cluster
          centers <- as.numeric(clustering$centers)
          order_map <- order(centers)
          remap <- integer(length(order_map))
          remap[order_map] <- seq_along(order_map)
          assign_vec[non_na_idx] <- remap[assign_vec[non_na_idx]]
          assign_vec[is.na(assign_vec)] <- 1L
          page_df$col_index <- assign_vec
        }
      }

      # banding
      yvals <- sort(unique(na.omit(page_df$y_top)), decreasing = TRUE)
      if (length(yvals) <= 1) {
        line_spacing <- 12 * line_tol_mult
      } else {
        diffs <- abs(diff(yvals))
        line_spacing <- median(diffs, na.rm = TRUE) * line_tol_mult
        if (is.na(line_spacing) || line_spacing <= 0) line_spacing <- 12 * line_tol_mult
      }
      page_df$band <- ifelse(is.na(page_df$y_top), NA_integer_, floor(page_df$y_top / line_spacing))
      page_df$band[is.na(page_df$band)] <- 0L

      page_df$col_index <- as.integer(page_df$col_index)
      page_df$band <- as.integer(page_df$band)

      page_df
    }, .keep = TRUE) %>%
    ungroup()

  # final ordering
  df_final <- df_out %>%
    mutate(
      page = as.integer(page),
      col_index = ifelse(is.na(col_index), 1L, as.integer(col_index)),
      band = ifelse(is.na(band), 0L, as.integer(band))
    ) %>%
    arrange(page, col_index, desc(band), x_left) %>%
    mutate(read_order = row_number())

  return(df_final)
}
