parse_coords_one <- function(coords_str, rect_str) {
  out <- c(x_left = NA_real_, x_center = NA_real_, y_top = NA_real_, y_center = NA_real_, y_bottom = NA_real_)
  if (!is.na(coords_str) && nzchar(coords_str)) {
    nums <- suppressWarnings(as.numeric(unlist(strsplit(coords_str, "[,\\s]+"))))
    if (length(nums) >= 2 && !all(is.na(nums))) {
      xs <- nums[seq(1, length(nums), by = 2)]
      ys <- nums[seq(2, length(nums), by = 2)]
      out["x_left"]   <- min(xs, na.rm = TRUE)
      out["x_center"] <- mean(xs, na.rm = TRUE)
      out["y_top"]    <- max(ys, na.rm = TRUE)
      out["y_center"] <- mean(ys, na.rm = TRUE)
      out["y_bottom"] <- min(ys, na.rm = TRUE)
      return(out)
    }
  }
  if (!is.na(rect_str) && nzchar(rect_str)) {
    parts <- suppressWarnings(as.numeric(unlist(strsplit(rect_str, "[,\\s]+"))))
    if (length(parts) >= 4 && !all(is.na(parts))) {
      L <- parts[1]; P2 <- parts[2]; R <- parts[3]; P4 <- parts[4]
      if (!is.na(P2) && !is.na(P4) && P2 > P4) {
        top <- P2; bottom <- P4
      } else {
        top <- P4; bottom <- P2
      }
      out["x_left"]   <- L
      out["x_center"] <- mean(c(L, R), na.rm = TRUE)
      out["y_top"]    <- top
      out["y_center"] <- mean(c(top, bottom), na.rm = TRUE)
      out["y_bottom"] <- bottom
      return(out)
    }
  }
  out
}
