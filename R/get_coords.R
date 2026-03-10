coords_to_center <- function(coords_str) {
  if (is.na(coords_str) || !nzchar(coords_str)) return(list(x_center = NA_real_, y_center = NA_real_))
  # split on commas and/or spaces
  nums <- as.numeric(unlist(strsplit(coords_str, "[,\\s]+")))
  if (length(nums) < 2) return(list(x_center = NA_real_, y_center = NA_real_))
  xs <- nums[seq(1, length(nums), by = 2)]
  ys <- nums[seq(2, length(nums), by = 2)]
  x_center <- mean(xs, na.rm = TRUE)
  y_center <- mean(ys, na.rm = TRUE)
  list(x_center = x_center, y_center = y_center)
}
