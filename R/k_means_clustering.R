safe_kmeans_clustering <- function(xs, k) {
  non_na_idx <- which(!is.na(xs))
  if (length(non_na_idx) < 2) return(NULL)
  xs_non_na <- xs[non_na_idx]
  uniq_count <- length(unique(xs_non_na))
  k_use <- min(k, uniq_count)
  if (k_use <= 1) return(NULL)
  km <- tryCatch(kmeans(xs_non_na, centers = k_use, nstart = 10), error = function(e) NULL)
  if (is.null(km)) {
    probs <- seq(0, 1, length.out = (k_use + 1))
    breaks <- unique(quantile(xs_non_na, probs = probs, na.rm = TRUE))
    if (length(breaks) <= 1) return(NULL)
    qcuts <- as.integer(cut(xs_non_na, breaks = breaks, include.lowest = TRUE))
    return(list(cluster = qcuts, centers = tapply(xs_non_na, qcuts, mean)))
  } else {
    centers <- tapply(xs_non_na, km$cluster, mean)
    return(list(cluster = km$cluster, centers = centers))
  }
}
