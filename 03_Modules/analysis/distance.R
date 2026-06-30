# Distance matrix — feriha-sosyal.R satir 126

compute_distance <- function(df, reduced_vars, method = "bray") {
  if (!requireNamespace("vegan", quietly = TRUE)) {
    stop("Package 'vegan' is required.")
  }
  mat <- df[, reduced_vars, drop = FALSE]
  vegan::vegdist(mat, method = method)
}
