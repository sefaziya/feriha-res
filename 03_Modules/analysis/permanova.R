# PERMANOVA — feriha-sosyal.R: adonis2(dist ~ grouping_var, permutations=999)

run_permanova <- function(dist_matrix, df, grouping_var, config) {
  if (!requireNamespace("vegan", quietly = TRUE)) {
    stop("Package 'vegan' is required.")
  }
  set.seed(config$analysis$seed %||% 123)
  permutations <- config$analysis$permutations %||% 999
  formula <- stats::as.formula(paste("dist_matrix ~", grouping_var))
  vegan::adonis2(formula, data = df, permutations = permutations)
}
