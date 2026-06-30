# PERMDISP — feriha-sosyal.R: betadisper + anova

run_permdisp <- function(dist_matrix, df, grouping_var) {
  if (!requireNamespace("vegan", quietly = TRUE)) {
    stop("Package 'vegan' is required.")
  }
  dispersion_results <- vegan::betadisper(dist_matrix, df[[grouping_var]])
  list(
    dispersion = dispersion_results,
    anova = stats::anova(dispersion_results)
  )
}
