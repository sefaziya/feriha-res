# ENVFIT — feriha-sosyal.R satir 741-742

run_envfit <- function(nmds_result, df, reduced_vars, config) {
  if (!requireNamespace("vegan", quietly = TRUE)) {
    stop("Package 'vegan' is required.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }

  set.seed(config$analysis$seed %||% 123)
  permutations <- config$analysis$permutations %||% 999
  mat <- df |> dplyr::select(dplyr::all_of(reduced_vars))
  vegan::envfit(nmds_result, mat, permutations = permutations)
}
