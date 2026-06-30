# NMDS — feriha-sosyal.R satir 144-147

run_nmds <- function(df, reduced_vars, config) {
  if (!requireNamespace("vegan", quietly = TRUE)) {
    stop("Package 'vegan' is required.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }

  set.seed(config$analysis$seed %||% 123)
  mat <- df |> dplyr::select(dplyr::all_of(reduced_vars))

  vegan::metaMDS(
    mat,
    distance = config$analysis$distance %||% "bray",
    k = config$analysis$nmds_k %||% 3,
    trymax = config$analysis$trymax %||% 30,
    trace = FALSE
  )
}
