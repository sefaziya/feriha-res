# Preprocessing — feriha-sosyal.R metodolojisi, tum parametreler config'den
# Hardcoded alan/filtre YOK — title_filter, num_vars, drop_vars yalnizca config.yml

winsorize_numeric <- function(x, prob = 0.99) {
  cutoff <- stats::quantile(x, prob, na.rm = TRUE)
  if (cutoff > 0) x[x > cutoff] <- cutoff
  return(x)
}

get_production_vars <- function(config) {
  num_vars <- config$data$num_vars
  drop_vars <- config$data$drop_vars %||% character()
  if (is.null(num_vars) || length(num_vars) == 0) {
    stop("config$data$num_vars tanimli olmali.")
  }
  reduced_vars <- setdiff(num_vars, drop_vars)
  list(num_vars = num_vars, drop_vars = drop_vars, reduced_vars = reduced_vars)
}

get_grouping_variable <- function(config) {
  gv <- config$fields$grouping_variable
  if (is.null(gv) || !nzchar(gv)) {
    stop("config$fields$grouping_variable tanimli olmali.")
  }
  gv
}

preprocess_field_data <- function(df, config) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }

  grouping_var <- get_grouping_variable(config)
  vars <- get_production_vars(config)
  drop_vars <- vars$drop_vars
  reduced_vars <- vars$reduced_vars
  n_raw <- nrow(df)

  df_out <- df

  title_filter <- config$data$title_filter
  if (!is.null(title_filter) && length(title_filter) > 0) {
    df_out <- df_out[as.character(df_out$title) %in% title_filter, , drop = FALSE]
  }

  min_row_sum <- config$data$min_row_sum
  if (!is.null(min_row_sum) && is.numeric(min_row_sum) && min_row_sum > 0) {
    row_sums <- rowSums(df_out[, reduced_vars, drop = FALSE], na.rm = TRUE)
    df_out <- df_out[row_sums >= min_row_sum, , drop = FALSE]
  }

  log_info("Orijinal veri seti boyutu: ", n_raw)
  log_info("Filtre sonrasi aktif akademisyen sayisi: ", nrow(df_out))
  log_info("Cikarilan gurultulu degiskenler: ", paste(drop_vars, collapse = ", "))
  log_info("Analize dahil degiskenler: ", paste(reduced_vars, collapse = ", "))

  winsor_prob <- config$preprocess$winsorize_prob %||% 0.99
  df_out <- df_out |>
    dplyr::mutate(dplyr::across(dplyr::all_of(reduced_vars), ~ winsorize_numeric(.x, prob = winsor_prob)))

  if (isTRUE(config$preprocess$log_transform %||% TRUE)) {
    df_out <- df_out |>
      dplyr::mutate(dplyr::across(dplyr::all_of(reduced_vars), ~ log1p(.x)))
  }

  list(
    df = as.data.frame(df_out),
    reduced_vars = reduced_vars,
    num_vars = vars$num_vars,
    drop_vars = drop_vars,
    grouping_var = grouping_var,
    meta = list(
      n_raw = n_raw,
      n_processed = nrow(df_out),
      reduced_vars = reduced_vars,
      drop_vars = drop_vars
    )
  )
}
