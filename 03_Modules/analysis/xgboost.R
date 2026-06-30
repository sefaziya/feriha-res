# XGBoost — feriha-sosyal.R: boost_tree + workflow + vip::vi

run_xgboost <- function(df, grouping_var, reduced_vars, config) {
  if (!requireNamespace("tidymodels", quietly = TRUE)) {
    stop("Package 'tidymodels' is required.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package 'xgboost' is required.")
  }

  set.seed(config$analysis$seed %||% 123)
  xgb_cfg <- config$analysis$xgboost %||% list()

  model_df <- df |> dplyr::select(dplyr::all_of(c(grouping_var, reduced_vars)))

  xgb_spec <- parsnip::boost_tree(
    trees = xgb_cfg$trees %||% 100,
    tree_depth = xgb_cfg$tree_depth %||% 4,
    learn_rate = xgb_cfg$learn_rate %||% 0.1
  ) |>
    parsnip::set_engine("xgboost", importance = xgb_cfg$importance %||% "impurity") |>
    parsnip::set_mode(xgb_cfg$mode %||% "classification")

  xgb_workflow <- workflows::workflow() |>
    workflows::add_recipe(
      recipes::recipe(
        stats::reformulate(".", response = grouping_var),
        data = model_df
      )
    ) |>
    workflows::add_model(xgb_spec)

  xgb_fit <- parsnip::fit(xgb_workflow, data = df)

  importance_data <- xgb_fit |>
    workflows::extract_fit_parsnip() |>
    vip::vi()

  list(fit = xgb_fit, importance = importance_data)
}
