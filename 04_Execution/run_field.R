# Field pipeline — feriha-sosyal.R HIKAYE 1-3

run_field_pipeline <- function(df, field, config, run_date = NULL) {
  if (is.null(run_date)) run_date <- format(Sys.Date(), "%Y%m%d")
  out_dir <- get_output_dir(field, config, run_date)
  ensure_output_dirs(out_dir)

  start_time <- Sys.time()
  init_field_log(field, config, out_dir)
  on.exit(close_field_log(), add = TRUE)

  min_n <- config$fields$min_n %||% 30
  df_f <- filter_field_data(df, field)

  if (nrow(df_f) < min_n) {
    log_warn("Skipping field '", field, "' — insufficient rows (", nrow(df_f), " < ", min_n, ")")
    write_checkpoint(field, "DONE", out_dir, list(skipped = TRUE, reason = "insufficient_n"))
    return(invisible(NULL))
  }

  if (checkpoint_exists(out_dir, field, "DONE")) {
    log_info("Field already complete: ", field)
    return(invisible(NULL))
  }

  start_step <- resume_from_step(out_dir, field)
  if (is.null(start_step)) {
    log_info("Field already complete: ", field)
    return(invisible(NULL))
  }

  log_info("Running pipeline for field='", field, "' from step='", start_step, "'")

  results <- list(field = field, grouping_var = get_grouping_variable(config))

  prep <- preprocess_field_data(df_f, config)
  results$df <- prep$df
  results$reduced_vars <- prep$reduced_vars
  results$num_vars <- prep$num_vars
  results$grouping_var <- prep$grouping_var
  results$preprocess_meta <- prep$meta

  grouping_var <- results$grouping_var
  if (length(unique(results$df[[grouping_var]])) < 2) {
    log_warn("Skipping analyses — fewer than 2 groups in ", grouping_var)
    write_checkpoint(field, "DONE", out_dir, list(skipped = TRUE, reason = "insufficient_groups"))
    return(invisible(NULL))
  }

  if (!checkpoint_exists(out_dir, field, "distance") && should_run_step("distance", start_step)) {
    log_info("--- HIKAYE 1: PERMANOVA & PERMDISP (BRAY-CURTIS) ---")
    log_info("Step: distance")
    results$dist <- compute_distance(results$df, results$reduced_vars, config$analysis$distance %||% "bray")
    saveRDS(results$dist, file.path(out_dir, "Models", "dist.rds"))
    write_checkpoint(field, "distance", out_dir)
  } else if (file.exists(file.path(out_dir, "Models", "dist.rds"))) {
    results$dist <- readRDS(file.path(out_dir, "Models", "dist.rds"))
  } else {
    results$dist <- compute_distance(results$df, results$reduced_vars, config$analysis$distance %||% "bray")
  }

  if (!checkpoint_exists(out_dir, field, "permanova") && should_run_step("permanova", start_step)) {
    log_info("Step: permanova")
    results$permanova <- run_permanova(results$dist, results$df, grouping_var, config)
    print(results$permanova)
    saveRDS(results$permanova, file.path(out_dir, "Models", "permanova.rds"))
    write_checkpoint(field, "permanova", out_dir)
  } else if (file.exists(file.path(out_dir, "Models", "permanova.rds"))) {
    results$permanova <- readRDS(file.path(out_dir, "Models", "permanova.rds"))
  }

  if (!checkpoint_exists(out_dir, field, "permdisp") && should_run_step("permdisp", start_step)) {
    log_info("Step: permdisp")
    results$permdisp <- run_permdisp(results$dist, results$df, grouping_var)
    print(results$permdisp$anova)
    saveRDS(results$permdisp, file.path(out_dir, "Models", "permdisp.rds"))
    write_checkpoint(field, "permdisp", out_dir)
  } else if (file.exists(file.path(out_dir, "Models", "permdisp.rds"))) {
    results$permdisp <- readRDS(file.path(out_dir, "Models", "permdisp.rds"))
  }

  if (!checkpoint_exists(out_dir, field, "nmds") && should_run_step("nmds", start_step)) {
    log_info("--- HIKAYE 2: NMDS HESAPLAMASI ---")
    log_info("Step: nmds")
    results$nmds <- run_nmds(results$df, results$reduced_vars, config)
    log_info("NMDS Stres Degeri: ", round(results$nmds$stress, 4))
    saveRDS(results$nmds, file.path(out_dir, "Models", "nmds.rds"))
    write_checkpoint(field, "nmds", out_dir)
  } else if (file.exists(file.path(out_dir, "Models", "nmds.rds"))) {
    results$nmds <- readRDS(file.path(out_dir, "Models", "nmds.rds"))
  }

  if (!checkpoint_exists(out_dir, field, "envfit") && should_run_step("envfit", start_step)) {
    log_info("--- ENVFIT HESAPLAMASI (VEKTOR OKLARI) ---")
    log_info("Step: envfit")
    results$envfit <- run_envfit(results$nmds, results$df, results$reduced_vars, config)
    print(results$envfit)
    saveRDS(results$envfit, file.path(out_dir, "Models", "envfit.rds"))
    write_checkpoint(field, "envfit", out_dir)
  } else if (file.exists(file.path(out_dir, "Models", "envfit.rds"))) {
    results$envfit <- readRDS(file.path(out_dir, "Models", "envfit.rds"))
  }

  if (!checkpoint_exists(out_dir, field, "xgboost") && should_run_step("xgboost", start_step)) {
    log_info("--- HIKAYE 3: XGBOOST DEGISKEN ONEMI ---")
    log_info("Step: xgboost")
    results$xgb <- run_xgboost(results$df, grouping_var, results$reduced_vars, config)
    if (!is.null(results$xgb$importance)) print(results$xgb$importance)
    saveRDS(results$xgb, file.path(out_dir, "Models", "xgboost.rds"))
    write_checkpoint(field, "xgboost", out_dir)
  } else if (file.exists(file.path(out_dir, "Models", "xgboost.rds"))) {
    results$xgb <- readRDS(file.path(out_dir, "Models", "xgboost.rds"))
  }

  if (!checkpoint_exists(out_dir, field, "save")) {
    log_info("Step: save")
    save_workspace(out_dir, results)
    save_model_artifacts(out_dir, results)
    save_tables(out_dir, results)
    write_session_info(out_dir, config)

    summary <- build_field_summary(field, results, start_time, config)
    write_summary_json(out_dir, summary)

    write_checkpoint(field, "preprocess", out_dir)
    write_checkpoint(field, "save", out_dir)
    write_checkpoint(field, "DONE", out_dir)
    log_info("Analiz basariyla tamamlandi ve yedeklendi.")
    log_info("Field complete: ", field)
  }

  invisible(results)
}
