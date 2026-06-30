# Save workspace and structured outputs

ensure_output_dirs <- function(out_dir) {
  dirs <- c(
    "Workspace", "Models", "Tables", "Metadata",
    "Logs", "Checkpoints", "Plots"
  )
  for (d in dirs) {
    dir.create(file.path(out_dir, d), recursive = TRUE, showWarnings = FALSE)
  }
}

save_workspace <- function(out_dir, workspace) {
  ensure_output_dirs(out_dir)
  path <- file.path(out_dir, "Workspace", "workspace.rds")
  saveRDS(workspace, path)
  invisible(path)
}

save_model_artifacts <- function(out_dir, results) {
  ensure_output_dirs(out_dir)
  models_dir <- file.path(out_dir, "Models")

  if (!is.null(results$permanova)) {
    saveRDS(results$permanova, file.path(models_dir, "permanova.rds"))
  }
  if (!is.null(results$permdisp)) {
    saveRDS(results$permdisp, file.path(models_dir, "permdisp.rds"))
  }
  if (!is.null(results$nmds)) {
    saveRDS(results$nmds, file.path(models_dir, "nmds.rds"))
  }
  if (!is.null(results$envfit)) {
    saveRDS(results$envfit, file.path(models_dir, "envfit.rds"))
  }
  if (!is.null(results$xgb)) {
    saveRDS(results$xgb, file.path(models_dir, "xgboost.rds"))
  }
}

save_tables <- function(out_dir, results) {
  ensure_output_dirs(out_dir)
  tables_dir <- file.path(out_dir, "Tables")

  if (!is.null(results$permanova)) {
    perm_df <- as.data.frame(results$permanova)
    perm_df$term <- rownames(perm_df)
    utils::write.csv(perm_df, file.path(tables_dir, "permanova.csv"), row.names = FALSE)
  }

  if (!is.null(results$xgb$importance)) {
    utils::write.csv(results$xgb$importance, file.path(tables_dir, "importance.csv"), row.names = FALSE)
  }
}

write_session_info <- function(out_dir, config) {
  if (!isTRUE(config$output$include_session_info %||% TRUE)) return(invisible(NULL))
  ensure_output_dirs(out_dir)
  path <- file.path(out_dir, "Metadata", "sessionInfo.txt")
  writeLines(capture.output(sessionInfo()), path)
  invisible(path)
}

format_runtime <- function(start_time) {
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  sprintf("%02d:%02d:%02d",
          as.integer(elapsed %/% 3600),
          as.integer((elapsed %% 3600) %/% 60),
          as.integer(elapsed %% 60))
}

write_summary_json <- function(out_dir, summary) {
  ensure_output_dirs(out_dir)
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required.")
  }
  path <- file.path(out_dir, "Metadata", "summary.json")
  jsonlite::write_json(summary, path, pretty = TRUE, auto_unbox = TRUE)
  invisible(path)
}
