# Visualization runner — reads workspace only

run_field_visualizations <- function(field, config, run_date = NULL) {
  if (is.null(run_date)) run_date <- format(Sys.Date(), "%Y%m%d")
  out_dir <- get_output_dir(field, config, run_date)
  ws_path <- file.path(out_dir, "Workspace", "workspace.rds")

  if (!file.exists(ws_path)) {
    stop("Workspace not found for field: ", field, " at ", ws_path)
  }

  workspace <- readRDS(ws_path)
  plots_dir <- file.path(out_dir, "Plots")
  dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

  outputs <- list(
    nmds = plot_nmds(workspace, config, file.path(plots_dir, "nmds.png")),
    envfit = plot_envfit(workspace, config, file.path(plots_dir, "envfit_vectors.png")),
    ridge = plot_ridge(workspace, config, file.path(plots_dir, "ridge.png")),
    heatmap = plot_heatmap(workspace, config, file.path(plots_dir, "profile_heatmap.png")),
    pairwise = plot_pairwise(workspace, config, file.path(plots_dir, "centroid_distance.png")),
    radar = plot_radar(workspace, config, file.path(plots_dir, "radar.png")),
    network = plot_network(workspace, config, file.path(plots_dir, "similarity_network.png")),
    interactive = plot_interactive_nmds(workspace, config, file.path(plots_dir, "interactive_nmds.html"))
  )

  invisible(outputs)
}

run_all_visualizations <- function(config, run_date = NULL) {
  if (is.null(run_date)) run_date <- format(Sys.Date(), "%Y%m%d")
  base <- file.path(project_root(), config$output$base_dir %||% "05_Output")
  field_dirs <- list.dirs(base, recursive = FALSE, full.names = FALSE)
  field_dirs <- field_dirs[field_dirs != "_meta"]

  for (fd in field_dirs) {
    # reverse map sanitized name — use summary if available
    run_dir <- file.path(base, fd, run_date)
    summary_path <- file.path(run_dir, "Metadata", "summary.json")
    if (!file.exists(summary_path)) next

    if (requireNamespace("jsonlite", quietly = TRUE)) {
      summary <- jsonlite::read_json(summary_path, simplifyVector = TRUE)
      field_name <- summary$field
      if (!is.null(field_name)) {
        message("Visualizing: ", field_name)
        tryCatch(run_field_visualizations(field_name, config, run_date), error = function(e) {
          warning("Viz failed for ", field_name, ": ", conditionMessage(e))
        })
      }
    }
  }
}
