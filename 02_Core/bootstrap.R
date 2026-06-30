# RES bootstrap — load all modules from project root

get_project_root <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(file.path(dirname(sub("^--file=", "", file_arg[1])), "..")))
  }
  normalizePath(getwd())
}

init_res <- function(project_root = NULL) {
  if (is.null(project_root)) {
    project_root <- get_project_root()
  }
  options(res.project_root = project_root)
  old_wd <- getwd()
  setwd(project_root)
  on.exit(setwd(old_wd), add = TRUE)

  module_files <- c(
    "03_Modules/io.R",
    "03_Modules/discovery.R",
    "03_Modules/preprocess.R",
    "03_Modules/logging.R",
    "03_Modules/checkpoint.R",
    "03_Modules/save.R",
    "03_Modules/analysis/distance.R",
    "03_Modules/analysis/permanova.R",
    "03_Modules/analysis/permdisp.R",
    "03_Modules/analysis/nmds.R",
    "03_Modules/analysis/envfit.R",
    "03_Modules/analysis/xgboost.R",
    "03_Modules/visualization/palettes.R",
    "03_Modules/visualization/nmds_plot.R",
    "03_Modules/visualization/envfit_plot.R",
    "03_Modules/visualization/ridge.R",
    "03_Modules/visualization/heatmap.R",
    "03_Modules/visualization/pairwise.R",
    "03_Modules/visualization/radar.R",
    "03_Modules/visualization/network.R",
    "03_Modules/visualization/interactive_nmds.R",
    "04_Execution/run_field.R",
    "02_Core/orchestrator.R",
    "06_Meta/summary_builder.R",
    "06_Meta/meta_analysis.R"
  )

  for (f in module_files) {
    path <- file.path(project_root, f)
    if (!file.exists(path)) {
      stop("Missing module: ", path)
    }
    source(path, local = FALSE)
  }

  invisible(project_root)
}

project_root <- function() {
  root <- getOption("res.project_root")
  if (is.null(root)) {
    root <- init_res()
  }
  root
}
