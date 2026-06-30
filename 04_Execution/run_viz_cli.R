#!/usr/bin/env Rscript

script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]))
source(file.path(script_dir, "..", "02_Core", "bootstrap.R"))
source(file.path(script_dir, "run_viz.R"))
init_res()
config <- load_config()
run_all_visualizations(config)
message("Visualization complete.")
