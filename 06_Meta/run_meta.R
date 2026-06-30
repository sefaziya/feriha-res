#!/usr/bin/env Rscript

source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])), "..", "02_Core", "bootstrap.R"))
init_res()
config <- load_config()
run_meta_analysis(config)
message("Meta analysis complete.")
