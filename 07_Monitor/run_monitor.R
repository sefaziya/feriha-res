#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
proj_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg[1])), ".."))
} else {
  normalizePath(getwd())
}

options(res.project_root = proj_root)
source(file.path(proj_root, "03_Modules", "io.R"))
source(file.path(proj_root, "07_Monitor", "status.R"))
source(file.path(proj_root, "07_Monitor", "app.R"))

monitor_cfg <- load_monitor_config()
host <- monitor_cfg$host %||% "127.0.0.1"
port <- monitor_cfg$port %||% 8788L

cat(sprintf("RES Monitor: http://%s:%d\n", host, port))
app <- run_monitor_app(proj_root, monitor_cfg = monitor_cfg)
shiny::runApp(app, host = host, port = port, launch.browser = FALSE)
