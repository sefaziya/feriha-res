#!/usr/bin/env Rscript

script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]))
source(file.path(script_dir, "bootstrap.R"))

init_res()
config <- load_config()
run_date <- format(Sys.Date(), "%Y%m%d")

get_res_run_id()
init_engine_log(config)
on.exit(close_engine_log(), add = TRUE)

log_start(config)
df <- load_all_excels(config = config)
fields <- discover_fields(df)
plan <- build_execution_plan(fields$academic_fields, config, run_date)

log_info("Discovered academic fields: ", length(fields$academic_fields))
log_info("Pending execution plan: ", length(plan))
write_execution_manifest(fields$academic_fields, plan, config, run_date)

run_execution_plan(df, plan, config, run_date)
build_global_summary(config, run_date)

log_info("RES engine finished.")
