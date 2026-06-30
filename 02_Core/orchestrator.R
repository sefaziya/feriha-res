# Execution orchestrator — sequential / controlled_parallel / parallel

run_fields_sequential <- function(df, plan, config, run_date = NULL) {
  for (field in plan) {
    run_field_pipeline(df, field, config, run_date)
  }
}

run_fields_parallel <- function(df, plan, config, run_date = NULL, workers = 4) {
  if (!requireNamespace("future", quietly = TRUE) || !requireNamespace("furrr", quietly = TRUE)) {
    stop("Packages 'future' and 'furrr' are required for parallel execution.")
  }
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::multisession, workers = workers)
  furrr::future_walk(plan, function(field) {
    run_field_pipeline(df, field, config, run_date)
  })
}

run_execution_plan <- function(df, plan, config, run_date = NULL) {
  if (length(plan) == 0) {
    log_info("No pending fields in execution plan.")
    return(invisible(NULL))
  }

  mode <- config$execution$mode %||% "sequential"
  workers <- config$parallel$workers %||% 4

  switch(mode,
    sequential = run_fields_sequential(df, plan, config, run_date),
    controlled_parallel = run_fields_parallel(df, plan, config, run_date, workers),
    parallel = run_fields_parallel(df, plan, config, run_date, workers),
    stop("Unknown execution mode: ", mode)
  )
}
