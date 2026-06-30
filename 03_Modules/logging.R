# Logging utilities
# - Her calistirma: ayri timestamp'li dosya (_logs/engine_*.log, Logs/{field}_*.log)
# - Ayrica: 05_Output/engine_run.log (append, onceki kayitlar korunur)

LOG_LEVELS <- c(ERROR = 1L, WARN = 2L, INFO = 3L, DEBUG = 4L)

.res_log_state <- new.env(parent = emptyenv())

log_level_value <- function(level) {
  LOG_LEVELS[[toupper(level)]] %||% LOG_LEVELS[["INFO"]]
}

res_run_timestamp <- function() {
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

get_res_run_id <- function() {
  rid <- getOption("res.run_id")
  if (is.null(rid) || !nzchar(rid)) {
    rid <- res_run_timestamp()
    options(res.run_id = rid)
  }
  rid
}

get_aggregate_log_path <- function(config) {
  base <- config$output$base_dir %||% "05_Output"
  rel <- config$logging$aggregate_log %||% "engine_run.log"
  file.path(project_root(), base, rel)
}

log_file_name <- function(prefix, run_id = NULL) {
  if (is.null(run_id)) run_id <- get_res_run_id()
  paste0(sanitize_field_name(prefix), "_", run_id, ".log")
}

open_log_file <- function(path, append = FALSE) {
  con <- file(path, open = if (append) "at" else "wt", encoding = "UTF-8")
  if (!append) {
    writeLines(paste("=== Log started:", as.character(Sys.time()), "==="), con)
    flush(con)
  }
  con
}

flush_log_connections <- function() {
  for (nm in c("engine_con", "aggregate_con", "con")) {
    con <- .res_log_state[[nm]]
    if (!is.null(con) && isOpen(con)) flush(con)
  }
  invisible(NULL)
}

live_status_path <- function(config, run_date = NULL) {
  if (is.null(run_date)) run_date <- .res_log_state$run_date %||% format(Sys.Date(), "%Y%m%d")
  base <- config$output$base_dir %||% "05_Output"
  file.path(project_root(), base, "_meta", run_date, "live_status.json")
}

update_live_status <- function(field = NULL, step = NULL, extra = list()) {
  config <- .res_log_state$config
  if (is.null(config)) return(invisible(NULL))
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(invisible(NULL))

  run_date <- .res_log_state$run_date %||% format(Sys.Date(), "%Y%m%d")
  if (!is.null(field) && nzchar(field)) .res_log_state$current_field <- field
  if (!is.null(step) && nzchar(step)) .res_log_state$current_step <- step

  status <- c(
    list(
      updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      run_id = get_res_run_id(),
      pid = Sys.getpid(),
      current_field = .res_log_state$current_field %||% NA_character_,
      current_step = .res_log_state$current_step %||% NA_character_,
      engine_running = TRUE
    ),
    extra
  )

  path <- live_status_path(config, run_date)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  jsonlite::write_json(status, tmp, pretty = TRUE, auto_unbox = TRUE, na = "null")
  file.rename(tmp, path)
  invisible(path)
}

notify_checkpoint <- function(field, step, out_dir) {
  update_live_status(field = field, step = step)
}

mark_engine_stopped <- function(config, run_date = NULL) {
  if (is.null(config)) config <- .res_log_state$config
  if (is.null(config)) return(invisible(NULL))
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(invisible(NULL))
  if (is.null(run_date)) run_date <- .res_log_state$run_date %||% format(Sys.Date(), "%Y%m%d")

  path <- live_status_path(config, run_date)
  prev <- if (file.exists(path)) {
    tryCatch(jsonlite::read_json(path), error = function(e) list())
  } else {
    list()
  }

  status <- c(
    prev,
    list(
      updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      engine_running = FALSE,
      pid = NA
    )
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(status, path, pretty = TRUE, auto_unbox = TRUE, na = "null")
  invisible(path)
}

write_aggregate_run_header <- function(con, run_id) {
  writeLines(c(
    "",
    paste(rep("=", 72), collapse = ""),
    paste("RES RUN START |", as.character(Sys.time()), "| Run ID:", run_id),
    paste(rep("=", 72), collapse = ""),
    ""
  ), con)
}

write_aggregate_run_footer <- function(con, run_id) {
  writeLines(c(
    "",
    paste(rep("-", 72), collapse = ""),
    paste("RES RUN END   |", as.character(Sys.time()), "| Run ID:", run_id),
    paste(rep("-", 72), collapse = ""),
    ""
  ), con)
}

write_log_lines <- function(msg) {
  cat(msg, "\n")
  flush.console()
  if (!is.null(.res_log_state$engine_con)) {
    writeLines(msg, .res_log_state$engine_con)
  }
  if (!is.null(.res_log_state$aggregate_con)) {
    writeLines(msg, .res_log_state$aggregate_con)
  }
  if (!is.null(.res_log_state$con)) {
    writeLines(msg, .res_log_state$con)
  }
  flush_log_connections()
}

init_engine_log <- function(config, run_date = NULL) {
  if (is.null(run_date)) run_date <- format(Sys.Date(), "%Y%m%d")
  .res_log_state$config <- config
  .res_log_state$run_date <- run_date
  .res_log_state$current_field <- NA_character_
  .res_log_state$current_step <- NA_character_

  run_id <- get_res_run_id()
  base <- config$output$base_dir %||% "05_Output"
  log_dir <- file.path(project_root(), base, "_logs")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

  log_path <- file.path(log_dir, log_file_name("engine", run_id))
  .res_log_state$engine_con <- open_log_file(log_path, append = FALSE)
  .res_log_state$engine_path <- log_path
  .res_log_state$level <- log_level_value(config$logging$level %||% "INFO")

  if (isTRUE(config$logging$aggregate_log_enabled %||% TRUE)) {
    aggregate_path <- get_aggregate_log_path(config)
    dir.create(dirname(aggregate_path), recursive = TRUE, showWarnings = FALSE)
    .res_log_state$aggregate_con <- open_log_file(aggregate_path, append = TRUE)
    .res_log_state$aggregate_path <- aggregate_path
    write_aggregate_run_header(.res_log_state$aggregate_con, run_id)
  }

  log_info("Engine log file: ", log_path)
  if (!is.null(.res_log_state$aggregate_path)) {
    log_info("Aggregate log file: ", .res_log_state$aggregate_path)
  }
  update_live_status(extra = list(event = "engine_start"))
  invisible(log_path)
}

close_engine_log <- function() {
  run_id <- get_res_run_id()
  config <- .res_log_state$config
  run_date <- .res_log_state$run_date

  if (!is.null(.res_log_state$engine_con)) {
    writeLines(paste("=== Log ended:", as.character(Sys.time()), "==="), .res_log_state$engine_con)
    flush(.res_log_state$engine_con)
    close(.res_log_state$engine_con)
    .res_log_state$engine_con <- NULL
  }

  if (!is.null(.res_log_state$aggregate_con)) {
    write_aggregate_run_footer(.res_log_state$aggregate_con, run_id)
    flush(.res_log_state$aggregate_con)
    close(.res_log_state$aggregate_con)
    .res_log_state$aggregate_con <- NULL
  }

  mark_engine_stopped(config, run_date)
}

init_field_log <- function(field, config, out_dir) {
  run_id <- get_res_run_id()
  log_dir <- file.path(out_dir, "Logs")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

  log_path <- file.path(log_dir, log_file_name(sanitize_field_name(field), run_id))
  .res_log_state$con <- open_log_file(log_path, append = FALSE)
  .res_log_state$path <- log_path
  .res_log_state$level <- log_level_value(config$logging$level %||% "INFO")

  log_info("Field log file: ", log_path)
  log_info("Log started for field: ", field)
  invisible(log_path)
}

close_field_log <- function() {
  if (!is.null(.res_log_state$con)) {
    writeLines(paste("=== Log ended:", as.character(Sys.time()), "==="), .res_log_state$con)
    flush(.res_log_state$con)
    close(.res_log_state$con)
    .res_log_state$con <- NULL
  }
}

log_message <- function(level, ...) {
  threshold <- .res_log_state$level %||% LOG_LEVELS[["INFO"]]
  if (log_level_value(level) > threshold) return(invisible(NULL))
  stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- paste0("[", stamp, "][", toupper(level), "] ", paste(..., collapse = ""))
  write_log_lines(msg)

  if (grepl("Log started for field:", msg, fixed = TRUE)) {
    field <- trimws(sub(".*Log started for field: ", "", msg))
    update_live_status(field = field)
  } else if (grepl("Step:", msg, fixed = TRUE)) {
    step <- trimws(sub(".*Step: ", "", msg))
    update_live_status(step = step)
  }

  invisible(msg)
}

log_info <- function(...) log_message("INFO", ...)
log_warn <- function(...) log_message("WARN", ...)
log_error <- function(...) log_message("ERROR", ...)
log_debug <- function(...) log_message("DEBUG", ...)

log_start <- function(config) {
  log_info("RES engine start | mode=", config$execution$mode %||% "sequential")
  log_info("Run ID: ", get_res_run_id())
}
