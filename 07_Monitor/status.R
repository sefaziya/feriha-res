# RES Monitor — filesystem + process status

PIPELINE_STEP_LABELS <- c(
  preprocess = "On isleme",
  distance = "Mesafe matrisi",
  permanova = "PERMANOVA",
  permdisp = "PERMDISP",
  nmds = "NMDS",
  envfit = "ENVFIT",
  xgboost = "XGBoost",
  save = "Kayit",
  DONE = "Tamamlandi"
)

load_monitor_config <- function(path = NULL) {
  if (is.null(path)) {
    path <- file.path(project_root(), "07_Monitor", "monitor_config.yml")
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required.")
  }
  yaml::read_yaml(path)
}

output_base_dir <- function(config) {
  file.path(project_root(), config$output$base_dir %||% "05_Output")
}

list_run_dates <- function(config) {
  base <- output_base_dir(config)
  if (!dir.exists(base)) return(character())
  dates <- unique(unlist(lapply(list.dirs(base, recursive = TRUE, full.names = FALSE), function(p) {
    parts <- strsplit(p, .Platform$file.sep)[[1]]
    parts[nchar(parts) == 8 & grepl("^\\d{8}$", parts)]
  })))
  sort(dates, decreasing = TRUE)
}

read_execution_manifest <- function(config, run_date) {
  path <- file.path(output_base_dir(config), "_meta", run_date, "execution_manifest.json")
  if (!file.exists(path)) return(NULL)
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
  jsonlite::read_json(path, simplifyVector = TRUE)
}

field_output_dir <- function(field, config, run_date) {
  file.path(output_base_dir(config), sanitize_field_name(field), run_date)
}

read_checkpoint_field_name <- function(out_dir) {
  ckpt_dir <- file.path(out_dir, "Checkpoints")
  if (!dir.exists(ckpt_dir)) return(NA_character_)
  files <- list.files(ckpt_dir, pattern = "^step_.*_done\\.rds$", full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  info <- tryCatch(readRDS(files[[1]]), error = function(e) NULL)
  if (is.null(info) || is.null(info$field)) return(NA_character_)
  as.character(info$field)
}

get_field_step_progress <- function(out_dir) {
  if (!dir.exists(out_dir)) {
    return(list(status = "queued", completed = character(), next_step = "preprocess", pct = 0))
  }

  if (checkpoint_exists(out_dir, "", "DONE")) {
    return(list(status = "complete", completed = PIPELINE_STEPS, next_step = NULL, pct = 100))
  }

  completed <- character()
  for (step in PIPELINE_STEPS) {
    if (checkpoint_exists(out_dir, "", step)) {
      completed <- c(completed, step)
    }
  }

  if (length(completed) == 0) {
    return(list(status = "pending", completed = character(), next_step = "preprocess", pct = 0))
  }

  last <- completed[length(completed)]
  if (last == "DONE") {
    return(list(status = "complete", completed = completed, next_step = NULL, pct = 100))
  }

  idx <- match(last, PIPELINE_STEPS)
  next_step <- if (!is.na(idx) && idx < length(PIPELINE_STEPS)) PIPELINE_STEPS[[idx + 1]] else NULL
  pct <- round(100 * length(completed) / length(PIPELINE_STEPS))

  list(
    status = if (is.null(next_step)) "complete" else "running",
    completed = completed,
    next_step = next_step,
    pct = pct
  )
}

read_field_summary <- function(out_dir) {
  path <- file.path(out_dir, "Metadata", "summary.json")
  if (!file.exists(path)) return(NULL)
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
  tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) NULL)
}

collect_field_rows <- function(config, run_date) {
  manifest <- read_execution_manifest(config, run_date)
  fields <- if (!is.null(manifest$academic_fields)) manifest$academic_fields else character()

  if (length(fields) == 0) {
    base <- output_base_dir(config)
    field_dirs <- list.dirs(base, recursive = FALSE, full.names = FALSE)
    field_dirs <- setdiff(field_dirs, c("_meta", "_logs"))
    for (fd in field_dirs) {
      out <- file.path(base, fd, run_date)
      if (!dir.exists(out)) next
      nm <- read_checkpoint_field_name(out)
      fields <- c(fields, if (is.na(nm)) fd else nm)
    }
    fields <- unique(fields)
  }

  lapply(fields, function(field) {
    out_dir <- field_output_dir(field, config, run_date)
    prog <- get_field_step_progress(out_dir)
    summary <- read_field_summary(out_dir)
    last_ckpt_time <- NA_character_
    ckpt_dir <- file.path(out_dir, "Checkpoints")
    if (dir.exists(ckpt_dir)) {
      files <- list.files(ckpt_dir, pattern = "_done\\.rds$", full.names = TRUE)
      if (length(files) > 0) {
        last_ckpt_time <- format(file.mtime(files[[length(files)]]), "%Y-%m-%d %H:%M:%S")
      }
    }

    list(
      field = field,
      status = prog$status,
      completed_steps = paste(prog$completed, collapse = ", "),
      next_step = prog$next_step %||% "—",
      progress_pct = prog$pct,
      N = summary$N %||% NA,
      PERMANOVA_R2 = summary$PERMANOVA_R2 %||% NA,
      Stress = summary$Stress %||% NA,
      Runtime = summary$Runtime %||% "—",
      last_update = last_ckpt_time %||% "—"
    )
  })
}

is_engine_running <- function() {
  out <- tryCatch(
    system2("pgrep", "-f", "02_Core/run_engine.R", stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  length(out) > 0
}

tmux_session_active <- function(name) {
  status <- system2("tmux", c("has-session", "-t", name), stdout = FALSE, stderr = FALSE)
  identical(status, 0L)
}

list_tmux_sessions <- function() {
  out <- tryCatch(
    system2("tmux", "list-sessions", stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  if (length(out) == 0) return(character())
  sub(":.*", "", out)
}

tail_aggregate_log <- function(config, n = 80) {
  path <- file.path(output_base_dir(config), config$logging$aggregate_log %||% "engine_run.log")
  if (!file.exists(path)) return("(log dosyasi yok)")
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  tail(lines, n)
}

parse_log_activity <- function(config) {
  lines <- tail_aggregate_log(config, 30)
  if (length(lines) == 0) return(list(step = "—", field = "—", run_id = "—"))
  step_line <- grep("Step:", lines, value = TRUE)
  field_line <- grep("Log started for field:", lines, value = TRUE)
  run_line <- grep("Run ID:", lines, value = TRUE)
  list(
    step = if (length(step_line)) sub(".*Step: ", "", tail(step_line, 1)) else "—",
    field = if (length(field_line)) trimws(sub(".*Log started for field: ", "", tail(field_line, 1))) else "—",
    run_id = if (length(run_line)) trimws(sub(".*Run ID: ", "", tail(run_line, 1))) else "—"
  )
}

count_data_files <- function(config) {
  data_dir <- file.path(project_root(), config$data$data_dir %||% "00_Data")
  if (!dir.exists(data_dir)) return(0L)
  length(list.files(data_dir, pattern = "\\.xlsx$", ignore.case = TRUE))
}

collect_dashboard_status <- function(config, run_date, monitor_cfg = NULL) {
  if (is.null(monitor_cfg)) monitor_cfg <- list()
  tmux_name <- monitor_cfg$tmux_session %||% "RES"
  fields <- collect_field_rows(config, run_date)
  n_complete <- sum(vapply(fields, function(x) x$status == "complete", logical(1)))
  n_total <- length(fields)
  activity <- parse_log_activity(config)

  list(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    run_date = run_date,
    engine_running = is_engine_running(),
    tmux_res = tmux_session_active(tmux_name),
    tmux_sessions = list_tmux_sessions(),
    fields_complete = n_complete,
    fields_total = n_total,
    current_field = activity$field,
    current_step = activity$step,
    run_id = activity$run_id,
    data_files = count_data_files(config),
    fields = fields
  )
}

start_engine_tmux <- function(monitor_cfg = list()) {
  root <- project_root()
  session <- monitor_cfg$tmux_session %||% "RES"
  if (tmux_session_active(session)) {
    return(list(ok = FALSE, message = paste0("tmux oturumu '", session, "' zaten var.")))
  }
  if (is_engine_running()) {
    return(list(ok = FALSE, message = "Engine process zaten calisiyor."))
  }
  cmd <- sprintf("cd %s && ./run_res.sh", shQuote(root))
  status <- system2("tmux", c("new-session", "-d", "-s", session, cmd), stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L)) {
    return(list(ok = FALSE, message = "tmux oturumu baslatilamadi."))
  }
  list(ok = TRUE, message = paste0("Engine tmux '", session, "' icinde baslatildi."))
}
