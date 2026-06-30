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

  merge_cfg <- function(base, overlay) {
    if (is.null(overlay)) return(base)
    for (nm in names(overlay)) {
      if (is.list(overlay[[nm]]) && is.list(base[[nm]])) {
        base[[nm]] <- merge_cfg(base[[nm]], overlay[[nm]])
      } else {
        base[[nm]] <- overlay[[nm]]
      }
    }
    base
  }

  cfg <- yaml::read_yaml(path)
  local_path <- file.path(dirname(path), "monitor_config.local.yml")
  if (file.exists(local_path)) {
    cfg <- merge_cfg(cfg, yaml::read_yaml(local_path))
  }

  if (nzchar(Sys.getenv("RES_MONITOR_HOST", unset = ""))) {
    cfg$host <- Sys.getenv("RES_MONITOR_HOST")
  }
  if (nzchar(Sys.getenv("RES_MONITOR_PORT", unset = ""))) {
    cfg$port <- as.integer(Sys.getenv("RES_MONITOR_PORT"))
  }
  if (nzchar(Sys.getenv("RES_MONITOR_USER", unset = ""))) {
    if (is.null(cfg$auth)) cfg$auth <- list()
    cfg$auth$username <- Sys.getenv("RES_MONITOR_USER")
  }
  if (nzchar(Sys.getenv("RES_MONITOR_PASSWORD", unset = ""))) {
    if (is.null(cfg$auth)) cfg$auth <- list()
    cfg$auth$password <- Sys.getenv("RES_MONITOR_PASSWORD")
  }

  cfg
}

output_base_dir <- function(config) {
  file.path(project_root(), config$output$base_dir %||% "05_Output")
}

live_status_file <- function(config, run_date) {
  file.path(output_base_dir(config), "_meta", run_date, "live_status.json")
}

read_live_status <- function(config, run_date) {
  path <- live_status_file(config, run_date)
  if (!file.exists(path)) return(NULL)
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
  tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) NULL)
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

get_field_step_progress <- function(out_dir, is_active = FALSE) {
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
    st <- if (is_active) "running" else "pending"
    return(list(status = st, completed = character(), next_step = "preprocess", pct = 0))
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

get_engine_process_info <- function() {
  pids <- tryCatch(
    system2("pgrep", c("-f", "[r]un_engine.R"), stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  pids <- pids[nzchar(pids)]

  for (pid in pids) {
    cmd <- tryCatch(
      system2("ps", c("-p", pid, "-o", "command="), stdout = TRUE, stderr = FALSE),
      error = function(e) character()
    )
    cmd <- paste(cmd, collapse = " ")
    if (!grepl("run_engine\\.R", cmd)) next
    if (!grepl("(/R |Rscript|/exec/R)", cmd)) next

    elapsed <- trimws(paste(tryCatch(
      system2("ps", c("-p", pid, "-o", "etime="), stdout = TRUE, stderr = FALSE),
      error = function(e) "—"
    ), collapse = " "))
    cpu <- trimws(paste(tryCatch(
      system2("ps", c("-p", pid, "-o", "%cpu="), stdout = TRUE, stderr = FALSE),
      error = function(e) "—"
    ), collapse = " "))
    mem <- trimws(paste(tryCatch(
      system2("ps", c("-p", pid, "-o", "%mem="), stdout = TRUE, stderr = FALSE),
      error = function(e) "—"
    ), collapse = " "))
    return(list(
      running = TRUE,
      pid = as.integer(pid),
      elapsed = if (nzchar(elapsed)) elapsed else "—",
      cpu = if (nzchar(cpu)) cpu else "—",
      mem = if (nzchar(mem)) mem else "—"
    ))
  }

  list(running = FALSE, pid = NA_integer_, elapsed = "—", cpu = "—", mem = "—")
}

is_engine_running <- function() {
  get_engine_process_info()$running
}

tmux_session_active <- function(name) {
  status <- system2("tmux", c("has-session", "-t", name), stdout = FALSE, stderr = FALSE)
  identical(status, 0L)
}

capture_tmux_pane <- function(session, n = 40) {
  if (!tmux_session_active(session)) return(character())
  out <- tryCatch(
    system2("tmux", c("capture-pane", "-t", session, "-p"), stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  if (length(out) == 0) return(character())
  tail(out, n)
}

list_tmux_sessions <- function() {
  out <- tryCatch(
    system2("tmux", "list-sessions", stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  if (length(out) == 0) return(character())
  sub(":.*", "", out)
}

latest_engine_log_path <- function(config) {
  log_dir <- file.path(output_base_dir(config), "_logs")
  if (!dir.exists(log_dir)) return(NA_character_)
  files <- list.files(log_dir, pattern = "^engine_.*\\.log$", full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  files[which.max(file.mtime(files))]
}

read_log_tail <- function(path, n = 100) {
  if (is.na(path) || !file.exists(path) || file.info(path)$size == 0) return(character())
  tryCatch({
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    tail(lines, n)
  }, error = function(e) character())
}

tail_aggregate_log <- function(config, n = 80) {
  path <- file.path(output_base_dir(config), config$logging$aggregate_log %||% "engine_run.log")
  read_log_tail(path, n)
}

tail_live_log <- function(config, monitor_cfg = NULL, n = 100) {
  if (is.null(monitor_cfg)) monitor_cfg <- list()
  session <- monitor_cfg$tmux_session %||% "RES"

  run_log <- read_log_tail(latest_engine_log_path(config), n)
  agg <- tail_aggregate_log(config, n)
  tmux_lines <- capture_tmux_pane(session, n)

  pick_nonempty <- function(x) if (length(x) > 0) x else character()
  run_log <- pick_nonempty(run_log)
  agg <- pick_nonempty(agg)
  tmux_lines <- pick_nonempty(tmux_lines)

  if (length(run_log) > 0) {
    return(c(paste0("--- engine log: ", basename(latest_engine_log_path(config)), " ---"), run_log))
  }
  if (length(agg) > 0) {
    return(c("--- aggregate log ---", agg))
  }
  if (length(tmux_lines) > 0) {
    return(c(paste0("--- tmux ", session, " (canli) ---"), tmux_lines))
  }
  "(log yok)"
}

infer_active_field_from_checkpoints <- function(config, run_date) {
  rows <- collect_field_rows(config, run_date)
  running <- Filter(function(x) x$status == "running", rows)
  if (length(running) == 0) return(list(field = "—", step = "—"))

  ord <- order(vapply(running, function(x) x$last_update, character(1)), decreasing = TRUE)
  top <- running[[ord[[1]]]]
  list(
    field = top$field,
    step = if (identical(top$next_step, "—")) "—" else top$next_step
  )
}

parse_log_activity <- function(config, run_date, monitor_cfg = NULL) {
  live <- read_live_status(config, run_date)
  proc <- get_engine_process_info()

  if (!is.null(live)) {
    field <- live$current_field %||% "—"
    step <- live$current_step %||% "—"
    if (identical(field, NA) || is.na(field)) field <- "—"
    if (identical(step, NA) || is.na(step)) step <- "—"
    return(list(
      step = as.character(step),
      field = as.character(field),
      run_id = as.character(live$run_id %||% "—"),
      source = "live_status",
      live_updated = as.character(live$updated_at %||% "—")
    ))
  }

  if (proc$running) {
    inferred <- infer_active_field_from_checkpoints(config, run_date)
    if (!identical(inferred$field, "—")) {
      return(list(
        step = inferred$step,
        field = inferred$field,
        run_id = "—",
        source = "checkpoints",
        live_updated = "—"
      ))
    }
  }

  lines <- tail_live_log(config, monitor_cfg, 40)
  if (length(lines) == 1 && identical(lines, "(log yok)")) {
    return(list(step = "—", field = "—", run_id = "—", source = "none", live_updated = "—"))
  }

  step_line <- grep("Step:", lines, value = TRUE)
  field_line <- grep("Log started for field:", lines, value = TRUE)
  run_line <- grep("Run ID:", lines, value = TRUE)
  list(
    step = if (length(step_line)) trimws(sub(".*Step: ", "", tail(step_line, 1))) else "—",
    field = if (length(field_line)) trimws(sub(".*Log started for field: ", "", tail(field_line, 1))) else "—",
    run_id = if (length(run_line)) trimws(sub(".*Run ID: ", "", tail(run_line, 1))) else "—",
    source = "log",
    live_updated = "—"
  )
}

count_data_files <- function(config) {
  data_dir <- file.path(project_root(), config$data$data_dir %||% "00_Data")
  if (!dir.exists(data_dir)) return(0L)
  length(list.files(data_dir, pattern = "\\.xlsx$", ignore.case = TRUE))
}

collect_field_rows <- function(config, run_date, active_field = NULL) {
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

  if (is.null(active_field) || !nzchar(active_field) || identical(active_field, "—")) {
    active_field <- NULL
  }

  lapply(fields, function(field) {
    out_dir <- field_output_dir(field, config, run_date)
    is_active <- !is.null(active_field) && identical(trimws(field), trimws(active_field))
    prog <- get_field_step_progress(out_dir, is_active = is_active)
    if (is_active && prog$status %in% c("pending", "queued")) {
      prog$status <- "running"
    }
    summary <- read_field_summary(out_dir)
    last_ckpt_time <- NA_character_
    ckpt_dir <- file.path(out_dir, "Checkpoints")
    if (dir.exists(ckpt_dir)) {
      files <- list.files(ckpt_dir, pattern = "_done\\.rds$", full.names = TRUE)
      if (length(files) > 0) {
        last_ckpt_time <- format(file.mtime(files[which.max(file.mtime(files))]), "%Y-%m-%d %H:%M:%S")
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
      last_update = last_ckpt_time %||% "—",
      is_active = is_active
    )
  })
}

collect_dashboard_status <- function(config, run_date, monitor_cfg = NULL) {
  if (is.null(monitor_cfg)) monitor_cfg <- list()
  tmux_name <- monitor_cfg$tmux_session %||% "RES"
  proc <- get_engine_process_info()
  activity <- parse_log_activity(config, run_date, monitor_cfg)
  live <- read_live_status(config, run_date)

  engine_running <- proc$running
  if (!engine_running && !is.null(live) && isTRUE(live$engine_running)) {
    live_age <- tryCatch(
      difftime(Sys.time(), as.POSIXct(live$updated_at, tz = Sys.timezone()), units = "mins"),
      error = function(e) NA
    )
    if (!is.na(live_age) && live_age < 10) engine_running <- TRUE
  }

  active_field <- activity$field
  fields <- collect_field_rows(config, run_date, active_field = active_field)
  n_complete <- sum(vapply(fields, function(x) x$status == "complete", logical(1)))
  n_total <- length(fields)

  list(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    run_date = run_date,
    engine_running = engine_running,
    engine_pid = proc$pid,
    engine_elapsed = proc$elapsed,
    engine_cpu = proc$cpu,
    engine_mem = proc$mem,
    tmux_res = tmux_session_active(tmux_name),
    tmux_sessions = list_tmux_sessions(),
    fields_complete = n_complete,
    fields_total = n_total,
    current_field = activity$field,
    current_step = activity$step,
    run_id = activity$run_id,
    activity_source = activity$source,
    live_updated = activity$live_updated,
    data_files = count_data_files(config),
    fields = fields
  )
}

start_engine_tmux <- function(monitor_cfg = list()) {
  root <- project_root()
  session <- monitor_cfg$tmux_session %||% "RES"
  if (tmux_session_active(session) && is_engine_running()) {
    return(list(ok = FALSE, message = paste0("Engine zaten calisiyor (tmux:", session, ").")))
  }
  if (tmux_session_active(session) && !is_engine_running()) {
    system2("tmux", c("kill-session", "-t", session), stdout = FALSE, stderr = FALSE)
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
