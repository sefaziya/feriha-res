# Checkpoint system

PIPELINE_STEPS <- c(
  "preprocess", "distance", "permanova", "permdisp",
  "nmds", "envfit", "xgboost", "save", "DONE"
)

checkpoint_path <- function(out_dir, field, step) {
  ckpt_dir <- file.path(out_dir, "Checkpoints")
  dir.create(ckpt_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(ckpt_dir, paste0("step_", step, "_done.rds"))
}

write_checkpoint <- function(field, step, out_dir, payload = list()) {
  path <- checkpoint_path(out_dir, field, step)
  saveRDS(c(list(field = field, step = step, time = Sys.time()), payload), path)
  if (exists("notify_checkpoint", mode = "function")) {
    notify_checkpoint(field, step, out_dir)
  }
  invisible(path)
}

checkpoint_exists <- function(out_dir, field, step) {
  file.exists(checkpoint_path(out_dir, field, step))
}

get_last_completed_step <- function(out_dir, field) {
  completed <- character()
  for (step in PIPELINE_STEPS) {
    if (checkpoint_exists(out_dir, field, step)) {
      completed <- c(completed, step)
    }
  }
  if (length(completed) == 0) return(NULL)
  completed[length(completed)]
}

resume_from_step <- function(out_dir, field) {
  last <- get_last_completed_step(out_dir, field)
  if (is.null(last) || last == "DONE") {
    return(PIPELINE_STEPS[[1]])
  }
  idx <- match(last, PIPELINE_STEPS)
  if (is.na(idx) || idx >= length(PIPELINE_STEPS)) {
    return(NULL)
  }
  PIPELINE_STEPS[[idx + 1]]
}

should_run_step <- function(step, start_step) {
  if (is.null(start_step)) return(FALSE)
  match(step, PIPELINE_STEPS) >= match(start_step, PIPELINE_STEPS)
}
