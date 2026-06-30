# Field discovery engine

discover_fields <- function(df) {
  af <- unique(as.character(df$academic_field))
  af <- af[!is.na(af) & nzchar(af)]
  rf <- unique(as.character(df$research_field))
  rf <- rf[!is.na(rf) & nzchar(rf)]
  list(
    academic_fields = sort(af),
    research_fields = sort(rf)
  )
}

build_hierarchy <- function(df) {
  split(df, df$academic_field)
}

sanitize_field_name <- function(field) {
  gsub("[^A-Za-z0-9._-]+", "_", field)
}

get_output_dir <- function(field, config, run_date = NULL) {
  if (is.null(run_date)) {
    run_date <- format(Sys.Date(), "%Y%m%d")
  }
  base <- config$output$base_dir %||% "05_Output"
  file.path(project_root(), base, sanitize_field_name(field), run_date)
}

build_execution_plan <- function(academic_fields, config, run_date = NULL) {
  if (is.null(run_date)) {
    run_date <- format(Sys.Date(), "%Y%m%d")
  }

  pending <- Filter(function(field) {
    out_dir <- get_output_dir(field, config, run_date)
    ckpt <- file.path(out_dir, "Checkpoints", "step_DONE_done.rds")
  !file.exists(ckpt)
  }, academic_fields)

  pending
}

filter_field_data <- function(df, field) {
  df[as.character(df$academic_field) == field, , drop = FALSE]
}

count_research_fields <- function(df, grouping_var) {
  length(unique(as.character(df[[grouping_var]])))
}
