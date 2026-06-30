# Field-level summary builder

extract_permanova_metrics <- function(permanova) {
  if (is.null(permanova)) {
    return(list(R2 = NA_real_, P = NA_real_))
  }
  model_row <- which(rownames(permanova) == "Model")
  if (length(model_row) == 0) model_row <- 1L
  r2 <- if ("R2" %in% colnames(permanova)) permanova[model_row, "R2"] else NA_real_
  p <- if ("Pr(>F)" %in% colnames(permanova)) permanova[model_row, "Pr(>F)"] else NA_real_
  list(R2 = as.numeric(r2), P = as.numeric(p))
}

extract_permdisp_p <- function(permdisp) {
  if (is.null(permdisp) || is.null(permdisp$anova)) return(NA_real_)
  an <- permdisp$anova
  if ("Pr(>F)" %in% colnames(an)) {
    return(as.numeric(an[1, "Pr(>F)"]))
  }
  NA_real_
}

extract_top_variables <- function(xgb, n = 5) {
  imp <- xgb$importance
  if (is.null(imp) || nrow(imp) == 0) return(character())
  if (!"Importance" %in% names(imp)) return(character())
  imp <- imp[order(-imp$Importance), , drop = FALSE]
  head(as.character(imp$Variable), n)
}

build_field_summary <- function(field, results, start_time, config) {
  perm <- extract_permanova_metrics(results$permanova)
  permdisp_p <- extract_permdisp_p(results$permdisp)
  stress <- if (!is.null(results$nmds)) as.numeric(results$nmds$stress) else NA_real_

  list(
    field = field,
    N = nrow(results$df),
    research_fields_count = count_research_fields(results$df, results$grouping_var),
    PERMANOVA_R2 = perm$R2,
    PERMANOVA_P = perm$P,
    PERMDISP_P = permdisp_p,
    Stress = stress,
    TopVariables = extract_top_variables(results$xgb),
    Runtime = format_runtime(start_time)
  )
}

write_execution_manifest <- function(academic_fields, plan, config, run_date) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(invisible(NULL))
  base <- file.path(project_root(), config$output$base_dir %||% "05_Output")
  meta_dir <- file.path(base, "_meta", run_date)
  dir.create(meta_dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- list(
    run_date = run_date,
    updated_at = as.character(Sys.time()),
    academic_fields = academic_fields,
    pending_at_start = plan
  )
  jsonlite::write_json(
    manifest,
    file.path(meta_dir, "execution_manifest.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )
  invisible(manifest)
}

build_global_summary <- function(config, run_date = NULL) {
  if (is.null(run_date)) run_date <- format(Sys.Date(), "%Y%m%d")
  base <- file.path(project_root(), config$output$base_dir %||% "05_Output")

  field_dirs <- list.dirs(base, recursive = FALSE, full.names = TRUE)
  field_dirs <- field_dirs[basename(field_dirs) != "_meta"]

  summaries <- lapply(field_dirs, function(fd) {
    summary_path <- file.path(fd, run_date, "Metadata", "summary.json")
    if (!file.exists(summary_path)) return(NULL)
    jsonlite::read_json(summary_path, simplifyVector = TRUE)
  })
  summaries <- Filter(Negate(is.null), summaries)

  meta_dir <- file.path(base, "_meta", run_date)
  dir.create(meta_dir, recursive = TRUE, showWarnings = FALSE)

  global <- list(
    generated_at = as.character(Sys.time()),
    run_date = run_date,
    fields = summaries
  )

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(global, file.path(meta_dir, "global_summary.json"), pretty = TRUE, auto_unbox = TRUE)
  }

  invisible(global)
}
