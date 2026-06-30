# IO utilities — config and Excel loading

load_config <- function(path = NULL) {
  if (is.null(path)) {
    path <- file.path(project_root(), "01_Config", "config.yml")
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required.")
  }
  yaml::read_yaml(path)
}

load_all_excels <- function(data_dir = NULL, config = NULL) {
  if (is.null(config)) config <- load_config()
  if (is.null(data_dir)) {
    data_dir <- file.path(project_root(), config$data$data_dir %||% "00_Data")
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required.")
  }

  files <- list.files(data_dir, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    stop("No Excel files found in: ", data_dir)
  }

  dfs <- lapply(files, function(f) {
    df <- readxl::read_excel(f)
    df$.source_file <- basename(f)
    as.data.frame(df)
  })

  master <- do.call(rbind, dfs)
  rownames(master) <- NULL

  required <- config$data$required_columns
  missing <- setdiff(required, names(master))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  master
}

discover_numeric_columns <- function(df, config) {
  drop_vars <- config$data$drop_vars %||% character()
  meta_cols <- c(
    config$data$required_columns,
    "academician_id", "name", "university", "faculty",
    "department", "department_sub_department", "additional_research_areas",
    ".source_file"
  )

  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, intersect(num_cols, meta_cols))
  setdiff(num_cols, drop_vars)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

project_root <- function() {
  root <- getOption("res.project_root")
  if (is.null(root) || !nzchar(root)) {
    return(normalizePath(getwd()))
  }
  root
}
