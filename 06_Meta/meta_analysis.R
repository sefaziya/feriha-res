# Meta analysis across academic fields

run_meta_analysis <- function(config, run_date = NULL) {
  if (is.null(run_date)) run_date <- format(Sys.Date(), "%Y%m%d")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite required")

  global <- build_global_summary(config, run_date)
  if (length(global$fields) == 0) {
    log_warn("No field summaries found for meta analysis.")
    return(invisible(NULL))
  }

  base <- file.path(project_root(), config$output$base_dir %||% "05_Output")
  meta_dir <- file.path(base, "_meta", run_date)
  plots_dir <- file.path(meta_dir, "Plots")
  dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

  fields_df <- do.call(rbind, lapply(global$fields, function(s) {
    data.frame(
      field = s$field %||% NA_character_,
      N = s$N %||% NA_real_,
      research_fields_count = s$research_fields_count %||% NA_real_,
      PERMANOVA_R2 = s$PERMANOVA_R2 %||% NA_real_,
      PERMANOVA_P = s$PERMANOVA_P %||% NA_real_,
      PERMDISP_P = s$PERMDISP_P %||% NA_real_,
      Stress = s$Stress %||% NA_real_,
      stringsAsFactors = FALSE
    )
  }))

  utils::write.csv(fields_df, file.path(meta_dir, "field_comparison.csv"), row.names = FALSE)

  if (nrow(fields_df) >= 2 && requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot(fields_df, ggplot2::aes(x = .data$field, y = .data$PERMANOVA_R2, fill = .data$field)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::labs(title = "Alanlar Arasi PERMANOVA R2 Karsilastirmasi", x = "", y = "R2")
    ggplot2::ggsave(file.path(plots_dir, "meta_permanova_r2.png"), p, width = 10, height = 6, dpi = 300)

    mat <- fields_df[, c("PERMANOVA_R2", "Stress", "N"), drop = FALSE]
    rownames(mat) <- fields_df$field
    mat <- scale(mat)
    dist_mat <- as.matrix(stats::dist(mat))
    sim_df <- as.data.frame(as.table(dist_mat))
    colnames(sim_df) <- c("Field1", "Field2", "Distance")

    p2 <- ggplot2::ggplot(sim_df, ggplot2::aes(x = .data$Field1, y = .data$Field2, fill = .data$Distance)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(low = "#f3f0df", high = "#005088") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Alan Benzerlik Matrisi (Ozet Metrikler)")
    ggplot2::ggsave(file.path(plots_dir, "meta_similarity_matrix.png"), p2, width = 8, height = 6, dpi = 300)
  }

  invisible(global)
}
