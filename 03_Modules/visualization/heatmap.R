plot_heatmap <- function(workspace, config, out_path) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")

  df <- add_group_column(workspace$df, workspace$grouping_var)
  reduced_vars <- workspace$reduced_vars

  heatmap_data <- stats::aggregate(
    df[, reduced_vars, drop = FALSE],
    by = list(.group = df$.group),
    FUN = mean,
    na.rm = TRUE
  )

  for (v in reduced_vars) {
    heatmap_data[[v]] <- as.numeric(scale(heatmap_data[[v]]))
  }

  heatmap_long <- tidyr::pivot_longer(heatmap_data, cols = reduced_vars, names_to = "Variable", values_to = "Z_Score")

  p <- ggplot2::ggplot(heatmap_long, ggplot2::aes(x = Variable, y = .group, fill = Z_Score)) +
    ggplot2::geom_tile(color = "white", linewidth = 1) +
    ggplot2::scale_fill_gradient2(low = "#2c7bb6", mid = "#ffffbf", high = "#d7191c", midpoint = 0) +
    ggplot2::geom_text(ggplot2::aes(label = round(Z_Score, 2)), color = "black", size = 3) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(title = "Uretim Profili Isi Haritasi", x = "Uretim Kalemi", y = "Arastirma Alani")

  ggplot2::ggsave(out_path, plot = p, width = 12, height = 7, dpi = 300, limitsize = FALSE)
  invisible(out_path)
}
