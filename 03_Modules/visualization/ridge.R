plot_ridge <- function(workspace, config, out_path) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")
  if (!requireNamespace("ggridges", quietly = TRUE)) stop("ggridges required")

  df <- add_group_column(workspace$df, workspace$grouping_var)
  reduced_vars <- workspace$reduced_vars
  viz <- config$visualization %||% list()
  n_groups <- length(unique(df$.group))

  df_long <- df[, c(".group", reduced_vars), drop = FALSE]
  df_long <- tidyr::pivot_longer(df_long, cols = reduced_vars, names_to = "Variable", values_to = "Value")

  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = Value, y = .group, fill = .group)) +
    ggridges::geom_density_ridges(alpha = 0.7, scale = 1.5, color = "white", show.legend = FALSE) +
    ggplot2::facet_wrap(~ Variable, scales = "free_x", ncol = 3) +
    ggplot2::scale_fill_manual(values = res_color_palette(n_groups, viz$color_palette %||% "dynamic")) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      title = "Degisken Bazli Uretim Yogunluklari (Log Donusumlu)",
      x = "Uretim Miktari (Log1p)", y = ""
    )

  ggplot2::ggsave(out_path, plot = p, width = 14, height = 8, dpi = 300)
  invisible(out_path)
}
