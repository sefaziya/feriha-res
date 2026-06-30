plot_nmds <- function(workspace, config, out_path) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")

  nmds <- workspace$nmds
  df <- workspace$df
  grouping_var <- workspace$grouping_var
  viz <- config$visualization %||% list()

  nmds_scores <- build_nmds_scores(nmds, df, grouping_var)
  centroids <- build_centroids(nmds_scores)
  n_groups <- length(unique(nmds_scores$.group))

  p <- ggplot2::ggplot(nmds_scores, ggplot2::aes(x = NMDS1, y = NMDS2, color = .group, shape = .group)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
    ggplot2::stat_ellipse(ggplot2::aes(fill = .group), geom = "polygon", alpha = 0.1, color = NA, level = 0.95) +
    ggplot2::stat_ellipse(ggplot2::aes(color = .group), geom = "path", linewidth = 0.8, level = 0.95, show.legend = FALSE) +
    ggplot2::geom_point(alpha = viz$nmds_alpha %||% 0.4, size = 1.2) +
    ggplot2::geom_point(data = centroids, ggplot2::aes(x = NMDS1, y = NMDS2, color = .group, shape = .group), size = 5, stroke = 2) +
    ggplot2::scale_color_manual(values = res_color_palette(n_groups, viz$color_palette %||% "dynamic")) +
    ggplot2::scale_fill_manual(values = res_color_palette(n_groups, viz$color_palette %||% "dynamic")) +
    ggplot2::scale_shape_manual(values = res_shape_palette(n_groups)) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position = viz$legend_position %||% "bottom",
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
    ) +
    ggplot2::labs(
      title = "Akademik Uretim Kulturu Haritasi (NMDS)",
      subtitle = paste("Bray-Curtis | Stress:", round(nmds$stress, 3)),
      x = "NMDS Boyut 1", y = "NMDS Boyut 2",
      color = "Alan", fill = "Alan", shape = "Alan"
    )

  ggplot2::ggsave(out_path, plot = p, width = 12, height = 8, dpi = 300)
  invisible(out_path)
}
