plot_envfit <- function(workspace, config, out_path) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")
  if (!requireNamespace("ggrepel", quietly = TRUE)) stop("ggrepel required")

  nmds <- workspace$nmds
  df <- workspace$df
  fit <- workspace$envfit
  grouping_var <- workspace$grouping_var
  viz <- config$visualization %||% list()
  mult <- config$analysis$envfit_vector_mult %||% 0.8
  p_thresh <- config$analysis$envfit_p_threshold %||% 0.05

  nmds_scores <- build_nmds_scores(nmds, df, grouping_var)
  centroids <- build_centroids(nmds_scores)
  n_groups <- length(unique(nmds_scores$.group))

  vector_coords <- as.data.frame(vegan::scores(fit, display = "vectors"))
  vector_coords$Variable <- rownames(vector_coords)
  vector_coords$Pval <- fit$vectors$pvals
  significant_vectors <- vector_coords[vector_coords$Pval < p_thresh, , drop = FALSE]

  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.3) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.3) +
    ggplot2::geom_point(data = centroids, ggplot2::aes(x = NMDS1, y = NMDS2, color = .group, shape = .group), size = 6, stroke = 1.5, alpha = 0.8) +
    ggplot2::geom_segment(data = significant_vectors, ggplot2::aes(x = 0, y = 0, xend = NMDS1 * mult, yend = NMDS2 * mult), arrow = ggplot2::arrow(length = grid::unit(0.3, "cm"), type = "closed"), color = "black", linewidth = 1) +
    ggrepel::geom_text_repel(data = significant_vectors, ggplot2::aes(x = NMDS1 * mult, y = NMDS2 * mult, label = Variable), size = 5, fontface = "bold") +
    ggplot2::scale_color_manual(values = res_color_palette(n_groups, viz$color_palette %||% "dynamic")) +
    ggplot2::scale_shape_manual(values = res_shape_palette(n_groups)) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Eksenlerin Karakteristigi: Akademik Uretim Vektorleri",
      subtitle = paste("Anlamli vektorler p <", p_thresh),
      x = "NMDS Boyut 1", y = "NMDS Boyut 2",
      color = "Alan", shape = "Alan"
    )

  ggplot2::ggsave(out_path, plot = p, width = 12, height = 8, dpi = 300)
  invisible(out_path)
}
