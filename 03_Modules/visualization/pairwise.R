plot_pairwise <- function(workspace, config, out_path) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")
  if (!requireNamespace("reshape2", quietly = TRUE)) stop("reshape2 required")

  nmds <- workspace$nmds
  df <- workspace$df
  grouping_var <- workspace$grouping_var

  nmds_scores <- build_nmds_scores(nmds, df, grouping_var)
  centroids <- build_centroids(nmds_scores)

  c_coords <- centroids[, c("NMDS1", "NMDS2"), drop = FALSE]
  rownames(c_coords) <- centroids$.group
  dist_df <- as.matrix(stats::dist(c_coords))
  heatmap_c <- reshape2::melt(dist_df)

  p <- ggplot2::ggplot(heatmap_c, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = round(value, 2)), color = "white", size = 3) +
    ggplot2::scale_fill_gradient(low = "#005088", high = "#f3f0df") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(title = "Disiplin Merkezleri Arasi Mesafe Matrisi", x = "", y = "")

  ggplot2::ggsave(out_path, plot = p, width = 12, height = 10, dpi = 300, limitsize = FALSE)
  invisible(out_path)
}
