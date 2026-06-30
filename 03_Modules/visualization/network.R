plot_network <- function(workspace, config, out_path) {
  if (!requireNamespace("igraph", quietly = TRUE)) stop("igraph required")

  nmds <- workspace$nmds
  df <- workspace$df
  grouping_var <- workspace$grouping_var

  nmds_scores <- build_nmds_scores(nmds, df, grouping_var)
  centroids <- build_centroids(nmds_scores)

  c_coords <- centroids[, c("NMDS1", "NMDS2"), drop = FALSE]
  rownames(c_coords) <- centroids$.group
  dist_df <- as.matrix(stats::dist(c_coords))
  adj_matrix <- 1 / (1 + dist_df)
  graph <- igraph::graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)

  png(out_path, width = 1200, height = 900, res = 150)
  on.exit(dev.off(), add = TRUE)
  plot(graph,
       vertex.label.cex = 0.6,
       vertex.label.color = "black",
       edge.width = igraph::E(graph)$weight * 5,
       vertex.size = 12,
       main = "Uretim Kulturu Benzerlik Agi")

  invisible(out_path)
}
