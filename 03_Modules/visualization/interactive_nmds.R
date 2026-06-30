plot_interactive_nmds <- function(workspace, config, out_path) {
  if (!requireNamespace("plotly", quietly = TRUE)) stop("plotly required")
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) stop("htmlwidgets required")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")

  nmds <- workspace$nmds
  df <- workspace$df
  grouping_var <- workspace$grouping_var
  viz <- config$visualization %||% list()

  nmds_scores <- build_nmds_scores(nmds, df, grouping_var)
  if ("title" %in% names(df)) {
    title_levels <- discover_title_levels(df$title)
    nmds_scores$title <- factor(df$title, levels = title_levels)
  }

  nmds_scores$hover_text <- paste0("<b>", grouping_var, ":</b> ", nmds_scores$.group)
  n_groups <- length(unique(nmds_scores$.group))

  p <- ggplot2::ggplot(nmds_scores, ggplot2::aes(x = NMDS1, y = NMDS2, color = .group, shape = .group, text = hover_text)) +
    ggplot2::geom_point(alpha = 0.5, size = 1.8) +
    ggplot2::scale_color_manual(values = res_color_palette(n_groups, viz$color_palette %||% "dynamic")) +
    ggplot2::scale_shape_manual(values = res_shape_palette(n_groups)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Etkilesimli NMDS")

  if ("title" %in% names(nmds_scores)) {
    p <- p + ggplot2::facet_wrap(~ title, ncol = min(3, length(levels(nmds_scores$title))))
  }

  interactive <- plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))

  htmlwidgets::saveWidget(interactive, out_path, selfcontained = TRUE)
  invisible(out_path)
}
