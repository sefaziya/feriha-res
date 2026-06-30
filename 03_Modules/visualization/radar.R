plot_radar <- function(workspace, config, out_path) {
  if (!requireNamespace("fmsb", quietly = TRUE)) stop("fmsb required")

  grouping_var <- workspace$grouping_var
  df <- add_group_column(workspace$df, grouping_var)
  reduced_vars <- workspace$reduced_vars

  radar_data <- stats::aggregate(
    df[, reduced_vars, drop = FALSE],
    by = list(.group = df$.group),
    FUN = mean,
    na.rm = TRUE
  )

  rownames(radar_data) <- radar_data$.group
  radar_data$.group <- NULL

  radar_norm <- as.data.frame(apply(radar_data, 2, function(x) {
    rng <- max(x) - min(x)
    if (rng == 0) return(rep(0.5, length(x)))
    (x - min(x)) / rng
  }))

  n_vars <- ncol(radar_norm)
  radar_plot_df <- rbind(rep(1, n_vars), rep(0, n_vars), radar_norm)

  png(out_path, width = 1200, height = 900, res = 150)
  on.exit(dev.off(), add = TRUE)
  cols <- res_color_palette(nrow(radar_norm), config$visualization$color_palette %||% "dynamic")
  fmsb::radarchart(radar_plot_df, pcol = cols, plwd = 2, cex.axis = 0.8)

  invisible(out_path)
}
