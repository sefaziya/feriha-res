# Dynamic palettes and score builders

res_color_palette <- function(n, palette = "dynamic") {
  n <- max(as.integer(n), 1L)
  if (palette == "set1" && n <= 9) {
    cols <- RColorBrewer::brewer.pal(max(3, n), "Set1")
    return(cols[seq_len(n)])
  }
  grDevices::hcl(h = seq(15, 375, length.out = n + 1), c = 65, l = 55)[seq_len(n)]
}

res_shape_palette <- function(n) {
  shapes <- c(16, 17, 15, 3, 7, 8, 4, 18, 9, 10, 11, 12, 13, 14)
  rep(shapes, length.out = max(as.integer(n), 1L))
}

build_nmds_scores <- function(nmds, df, grouping_var) {
  scores <- as.data.frame(vegan::scores(nmds, display = "sites"))
  scores$.group <- df[[grouping_var]]
  scores
}

build_centroids <- function(nmds_scores) {
  stats::aggregate(
    cbind(NMDS1, NMDS2) ~ .group,
    data = nmds_scores,
    FUN = mean
  )
}

add_group_column <- function(df, grouping_var, name = ".group") {
  df[[name]] <- df[[grouping_var]]
  df
}

discover_title_levels <- function(titles) {
  titles <- unique(as.character(titles))
  titles <- titles[!is.na(titles) & nzchar(titles)]
  rank <- function(x) {
    xu <- toupper(x)
    if (grepl("DOKTOR", xu)) return(1L)
    if (grepl("DOCENT|DOÇENT", xu)) return(2L)
    if (grepl("PROFES", xu)) return(3L)
    99L
  }
  titles[order(vapply(titles, rank, integer(1)), titles)]
}
