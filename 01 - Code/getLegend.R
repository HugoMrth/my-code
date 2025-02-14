getLegend <- function(plot) {
  # get tabular interpretation of plot
  plot_table <- ggplot_gtable(ggplot_build(plot))
  #  Mark only legend in plot
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  # extract legend
  legend <- plot_table$grobs[[legend_plot]]
  # return legend
  return(legend)
}
