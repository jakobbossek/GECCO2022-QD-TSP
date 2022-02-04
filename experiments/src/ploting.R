library(tidyverse)
library(ggplot2)
library(scales)

union_string = function(...) {
  args = unlist(list(...))
  args = sapply(args, function(e) gsub("[[:punct:]]", "_", e))
  #print(args)
  re::collapse(args, sep = "_")
}

plot_progress = function(tmp) {
  tmp$feature.names = gsub(",", "\nvs\n", tmp$feature.names)
  g = ggplot(tmp, aes(x = n.evals, y = n.mean, color = evolver, shape = evolver))
  g = g + geom_line(aes(group = evolver))
  g = g + geom_point()
  g = g + geom_errorbar(aes(ymin = n.mean - n.sd, ymax = n.mean + n.sd), width = 100)
  g = g + scale_color_brewer(palette = "Dark2")
  g = g + theme(legend.position = "top")
  g = g + scale_y_continuous()
  g = g + guides(shape = guide_legend(nrow = 1), color = guide_legend(nrow = 1))
  g = g + labs(
    x = "Iteration",
    y = "Number of covered cells\n(I.e., number of different instances)",
    color = "Algorithm",
    shape = "Algorithm")
  g = g + facet_grid(feature.names ~ algo.pair, scales = "free_y")
  return(g)
}

plot_objective = function(tmp, show.extras = FALSE) {
  g = ggplot(tmp, aes_string(x = "evolver", y = "obj", color = "evolver"))
  if (show.extras) {
    rects = data.frame(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1)
    g = g + geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, alpha = 0.05, fill = "green")
    g = g + geom_hline(yintercept = 1, color = "black", alpha = 0.5, size = 1)
  }

  g = g + geom_boxplot(outlier.shape = 4)
  g = g + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  g = g + scale_color_brewer(palette = "Dark2")
  return(g)
}

plot_tiles = function(tmp, what = "obj", objective.description, add.initial.population = TRUE) {
  feat.names = strsplit(tmp$feature.names[1L], ",")[[1L]]
  tmp = separate(tmp, feature.values, into = feat.names, convert = TRUE, sep = ",")
  g = ggplot(tmp, aes_string(x = feat.names[1L], y = feat.names[2L], fill = what)) + geom_tile()

  if (add.initial.population) {
    firsthit = filter(tmp, first.hit == 1)
    # for some reason for (1+1)-EA there are two first-hits
    firsthit1 = filter(firsthit, !grepl("1+1", evolver, fixed = TRUE))
    firsthit2 = filter(firsthit, grepl("1+1", evolver, fixed = TRUE)) %>% group_by(evolver) %>% filter(row_number() == 1L)
    firsthit = rbind(firsthit1, firsthit2)

    g = g + geom_point(data = firsthit, mapping = aes(fill = NULL), color = "white", shape = 4)
  }

  g = g + guides(fill = guide_colourbar(barwidth = 30, barheight = 1))
  g = g + theme(legend.position = "top")
  g = g + facet_wrap(vars(evolver), nrow = 3L)
  g = g + labs(
    x = sprintf("Feature 1\n(%s)", feat.names[1L]),
    y = sprintf("Feature 2\n(%s)", feat.names[2L]),
    fill = sprintf("Objective\n(%s)", objective.description))
  return(g)
}

plot_parscores = function(tmp) {
  g = ggplot(tmp, aes_string(x = "obj.algo.a", y = "obj.algo.b", shape = "evolver", color = "evolver"))
  g = g + ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "solid", colour = "gray20", size = 1.3)
  g = g + ggplot2::geom_hline(yintercept = c(1800), linetype = "dashed", colour = "gray40")
  g = g + ggplot2::geom_vline(xintercept = c(1800), linetype = "dashed", colour = "gray40")
  n.shapes = length(unique(tmp$evolver))
  g = g + ggplot2::scale_x_log10(
    breaks = c(0, 1, 10, 100, 500, 1800),
    labels = c("0", "1", "10", "100", "500", "10 x T = 1800"),
    limits = c(0.5, 1800))
  g = g + ggplot2::scale_y_log10(
    breaks = c(0, 1, 10, 100, 500, 1800),
    labels = c("0", "1", "10", "100", "500", "10 x T = 1800"),
    limits = c(0.5, 1800))
  g = g + ggplot2::scale_shape_manual(values = c(1, 4, 5, 16, 6, 0, 17, 18)[seq_len(n.shapes)])
  g = g + geom_point(alpha = 0.5)
  g = g + scale_color_brewer(palette = "Dark2")
  return(g)
}
