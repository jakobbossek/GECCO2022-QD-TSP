library(tidyverse)
library(ggplot2)

theme_set(
  theme_minimal() + theme(
    panel.background = element_rect(fill = NA, colour = NA),
    plot.background = element_rect(colour = NA, fill = NA)))

# import data
res = readRDS("../data/study01.rds")

# variables and stuff
plot.dir = "../images/study01"
if (!dir.exists(plot.dir)) dir.create(plot.dir, recursive = TRUE)

# helper to generate plots
generate_plots = function(res, output.path) {
  df = res$res
  job = res$job
  g = ggplot(df, )
  n.cols = ncol(df)
  feat.names = colnames(df)[(n.cols - 2L):(n.cols - 1L)]
  feats = strsplit(job$pars$algo.pars$feats, ",")[[1L]]
  feats = strsplit(job$pars$algo.pars$feats, ",")[[1L]]
  algos = c(job$algo.pars$algo.a, job$algo.pars$algo.b)

  # objective
  g1 = ggplot(df, aes_string(x = feat.names[1L], y = feat.names[2L], fill = "obj")) + geom_tile()
  g1 = g1 + labs(title = sprintf("%s vs. %s", algos[1], algos[2]))
  ggsave(paste0(output.path, "/objective.pdf"), plot = g1, width = 5, height = 5.5)

  # number of hits
  g2 = ggplot(df, aes_string(x = feat.names[1L], y = feat.names[2L], fill = "n.hits")) + geom_tile()
  g2 = g2 + labs(title = sprintf("%s vs. %s", algos[1], algos[2]))
  ggsave(paste0(output.path, "/nhits.pdf"), plot = g2, width = 5, height = 5.5)
}

for (i in seq_len(length(res))) {
  output.path = file.path(plot.dir, i)
  dir.create(output.path)
  generate_plots(res[[i]], output.path = output.path)
}


