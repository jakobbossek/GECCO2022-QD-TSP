library(tidyverse)
library(ggplot2)

theme_set(
  theme_minimal() + theme(
    panel.background = element_rect(fill = NA, colour = NA),
    plot.background = element_rect(colour = NA, fill = NA)))

# VARIABLES AND STUFF
# ===
plot.dir = "../images/study01"
if (!dir.exists(plot.dir)) dir.create(plot.dir, recursive = TRUE)


union_string = function(...) {
  args = unlist(list(...))
  args = sapply(args, function(e) gsub("[[:punct:]]", "_", e))
  #print(args)
  re::collapse(args, sep = "_")
}

# DATA IMPORT
# ===

res = readRDS("../../data/study01_expensive_data.rds") # results
jt = readRDS("../../data/study01_jt.rds") # job table (with algorithm parameters)

dfs = do.call(rbind, lapply(res, function(e) e$res))
dfs = dplyr::left_join(dfs, jt, by = "job.id")

# DATA PREPROCESSING
# ===

# nice short names for heuristcs
algo.keys = c("farthest_insertion" = "FI", "nearest_insertion" = "NI", "cheapest_insertion" = "CI", "eax" = "EAX", "lkh" = "LKH")
dfs$algo.a = recode(dfs$algo.a, !!!algo.keys)
dfs$algo.b = recode(dfs$algo.b, !!!algo.keys)
dfs$algo.pair = sprintf("%s vs. %s", dfs$algo.a, dfs$algo.b)

# nice names for targets
target.keys = c("tour.length" = "TL", "runtime" = "RT")
dfs$target = recode(dfs$target, !!!target.keys)

# nice names for algorithms
dfs$evolver[dfs$method == "qd"] = sprintf("QD [%s]", dfs[dfs$method == "qd", "collection"])
dfs$evolver[dfs$method != "qd"] = sprintf("(%i+1) EA [%s]", dfs[dfs$method != "qd", "mu"], dfs[dfs$method != "qd", "collection"])

dfs$evolver2[dfs$method == "qd"] = "QD"
dfs$evolver2[dfs$method != "qd"] = sprintf("(%i+1) EA", dfs[dfs$method != "qd", "mu"])

dfs$method = dfs$instance = NULL

# DATA ANALYSIS (FOCUS ON TOUR LENGTH AND SIMPLE HEURISTICS)
# ===

dfs.rt = filter(dfs, target == "RT", n.evals <= 1e+05)

# Q: How many cells are covered (i.e., how many instances are produced?)
cnt = dfs.rt %>%
  group_by(target, algo.pair, evolver, feature.names, repl) %>%
  dplyr::summarize(n = n()) %>%
  group_by(target, algo.pair, evolver, feature.names) %>%
  dplyr::summarize(n.mean = mean(n), n.sdev = sd(n))


# get the number of instances per parameter combination and nr. of iterations performed so far
dfs.rt.instances = dfs.rt %>%
  group_by(target, algo.pair, evolver, feature.names, n.evals, repl) %>%
  dplyr::summarize(n = n())

# plot the development of the number of instances over time
plot_objective = function(tmp) {
  g = ggplot(tmp, aes_string(x = "evolver", y = "obj", fill = "evolver"))
  g = g + geom_boxplot(alpha = 0.5)
  g = g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  g = g + scale_fill_brewer(palette = "Dark2")
  return(g)
}

plot_tiles = function(tmp, what = "obj", objective.description) {
  feat.names = strsplit(tmp$feature.names[1L], ",")[[1L]]
  tmp = separate(tmp, feature.values, into = feat.names, convert = TRUE, sep = ",")
  g = ggplot(tmp, aes_string(x = feat.names[1L], y = feat.names[2L], fill = what)) + geom_tile()
  g = g + theme(legend.position = "top")
  g = g + facet_grid(evolver2 ~ collection)
  g = g + labs(
    x = sprintf("Feature 1\n(%s)", feat.names[1L]),
    y = sprintf("Feature 2\n(%s)", feat.names[2L]),
    fill = sprintf("Objective\n(%s)", objective.description))
  return(g)
}


generate_plots = function(tmp) {
  feat.names = strsplit(tmp$feature.names[1L], ",")[[1L]]
  g = plot_tiles(tmp, "obj", "tour length ratio")
  fn = sprintf("%s/%s_tiles_objective.pdf", plot.dir, union_string(tmp$algo.pair[1L], feat.names[1], feat.names[2]))
  ggsave(fn, plot = g, width = 12, height = 7)

  g = plot_tiles(tmp, "n.hits", "Nr. of hits") + labs(fill = "Nr. of hits")
  fn = sprintf("%s/%s_tiles_nhits.pdf", plot.dir, union_string(tmp$algo.pair[1L], feat.names[1], feat.names[2]))
  ggsave(fn, plot = g, width = 12, height = 7)

  g = plot_tiles(tmp, "n.updates", "Nr. of updates") + labs(fill = "Nr. of updates")
  fn = sprintf("%s/%s_tiles_nupdates.pdf", plot.dir, union_string(tmp$algo.pair[1L], feat.names[1], feat.names[2]))
  ggsave(fn, plot = g, width = 12, height = 7)

  g = plot_tiles(tmp, "first.hit", "First hit") + labs(fill = "First hit")
  fn = sprintf("%s/%s_tiles_firsthit.pdf", plot.dir, union_string(tmp$algo.pair[1L], feat.names[1], feat.names[2]))
  ggsave(fn, plot = g, width = 12, height = 7)
}

# Final iteration
# ===
dfs.rt.features.final = dfs.rt %>%
  group_by(job.id) %>%
  filter(n.evals == max(n.evals)) %>%
  ungroup()

dfs.rt.features.final %>%
  group_by(algo.pair, feature.names) %>%
  dplyr::do(generate_plots(.))



tmp = dfs.rt.features.final
tmp$feature.names = gsub(",", "\nvs.\n", tmp$feature.names, fixed = TRUE)
g = plot_objective(tmp)
g = g + facet_wrap(. ~ algo.pair, scale = "free_y")
g = g + theme(legend.position = "top")
g = g + labs(x = NULL, y = "Objective\n(PAR10-score ratio)", fill = "Algorithm")
g = g + scale_y_log10()
g

plot_parscores = function(tmp) {
  g = ggplot(tmp, aes_string(x = "obj.algo.a", y = "obj.algo.b", shape = "evolver2", color = "evolver2"))
  g = g + ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "solid", colour = "gray20", size = 1.3)
  g = g + ggplot2::geom_hline(yintercept = c(1800), linetype = "dashed", colour = "gray40")
  g = g + ggplot2::geom_vline(xintercept = c(1800), linetype = "dashed", colour = "gray40")
  n.shapes = length(unique(tmp$evolver2))
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


# SCATTER-PLOT EAX vs. LKH
# all evolvers
# ===
eax.vs.lkh = filter(tmp, algo.pair == "EAX vs. LKH", grepl("all", evolver))
g = plot_parscores(eax.vs.lkh)
#g = g + facet_grid(. ~ collection)
g = g + theme(legend.position = "top")
g = g + labs(x = "PAR10 of EAX", y = "PAR10 of LKH", shape = "Algorithm", color = "Algorithm")
g
ggsave(file.path(plot.dir, "eax_vs_lkh_parscores.pdf"), plot = g, width = 6, height = 5.5)


# SCATTER-PLOT EAX vs. LKH
# all evolvers
# ===
lkh.vs.eax = filter(tmp, algo.pair == "LKH vs. EAX")
g = plot_parscores(lkh.vs.eax)
g = g + facet_grid(. ~ collection)
g = g + theme(legend.position = "top")
g = g + labs(x = "PAR10 of LKH", y = "PAR10 of EAX", shape = "Algorithm", color = "Algorithm")
g



ggsave(file.path(plot.dir, "boxplot_objectives_by_features_and_algopair.pdf"), plot = g, width = 8, height = 10)
g




stop()

# Cell-plots of n., first.hit, obj


generate_plots = function(df) {
  feat.names = strsplit(df$feature.names[1L], ",")[[1L]]
  df = separate(df, feature.values, into = feat.names, convert = TRUE, sep = ",")
  g1 = ggplot(df, aes_string(x = feat.names[1L], y = feat.names[2L], fill = "obj")) + geom_tile()
  g1 = g1 + facet_grid(. ~ evolver)
  fn = sprintf("%s/%s_objective.pdf", plot.dir, union_string(df$collection[1], df$algo.pair[1L], feat.names[1], feat.names[2]))
  ggsave(fn, plot = g1, width = 12, height = 4)

  g1 = ggplot(df, aes_string(x = feat.names[1L], y = feat.names[2L], fill = "n.hits")) + geom_tile()
  g1 = g1 + facet_grid(. ~ evolver)
  nhits_breaks = c(100, 250, 500, 500, 2500, 5000)
  g1 = g1 + scale_fill_gradient(name = "Log nr. of hits", trans = "log", breaks = nhits_breaks, labels = nhits_breaks)
  fn = sprintf("%s/%s_nhits.pdf", plot.dir, union_string(df$collection[1], df$algo.pair[1L], feat.names[1], feat.names[2]))
  ggsave(fn, plot = g1, width = 12, height = 4)

  g1 = ggplot(df, aes_string(x = feat.names[1L], y = feat.names[2L], fill = "first.hit")) + geom_tile()
  g1 = g1 + facet_grid(. ~ evolver)
  fn = sprintf("%s/%s_firsthit.pdf", plot.dir, union_string(df$collection[1], df$algo.pair[1L], feat.names[1], feat.names[2]))
  ggsave(fn, plot = g1, width = 12, height = 4)

  g1 = ggplot(df, aes_string(x = "evolver", y = "obj", fill = "evolver")) + geom_boxplot(alpha = 0.5)
  g1 = g1 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  g1 = g1 + scale_fill_brewer(pallette = "Dark2")
  fn = sprintf("%s/%s_objective_boxplots.pdf", plot.dir, union_string(df$collection[1], df$algo.pair[1L], feat.names[1], feat.names[2]))
  ggsave(fn, plot = g1, width = 4, height = 4)
  #BBmisc::pause()
}

dfs %>%
  group_by(target, algo.pair, feature.names, collection) %>%
  dplyr::do(generate_plots(.))

print("Hallo")

#dd = filter(dfs, algo.pair == "NI vs. FI", evolver == "QD [all]")
#generate_plots(dd)


stop("DONE")

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
  method = job$algo.pars$method
  collection = job$algo.pars$collection

  # objective
  g1 = ggplot(df, aes_string(x = feat.names[1L], y = feat.names[2L], fill = "obj")) + geom_tile()
  g1 = g1 + labs(
    title = sprintf("%s (%s)", method, collection),
    subtitle = sprintf("%s vs. %s", algos[1], algos[2]))
  ggsave(paste0(output.path, "/objective.pdf"), plot = g1, width = 5, height = 5.5)

  # number of hits
  g2 = ggplot(df, aes_string(x = feat.names[1L], y = feat.names[2L], fill = "n.hits")) + geom_tile()
  g2 = g2 + labs(
    title = sprintf("%s (%s)", method, collection),
    subtitle = sprintf("%s vs. %s", algos[1], algos[2]))
  ggsave(paste0(output.path, "/nhits.pdf"), plot = g2, width = 5, height = 5.5)

  # first hits
  g3 = ggplot(df, aes_string(x = feat.names[1L], y = feat.names[2L], fill = "first.hit")) + geom_tile()
  g3 = g3 + labs(
    title = sprintf("%s (%s)", method, collection),
    subtitle = sprintf("%s vs. %s", algos[1], algos[2]))
  ggsave(paste0(output.path, "/firsthit.pdf"), plot = g3, width = 5, height = 5.5)
}

for (i in seq_len(length(res))) {
  output.path = file.path(plot.dir, i)
  dir.create(output.path)
  generate_plots(res[[i]], output.path = output.path)
}


