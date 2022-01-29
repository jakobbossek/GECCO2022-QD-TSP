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

res = readRDS("../../data/study01_data.rds") # results
jt = readRDS("../../data/study01_jt.rds") # job table (with algorithm parameters)

dfs = do.call(rbind, lapply(res, function(e) e$res))
dfs = dplyr::left_join(dfs, jt, by = "job.id")

# DATA PREPROCESSING
# ===

# nice short names for heuristcs
algo.keys = c("farthest_insertion" = "FI", "nearest_insertion" = "NI", "cheapest_insertion" = "CI", "eax" = "EAX", "lkh" = "lkh")
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

dfs.tl = filter(dfs, target == "TL", n.evals <= 1e+05)


tt = filter(dfs.tl, n.evals == 1e05)

tt %>%
  group_by(algo.pair, feature.names, evolver) %>%
  dplyr::summarize(n = length(unique(job.id)))


# Q: How many cells are covered (i.e., how many instances are produced?)
cnt = dfs.tl %>%
  group_by(target, algo.pair, evolver, feature.names, repl) %>%
  dplyr::summarize(n = n()) %>%
  group_by(target, algo.pair, evolver, feature.names) %>%
  dplyr::summarize(n.mean = mean(n), n.sdev = sd(n))


# get the number of instances per parameter combination and nr. of iterations performed so far
dfs.tl.instances = dfs.tl %>%
  group_by(target, algo.pair, evolver, feature.names, n.evals, repl) %>%
  dplyr::summarize(n = n())

# select one special case for the paper
# select one special case for the paper
tmp = dfs.tl.instances
%>%
  filter(
    grepl("simple", evolver))#, # only with all mutation operator
    algo.pair == "FI vs. NI",
    feature.names == "nng_3_strong_components_max,nng_3_n_weak")

# plot the development of the number of instances over time
g = ggplot(tmp, aes(x = as.factor(n.evals), y = n, fill = evolver, color = evolver))
g = g + geom_boxplot(alpha = 0.5)
g = g + geom_jitter(alpha = 0.3)
g = g + scale_fill_brewer(palette = "Dark2")
g = g + scale_color_brewer(palette = "Dark2")
g = g + theme(legend.position = "top")
g = g + labs(
  x = "Iteration",
  y = "Number of covered cells\n(I.e., number of different instances)",
  fill = "Algorithm",
  color = "Algorithm")
g = g + facet_grid(algo.pair ~ feature.names)
#g
ggsave(file.path(plot.dir, "nr_of_instances_over_time_all.pdf"), plot = g, width = 14, height = 10)

# NOW LINE PLOTS WITH ERRORBARS
# ===
tmp = dfs.tl.instances %>%
  group_by(algo.pair, evolver, feature.names, n.evals) %>%
  dplyr::summarize(n.mean = mean(n), n.sd = sd(n)) %>%
  ungroup()

g = ggplot(tmp, aes(x = as.factor(n.evals), y = n.mean, color = evolver, shape = evolver))
g = g + geom_line(aes(group = evolver))
g = g + geom_point()
g = g + geom_errorbar(aes(ymin = n.mean - n.sd, ymax = n.mean + n.sd), width = .2, position=position_dodge(0.05))
g = g + scale_color_brewer(palette = "Dark2")
g = g + theme(legend.position = "top")
g = g + labs(
  x = "Iteration",
  y = "Number of covered cells\n(I.e., number of different instances)",
  color = "Algorithm",
  shape = "Algorithm")
g = g + facet_grid(algo.pair ~ feature.names)


# tmp = dfs.tl.instances %>%
#   filter(
#     grepl("all", evolver), # only with all mutation operator
#     algo.pair == "FI vs. NI",
#     feature.names == "nng_3_strong_components_max,nng_3_n_weak")

# # plot the development of the number of instances over time
# g = ggplot(tmp, aes(x = as.factor(n.evals), y = n, fill = evolver, color = evolver))
# g = g + geom_boxplot(alpha = 0.5)
# g = g + geom_jitter(alpha = 0.3)
# g = g + scale_fill_brewer(palette = "Dark2")
# g = g + scale_color_brewer(palette = "Dark2")
# g = g + theme(legend.position = "top")
# g = g + labs(
#   x = "Iteration",
#   y = "Number of covered cells\n(I.e., number of different instances)",
#   fill = "Algorithm",
#   color = "Algorithm")
# g
# ggsave(file.path(plot.dir, "nr_of_instances_over_time.pdf"), plot = g, width = 14, height = 4)

# now get for one experimental setting select four


plot_objective = function(tmp) {
  g = ggplot(tmp, aes_string(x = "evolver", y = "obj", fill = "evolver"))
  g = g +geom_boxplot(alpha = 0.5)
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
dfs.tl.features.final = filter(dfs.tl, n.evals == 1e+05, repl == 1)

dfs.tl.features.final %>%
  group_by(algo.pair, feature.names) %>%
  dplyr::do(generate_plots(.))



tmp = dfs.tl.features.final
tmp$feature.names = gsub(",", "\nvs.\n", tmp$feature.names, fixed = TRUE)
g = plot_objective(tmp)
g = g + facet_grid(feature.names ~ algo.pair)
g = g + theme(legend.position = "top")
g = g + labs(x = NULL, y = "Objective\n(tour length ratio)", fill = "Algorithm")
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
  nhits_breaks = c(100, 250, 500, 1000, 2500, 5000)
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


