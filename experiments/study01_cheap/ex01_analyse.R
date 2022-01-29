library(tidyverse)
library(ggplot2)

theme_set(
  theme_minimal() + theme(
    panel.background = element_rect(fill = NA, colour = NA),
    plot.background = element_rect(colour = NA, fill = NA)))

# VARIABLES AND STUFF
# ===
plot.dir = "../images/study01_cheap"
if (!dir.exists(plot.dir)) dir.create(plot.dir, recursive = TRUE)


union_string = function(...) {
  args = unlist(list(...))
  args = sapply(args, function(e) gsub("[[:punct:]]", "_", e))
  #print(args)
  re::collapse(args, sep = "_")
}

# DATA IMPORT
# ===

res = readRDS("../../data/study02_data.rds") # results
jt = readRDS("../../data/study02_jt.rds") # job table (with algorithm parameters)

dfs = do.call(rbind, lapply(res, function(e) e$res))
dfs = dplyr::left_join(dfs, jt, by = "job.id")

# DATA PREPROCESSING
# ===

# nice short names for heuristcs
algo.keys = c("farthest_insertion" = "FI", "nearest_insertion" = "NI")
dfs$algo.a = recode(dfs$algo.a, !!!algo.keys)
dfs$algo.b = recode(dfs$algo.b, !!!algo.keys)
dfs$algo.pair = sprintf("%s vs. %s", dfs$algo.a, dfs$algo.b)

# nice names for algorithms
dfs$evolver[dfs$method == "qd"] = sprintf("QD [%s]", dfs[dfs$method == "qd", "collection"])
dfs$evolver[dfs$method != "qd"] = sprintf("(%i+1) EA [%s]", dfs[dfs$method != "qd", "mu"], dfs[dfs$method != "qd", "collection"])

dfs$evolver2[dfs$method == "qd"] = "QD"
dfs$evolver2[dfs$method != "qd"] = sprintf("(%i+1) EA", dfs[dfs$method != "qd", "mu"])

dfs$method = dfs$instance = NULL

# DATA ANALYSIS (FOCUS ON TOUR LENGTH AND SIMPLE HEURISTICS)
# ===

dfs.tl = filter(dfs, n.evals == max(dfs$n.evals))

# Q: How many cells are covered (i.e., how many instances are produced?)
cnt = dfs.tl %>%
  group_by(algo.pair, evolver, feature.names, job.id) %>%
  dplyr::summarize(n = n()) %>%
  group_by(algo.pair, evolver, feature.names) %>%
  dplyr::summarize(n.mean = mean(n), n.sdev = sd(n))

#g = ggplot(cnt, aes(x = evolver, y = ))



# get the number of instances per parameter combination and nr. of iterations performed so far
dfs.tl.instances = dfs %>%
  group_by(algo.pair, evolver, feature.names, n.evals, job.id) %>%
  dplyr::summarize(n = n())

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
  g = g + geom_tile(data = filter(tmp, first.hit == 1), mapping = aes(fill = NULL), color = "white")
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
  fn = sprintf("%s/%s_tiles_objective.pdf", plot.dir, union_string(tmp$algo.pair[1L], feat.names[1], feat.names[2], tmp$repl[1]))
  ggsave(fn, plot = g, width = 12, height = 7)

  g = plot_tiles(tmp, "n.hits", "Nr. of hits") + labs(fill = "Nr. of hits")
  fn = sprintf("%s/%s_tiles_nhits.pdf", plot.dir, union_string(tmp$algo.pair[1L], feat.names[1], feat.names[2], tmp$repl[1]))
  ggsave(fn, plot = g, width = 12, height = 7)

  g = plot_tiles(tmp, "n.updates", "Nr. of updates") + labs(fill = "Nr. of updates")
  fn = sprintf("%s/%s_tiles_nupdates.pdf", plot.dir, union_string(tmp$algo.pair[1L], feat.names[1], feat.names[2], tmp$repl[1]))
  ggsave(fn, plot = g, width = 12, height = 7)

  g = plot_tiles(tmp, "first.hit", "First hit") + labs(fill = "First hit")
  fn = sprintf("%s/%s_tiles_firsthit.pdf", plot.dir, union_string(tmp$algo.pair[1L], feat.names[1], feat.names[2], tmp$repl[1]))
  ggsave(fn, plot = g, width = 12, height = 7)
}

# Final iteration
# ===
dfs.tl.features.final = filter(dfs, n.evals == max(n.evals))
dfs.tl.features.final = dfs.tl.features.final %>%
  group_by(algo.pair, evolver, feature.names, repl) %>%
  filter(job.id == unique(job.id)[1L])

dfs.tl.features.final %>%
  group_by(algo.pair, feature.names, repl) %>%
  dplyr::do(generate_plots(.))

dfs.up = dfs.tl.features.final %>%
  group_by(algo.pair, feature.names, evolver, repl) %>%
  dplyr::summarize(n.up = sum(n.updates)) %>%
  group_by(algo.pair, feature.names, evolver) %>%
  dplyr::summarize(n.up.mean = sum(n.up), n.up.sd = sd(n.up)) %>%
  ungroup()

g = ggplot(dfs.up, aes(x = as.factor(evolver), y = n.up.mean, color = evolver))
g = g + geom_point()
g = g + geom_errorbar(aes(ymin = n.up.mean - n.up.sd, ymax = n.up.mean + n.up.sd), width = .2, position=position_dodge(0.05))
g = g + scale_color_brewer(palette = "Dark2")
g = g + theme(legend.position = "top")
g = g + labs(
  x = "Iteration",
  y = "Number of covered cells\n(I.e., number of different instances)",
  color = "Algorithm")
g = g + facet_grid(algo.pair ~ feature.names)




tt = dfs %>%
  filter(algo.pair == "FI vs. NI", feature.names == "nng_5_n_strong,mst_depth_median",
    repl == 4, n.evals %in% c(10000, 50000, 100000, 200000, 300000, 400000, 500000), collection == "all")

g = plot_tiles(tt, "obj", "tour length ratio") + facet_grid(evolver ~ n.evals)
g


tt = dfs %>%
  filter(algo.pair == "FI vs. NI", feature.names == "nng_5_n_strong,mst_depth_median", collection == "all",
    n.evals == 1e06)
g = plot_tiles(tt, "obj", "tour length ratio") + facet_grid(evolver ~ repl)
g


tt = dfs.tl.features.final %>%
  filter(algo.pair == "FI vs. NI", feature.names == "nng_5_n_strong,mst_depth_median")

g = plot_tiles(tt, "obj", "tour length ratio")



tmp = dfs.tl.features.final
tmp$feature.names = gsub(",", "\nvs.\n", tmp$feature.names, fixed = TRUE)
g = plot_objective(tmp)
g = g + facet_grid(feature.names ~ algo.pair)
g = g + theme(legend.position = "top")
g = g + labs(x = NULL, y = "Objective\n(tour length ratio)", fill = "Algorithm")
ggsave(file.path(plot.dir, "boxplot_objectives_by_features_and_algopair.pdf"), plot = g, width = 8, height = 10)
g

