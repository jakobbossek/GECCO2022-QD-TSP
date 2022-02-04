library(tidyverse)
library(ggplot2)
library(scales)

source("../src/ploting.R")

theme_set(
  theme_minimal() + theme(
    panel.background = element_rect(fill = NA, colour = NA),
    plot.background = element_rect(colour = NA, fill = NA)))

# VARIABLES AND STUFF
# ===
plot.dir = "../images/study01_cheap"
if (!dir.exists(plot.dir)) dir.create(plot.dir, recursive = TRUE)


# DATA IMPORT
# ===

res = readRDS("../../data/study01_cheap_data.rds") # results
jt = readRDS("../../data/study01_cheap_jt.rds") # job table (with algorithm parameters)

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

# Calculate correlation between the number of updates / log(number of hits)
# and the objective value
dfs.cor = dfs.tl %>%
  group_by(evolver, algo.pair, feature.names, repl) %>%
  dplyr::summarize(cor = cor(log(n.hits), obj), n.hits = max(n.hits)) %>%
  group_by(evolver, algo.pair, feature.names) %>%
  dplyr::summarize(cor = mean(cor), n.hits = mean(n.hits)) %>%
  ungroup() %>%
  filter(grepl("all", evolver))

qplot(evolver, n.hits, data = dfs.cor)

# Exemplary scatter-plots of negative correlation
tmp = filter(dfs.tl, algo.pair == "NI vs. FI", grepl("all", evolver), repl == 1L)

# Number of hits
tmp %>% group_by(evolver, feature.names) %>% summarize(x = min(n.hits), y = max(n.hits))

g = ggplot(tmp, aes(x = n.hits, y = obj, shape = as.factor(evolver), color = as.factor(evolver)))
g = g + geom_vline(data = group_by(tmp, evolver, feature.names) %>% dplyr:::summarize(n.hits = max(n.hits)), aes(xintercept = n.hits, color = evolver), linetype = "dashed")
g = g + geom_point(alpha = 0.35)
g = g + geom_smooth(method = "lm", se = FALSE)
g = g + theme(legend.position = "top")
g = g + scale_color_brewer(palette = "Dark2")
g = g + labs(color = "Algorithm", shape = "Algorithm", x = "Number of hits [log-scale]", y = "Objective\n(Tour length ratio) -> min")
g = g + facet_grid(algo.pair ~ feature.names)
g = g + scale_x_log10()
g
ggsave(file.path(plot.dir, "FI_NI_scatterplot_nhits_vs_obj.pdf"), plot = g, width = 7.5, height = 4)


# Q: How many cells are covered (i.e., how many instances are produced?)
cnt = dfs.tl %>%
  group_by(algo.pair, evolver, feature.names, job.id) %>%
  dplyr::summarize(n = n()) %>%
  group_by(algo.pair, evolver, feature.names) %>%
  dplyr::summarize(n.mean = mean(n), n.sdev = sd(n))

#g = ggplot(cnt, aes(x = evolver, y = ))


meta = dfs.tl %>%
  group_by(job.id) %>%
  mutate(n.instances = n(), obj.best = min(obj)) %>%
  filter(row_number() == 1L) %>%
  ungroup()

meta.aggr = meta %>%
  group_by(evolver, feature.names) %>%
  summarize_at(c("n.replaces", "n.updates", "obj.best"), c(mean = mean, sd = sd, max = max)) %>%
  ungroup()


# get the number of instances per parameter combination and nr. of iterations performed so far
dfs.tl.instances = dfs %>%
  group_by(algo.pair, evolver, feature.names, n.evals, job.id) %>%
  dplyr::summarize(n = n())

# select one special case for the paper
tmp = dfs.tl.instances %>%
  filter(n.evals %in% c(10000, 50000, 100000, seq(200000, 1e6, by = 200000)))

# NOW LINE PLOTS WITH ERRORBARS
# ===
tmp = dfs.tl.instances %>%
  group_by(algo.pair, evolver, feature.names, n.evals) %>%
  dplyr::summarize(n.mean = mean(n), n.sd = sd(n)) %>%
  ungroup()

g = plot_progress(tmp)
g = g + guides(shape = guide_legend(nrow = 2), color = guide_legend(nrow = 2))

ggsave(file.path(plot.dir, "nr_of_instances_over_time.pdf"), plot = g, width = 8, height = 6.7)
#stop()

dfg = filter(tmp, grepl("mst", feature.names))
g = plot_progress(dfg)
ggsave(file.path(plot.dir, "nr_of_instances_over_time_nng5_mst.pdf"), plot = g, width = 10, height = 4)

dfg = filter(tmp, !grepl("mst", feature.names))
g = plot_progress(dfg)
ggsave(file.path(plot.dir, "nr_of_instances_over_time_nng3_nng3.pdf"), plot = g, width = 10, height = 4)


# now get for one experimental setting select four





tmp = dfs.tl.features.final %>%
  filter(algo.pair == "FI vs. NI", feature.names == "nng_5_n_strong,mst_depth_median", repl == 1)

g = plot_tiles(tmp, objective.description = "Tour length ratio")
g = g +  scale_fill_continuous(breaks = seq(0.5, 1.3, by = 0.1), limits = c(0.5, 1.3))
g = g + labs(title = "FI vs. NI")
ggsave(file.path(plot.dir, "FI_NI_nng5_mst_tile_obj.pdf"), plot = g, width = 8.5, height = 7.5)


tmp = dfs.tl.features.final %>%
  filter(algo.pair == "NI vs. FI", feature.names == "nng_5_n_strong,mst_depth_median", repl == 1)

g = plot_tiles(tmp, objective.description = "Tour length ratio")
g = g +  scale_fill_continuous(breaks = seq(0.5, 1.3, by = 0.1), limits = c(0.5, 1.3))
g = g + labs(title = "NI vs. FI")
ggsave(file.path(plot.dir, "NI_FI_nng5_mst_tile_obj.pdf"), plot = g, width = 8.5, height = 7.5)

tmp = dfs.tl.features.final %>%
  filter(algo.pair == "FI vs. NI", feature.names == "nng_3_strong_components_max,nng_3_n_weak", repl == 1)

g = plot_tiles(tmp, objective.description = "Tour length ratio")
g = g +  scale_fill_continuous(breaks = seq(0.5, 1.3, by = 0.1), limits = c(0.5, 1.3))
g = g + labs(title = "FI vs. NI")
ggsave(file.path(plot.dir, "FI_NI_nng3_nng3_tile_obj.pdf"), plot = g, width = 8.5, height = 7.5)

tmp = dfs.tl.features.final %>%
  filter(algo.pair == "NI vs. FI", feature.names == "nng_3_strong_components_max,nng_3_n_weak", repl == 1)

g = plot_tiles(tmp, objective.description = "Tour length ratio")
g = g +  scale_fill_continuous(breaks = seq(0.5, 1.3, by = 0.1), limits = c(0.5, 1.3))
g = g + labs(title = "NI vs. FI")
ggsave(file.path(plot.dir, "NI_FI_nng3_nng3_tile_obj.pdf"), plot = g, width = 8.5, height = 7.5)


# NUMBER OF HITS / UPDATES
# ===
tmp = dfs.tl.features.final %>%
  filter(algo.pair == "FI vs. NI", feature.names == "nng_5_n_strong,mst_depth_median", repl == 1,
    evolver %in% c("(1+1) EA [all]", "QD [all]"))

source("../src/ploting.R")
g = plot_tiles(tmp, "n.updates", objective.description = "Number of updates", add.initial.population = FALSE)
#g = g + scale_fill_gradient(trans = "log")
#g = g + scale_fill_continuous(breaks = seq(0.5, 1.3, by = 0.1), limits = c(0.5, 1.3))
g = g + labs(title = "FI vs. NI", fill = "Number of updates")
g = g + facet_wrap(. ~ evolver)
ggsave(file.path(plot.dir, "FI_NI_nng5_mst_tile_nupdates.pdf"), plot = g, width = 8.5, height = 4)

g = plot_tiles(tmp, "first.hit", objective.description = "Number of updates", add.initial.population = FALSE)
#g = g + scale_fill_gradient(trans = "log")
#g = g + scale_fill_continuous(breaks = seq(0.5, 1.3, by = 0.1), limits = c(0.5, 1.3))
g = g + labs(title = "FI vs. NI", fill = "First hitting time")
g = g + facet_wrap(. ~ evolver)
ggsave(file.path(plot.dir, "FI_NI_nng5_mst_tile_firsthit.pdf"), plot = g, width = 8.5, height = 4)



tmp = dfs.tl.features.final
dd = tmp %>%
  group_by(feature.names, algo.pair, evolver, repl) %>%
  dplyr::summarize(n.instances = n(), n.instances.median = sum(obj <= median(obj)), max.updates = max(n.updates), max.hits = max(n.hits), best.obj = min(obj), median.obj = median(obj)) %>%
  group_by(feature.names, algo.pair, evolver) %>%
  dplyr::summarize(n.mean = mean(n.instances), sd = sd(n.instances), n.instances.median = median(n.instances.median), max.updates = max(max.updates), max.hits = max(max.hits), best.obj = min(best.obj), median.obj = median(median.obj)) %>%
  ungroup() %>%
  arrange(feature.names, algo.pair, evolver)

load_all("~/repos/software/r/tblutils/")

split.by = c("feature.names", "algo.pair")
dd = highlight(dd, split.by, "n.mean", order.fun = "max")
dd = highlight(dd, split.by, "sd", order.fun = "min")
dd = highlight(dd, split.by, "max.updates", order.fun = "max")
dd = highlight(dd, split.by, "best.obj", order.fun = "min")
dd = highlight(dd, split.by, "max.hits", order.fun = "max")
dd = highlight(dd, split.by, "median.obj", order.fun = "min")
dd = highlight(dd, split.by, "n.instances.median", order.fun = "max")

dd = tblutils::widen(dd, split.col = "algo.pair", widen.cols = setdiff(colnames(dd), c("algo.pair", "feature.names", "evolver")))
dd$feature.names[grepl("nng_3_strong", dd$feature.names)] = "FC1"
dd$feature.names[grepl("nng_5_n_strong", dd$feature.names)] = "FC2"
dd$algo.pair = NULL
dd


#  row_spec(row = c(4, 8, 16, 20), extra_latex_after = "\\cline{2-11}")

cns = c("\\textbf{FC}", "\\textbf{Algorithm}", rep(c("\\textbf{mean}", "\\textbf{std}", "<\\textbf{med}", "\\textbf{upd.}", "\\textbf{hits}", "\\textbf{best}", "\\textbf{median}"), 2))
aln = c("l", "l", rep("r", 14))
caption = "Table of different aggregated values of interest for the experiments on the simple insertion heuristics. Best values (for the statistics of covered cells or objective values) or maximal values (for cell update statistics are highlighted)."
library(kableExtra)
ddlat = kableExtra::kable(dd, "latex", col.names = cns, align = aln, booktabs = TRUE, label = "statistics_cheap", caption = caption, escape = FALSE) %>%
  add_header_above(c(" " = 2, "\\textbf{Nr. of cells}" = 3, "\\textbf{Cell statistics}" = 2, "\\textbf{Objective}" = 2, "\\textbf{Nr. of cells}" = 3, "\\textbf{Cell statistics}" = 2, "\\textbf{Objective}" = 2), escape = FALSE) %>%
  add_header_above(c(" " = 2, "\\textbf{FI vs. NI}" = 7, "\\textbf{NI vs. FI}" = 7), escape = FALSE) %>%
  collapse_rows(columns = 1, latex_hline = "major", valign = "middle")

cat(ddlat, file = "tables/cheap.tex")



stop()



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


tmp = dfs.tl.features.final
tmp$feature.names = gsub(",", "\nvs.\n", tmp$feature.names, fixed = TRUE)
g = plot_objective(tmp, show.extras = TRUE)
g = g + facet_grid(feature.names ~ algo.pair)
g = g + theme(legend.position = "top")
#g = g + guides(color = guide_legend(nrow = 1))
g = g + labs(x = NULL, y = "Objective\n(tour length ratio)", color = "Algorithm")
ggsave(file.path(plot.dir, "boxplot_objectives_by_features_and_algopair.pdf"), plot = g, width = 8, height = 6.5)
g


stop("DONE :-)")


# ATTIC
# ===

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

