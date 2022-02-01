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
plot.dir = "../images/study02_expensive"
if (!dir.exists(plot.dir)) dir.create(plot.dir, recursive = TRUE)

# DATA IMPORT
# ===

res = readRDS("../../data/study01_expensive_data.rds") # results
jt = readRDS("../../data/study01_jt.rds") # job table (with algorithm parameters)

dfs = do.call(rbind, lapply(res, function(e) e$res))
dfs = dplyr::left_join(dfs, jt, by = "job.id")

# DATA PREPROCESSING
# ===

# nice short names for heuristcs
algo.keys = c("eax" = "EAX", "lkh" = "LKH")
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

# Here we need to group first since not all algorithms performed the same number of iterations
# (budget was maximum time)
dfs.rt = dfs %>%
  group_by(evolver, algo.pair, feature.names, repl) %>%
  filter(n.evals == max(n.evals)) %>%
  ungroup()

# How many cells are covered? I.e., how many instances are produced per log-point?
cnt = dfs.rt %>%
  group_by(algo.pair, evolver, feature.names, job.id) %>%
  dplyr::summarize(n = n()) %>%
  group_by(algo.pair, evolver, feature.names) %>%
  dplyr::summarize(n.mean = mean(n), n.sdev = sd(n))

# Final iteration
# ===
dfs.rt.features.final = dfs.rt %>%
  group_by(job.id) %>%
  filter(n.evals == max(n.evals)) %>%
  ungroup()


tmp = dfs.rt.features.final %>%
  filter(algo.pair == "EAX vs. LKH", feature.names == "nng_5_n_strong,mst_depth_median", repl == 2)

g = plot_tiles(tmp, objective.description = "PAR10 ratio")
#g = g +  scale_fill_continuous(breaks = seq(0.5, 1.3, by = 0.1), limits = c(0.5, 1.3))
g = g + labs(title = "EAX vs. LKH")
g

ggsave(file.path(plot.dir, "EAX_LKH_nng5_mst_tile_obj.pdf"), plot = g, width = 10, height = 9)



tmp = dfs.rt.features.final
tmp$feature.names = gsub(",", "\nvs.\n", tmp$feature.names, fixed = TRUE)
#tmp[tmp$algo.pair == "LKH vs. EAX", "obj"] = 1 / tmp[tmp$algo.pair == "LKH vs. EAX", "obj"]
g = plot_objective(tmp, show.extras = TRUE)
g = g + facet_wrap(. ~ algo.pair, scale = "free_y")
g = g + theme(legend.position = "top")
g = g + labs(x = NULL, y = "Objective\n(PAR10-score ratio)", fill = "Algorithm")
g = g + scale_y_log10()
g
ggsave(file.path(plot.dir, "EAX_LKH_boxplot_objectives_by_features_and_algopair.pdf"), plot = g, width = 8, height = 5)


table(tmp$algo.pair, tmp$evolver)

# SCATTER-PLOT EAX vs. LKH
# all evolvers
# ===
eax.vs.lkh = filter(tmp, algo.pair == "EAX vs. LKH", grepl("all", evolver))
hist(eax.vs.lkh$obj)

g = plot_parscores(eax.vs.lkh)
#g = g + facet_grid(. ~ collection)
g = g + theme(legend.position = "top")
g = g + labs(x = "PAR10 of EAX", y = "PAR10 of LKH", shape = "Algorithm", color = "Algorithm")
g
ggsave(file.path(plot.dir, "EAX_vs_LKH_parscores.pdf"), plot = g, width = 6, height = 5.5)


# SCATTER-PLOT LKH vs. EAX
# all evolvers
# ===
lkh.vs.eax = filter(tmp, algo.pair == "LKH vs. EAX", grepl("all", evolver))
hist(lkh.vs.eax$obj)

g = plot_parscores(lkh.vs.eax)
#g = g + facet_grid(. ~ collection)
g = g + theme(legend.position = "top")
g = g + labs(x = "PAR10 of LKH", y = "PAR10 of EAX", shape = "Algorithm", color = "Algorithm")
g
ggsave(file.path(plot.dir, "LKH_vs_EAX_parscores.pdf"), plot = g, width = 6, height = 5.5)

stop()
