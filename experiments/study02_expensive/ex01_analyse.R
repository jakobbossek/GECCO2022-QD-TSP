
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




tmp = dfs.rt %>%
  filter(algo.pair == "EAX vs. LKH", feature.names == "nng_5_n_strong,mst_depth_median", repl == 3)

g = plot_tiles(tmp, objective.description = "PAR10 length ratio", add.initial.population = FALSE)
g = g + labs(title = "LKH vs. EAX")
ggsave(file.path(plot.dir, "LKH_EAX_nng5_mst_tile_obj.pdf"), plot = g, width = 8.5, height = 7.5)


tmp = dfs.rt %>%
  filter(algo.pair == "LKH vs. EAX", feature.names == "nng_5_n_strong,mst_depth_median", repl == 2)

g = plot_tiles(tmp, objective.description = "PAR10 length ratio", add.initial.population = FALSE)
g = g + labs(title = "EAX vs. LKH")
ggsave(file.path(plot.dir, "EAX_LKH_nng5_mst_tile_obj.pdf"), plot = g, width = 8.5, height = 7.5)


tmp = dfs
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
caption = "Table of different aggregated values of interest for the experiments on state-of-the-art inexact TSP solver EAX and LKH. Best values (for the statistics of covered cells or objective values) or maximal values (for cell update statistics are highlighted)."
library(kableExtra)
ddlat = kableExtra::kable(dd, "latex", col.names = cns, align = aln, booktabs = TRUE, label = "statistics_cheap", caption = caption, escape = FALSE) %>%
  add_header_above(c(" " = 2, "\\textbf{Nr. of cells}" = 3, "\\textbf{Cell statistics}" = 2, "\\textbf{Objective}" = 2, "\\textbf{Nr. of cells}" = 3, "\\textbf{Cell statistics}" = 2, "\\textbf{Objective}" = 2), escape = FALSE) %>%
  add_header_above(c(" " = 2, "\\textbf{FI vs. NI}" = 7, "\\textbf{NI vs. FI}" = 7), escape = FALSE) %>%
  collapse_rows(columns = 1, latex_hline = "major", valign = "middle")

cat(ddlat, file = "tables/expensive.tex")
