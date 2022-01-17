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

# DATA IMPORT
# ===

res = readRDS("../data/study01_data.rds")
jt = readRDS("../data/study01_jt.rds")

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
target.keys = c("tour.length" = "TL", "par.score" = "PAR")
dfs$target = recode(dfs$target, !!!target.keys)

# nice names for algorithms
dfs$evolver[dfs$method == "qd"] = sprintf("QD [%s]", dfs[dfs$method == "qd", "collection"])
dfs$evolver[dfs$method != "qd"] = sprintf("(%i+1) EA [%s]", dfs[dfs$method != "qd", "mu"], dfs[dfs$method != "qd", "collection"])

dfs$method = NULL

# DATA ANALYSIS
# ===

# Q: How many cells are covered (i.e., how many instances are produced?)
dfs %>%
  group_by(target, algo.pair, evolver, feature.names) %>%
  dplyr::summarize(n = n())


# Cell-plots of n.hits, first.hit, obj

union_string = function(...) {
  args = unlist(list(...))
  args = sapply(args, function(e) gsub(" ", "_", e))
  print(args)
  re::collapse(args, sep = "_")
}

generate_plots = function(df) {
  feat.names = strsplit(df$feature.names[1L], ",")[[1L]]
  df = separate(df, feature.values, into = feat.names, convert = TRUE, sep = ",")
  g1 = ggplot(df, aes_string(x = feat.names[1L], y = feat.names[2L], fill = "obj")) + geom_tile()
  g1 = g1 + facet_grid(. ~ evolver)
  fn = sprintf("%s/%s_objective.pdf", plot.dir, union_string(df$algo.pair[1L], feat.names[1], feat.names[2]))
  ggsave(fn, plot = g1, width = 12, height = 2.5)

  g1 = ggplot(df, aes_string(x = feat.names[1L], y = feat.names[2L], fill = "n.hits")) + geom_tile()
  g1 = g1 + facet_grid(. ~ evolver)
  fn = sprintf("%s/%s_nhits.pdf", plot.dir, union_string(df$algo.pair[1L], feat.names[1], feat.names[2]))
  ggsave(fn, plot = g1, width = 12, height = 2.5)

  g1 = ggplot(df, aes_string(x = feat.names[1L], y = feat.names[2L], fill = "first.hit")) + geom_tile()
  g1 = g1 + facet_grid(. ~ evolver)
  fn = sprintf("%s/%s_firsthit.pdf", plot.dir, union_string(df$algo.pair[1L], feat.names[1], feat.names[2]))
  ggsave(fn, plot = g1, width = 12, height = 2.5)
  #BBmisc::pause()
}

dfs %>%
  group_by(target, algo.pair, feature.names) %>%
  dplyr::do(generate_plots(.))

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


