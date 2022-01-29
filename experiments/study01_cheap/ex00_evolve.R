library(batchtools)

source("defs.R")
source("../src/utils.R")

# STUDY 1
# ===
# WHAT WE WANT:
# For fixed number of cities n and a pair of algorithms (A, B), evolve instances
# (the QD way) such that each cell contains at most one instance which is easy for
# A and harder for B.
#
# COMBINATIONS OF ALGORITHMS:
# ===
# Focus on solution quality:
# * Nearest-neighbor vs. farthest neighbor (and the other way around)
# * Nearest-neighbor vs. concorde (i.e., vs. optimality)
#
# Focus on runtime until OPT is found:
# * EAX vs. LKH (and the other way around)
#
# COMBINATIONS OF FEATURES
# ===
# Take subsets of 2 or 3 of the following (see FOGA2019 and FOGA2021 TSP papers for justification of the choice):
# * nng_5_n_strong
# * mst_depth_median
# * nng_3_strong_components_max
# * nng_3_n_weak


# ATTENTION!
file.dir = "/scratch/tmp/bossek/tsp-QD-evolve/study01_cheap/bt-qd-evolve"
unlink(file.dir, recursive = TRUE, force = TRUE)

reg = batchtools::makeExperimentRegistry(
  file.dir = file.dir,
  seed = 1L,
  packages = c("tspgen", "salesperson"),
  source = c("../src/algorithms.R", "defs.R", "../src/utils.R"))
#reg$cluster.functions = batchtools::makeClusterFunctionsMulticore(ncpus = 8L, fs.latency = 0)

# no problem at all
batchtools::addProblem("DUMMY", data = list())

batchtools::addAlgorithm("QDEVOLVE", fun = function(job, data, ...) runner(job, data, ...))

# Design
design = data.table::as.data.table(read.table("design.csv", header = TRUE, sep = " "))


algo.designs = list(QDEVOLVE = design)

batchtools::addExperiments(algo.designs = algo.designs, repls = RUNS)

BBmisc::stopf("Registry successfully generated! :-)")

ids = findNotDone()

submitJobs(ids = ids, resources = list(walltime = 60 * 60 * 32, mem = 4000))

stop()

# TEST ENVIR
# ===
res = testJob(1)
# Obs.: some areas hit moreoften -> weakness of mutation operators

# # COLLECT
res = reduceResultsList(findDone())
saveRDS(res, file = file.path(DATA.DIR, "study02_data.rds"))
jt = unwrap(getJobTable()[, c("job.id", "repl", "algo.pars")])
saveRDS(jt, file = file.path(DATA.DIR, "study02_jt.rds"))
