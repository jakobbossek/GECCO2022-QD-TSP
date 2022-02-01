library(batchtools)

source("defs.R")
source("../src/utils.R")

# ATTENTION!
file.dir = "/scratch/tmp/bossek/tsp-QD-evolve/study02_expensive/bt-qd-evolve"
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
submitJobs(ids = ids, resources = list(walltime = 60 * 60 * 48, mem = 4000))

stop()

# TEST ENVIR
# ===
# res = testJob(1)

# COLLECT
# ===
res = reduceResultsList(findDone())
saveRDS(res, file = file.path(DATA.DIR, "study02_expensive_data.rds"))
jt = unwrap(getJobTable()[, c("job.id", "repl", "algo.pars")])
saveRDS(jt, file = file.path(DATA.DIR, "study02_expensive_jt.rds"))
