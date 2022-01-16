DATA.DIR = "../data/study01/results"
STORAGE.DIR = "../qdstorage/study01/storage"
MAX.TIME = Inf #60 * 60 * 24 * 2 # 48h
MAX.ITER =  5000L
CUTOFF.TIME = 60 * 3 # 3m
REPLS = 3L # number of runs for stochastic algorithms

ALGO.PARS = list(
  "nearest-neighbor" = list(),
  "farthest-neighbor" = list(),
  "eax" = list(with.restarts = TRUE),
  "lkh" = list(with.restarts = TRUE, max.trials = 1e8)
)

# TSP SOLVERS
# ===

# define paths to TSP solvers
pathToSolvers = path.expand("~/.config/salesperson/solvers")

if (!dir.exists(pathToSolvers))
  BBmisc::stopf("Path to solvers '%s' does not exist! Did you run the install script in salesperson package?", pathToSolvers)

SOLVER.PATH = c(
  "eax"      = file.path(pathToSolvers, "eax/main"),
  "lkh"      = file.path(pathToSolvers, "LKH-2.0.7/LKH"),
  "concorde" = file.path(pathToSolvers, "concorde/linux/concorde")
)
salesperson::solverPaths(as.list(SOLVER.PATH))


getPARscore = function(times, cutoff = 3600, f = 10, ...) {
  checkmate::assertNumeric(times, lower = 0, any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(cutoff, lower = 1)
  checkmate::assertNumber(f, lower = 1)

  idx.timed.out = which(times >= cutoff)
  if (length(idx.timed.out) > 0L)
    times[idx.timed.out] = f * cutoff
  return(mean(times))
}

make_aggregation_fun = function(target, cutoff.time) {
  if (target == "tour.length")
    return(base::mean)
  force(cutoff.time)
  f = function(x) {
    getPARscore(x, cutoff = cutoff.time)
  }
  return(f)
}

# Generate feature extractor
# Here, only the "features of interest" are returned
make_feature_fun = function(feats.of.interest) {
  function(x) {
    unlist(salesperson::getFeatureSet(x, black.list = "VRP", normalize = TRUE)[feats.of.interest])
  }
}

# Generate objective function
#
# @param algo.a [character(1)]
#  First algorithm.
# @param algo.b [character(1)]
#  Second algorithm.
# @param target [character(1)]
#  What do we use as the quality measure? E.g., \dQuote{tour.length}.
# @param aggr.fun [function(x)]
#  Function to aggregate results if algorithm(s) is/are stochastic.
#  Defaults to \code{base::mean}.
# @param repls [integer(1)]
#  Number of independent runs for stochastic algorithms.
#  Defaults to 3.
# @param cutoff.time [integer(1)]
#  Cutoff time in seconds. Defaults to 3 (for testing purposes).
# @return [numeric(1)] Single numeric objective.
make_objective_fun = function(algo.a, algo.b, algo.pars.a = list(), algo.pars.b = list(),
  target, aggr.fun = base::mean, repls = 3L, cutoff.time = 3L) {
  force(algo.a)
  force(algo.b)
  force(target)
  force(aggr.fun)
  force(repls)
  force(cutoff.time)

  function(x) {
    runAlgorihm = function(x, algo, algo.pars, repls) {
      res = sapply(seq_len(repls), function(i) {
        tmp = salesperson::runSolver(algo, instance = x, solver.pars = algo.pars)[[target]]
        if (target != "runtime")
          return(tmp)
        return(tmp[3L])
      })
      if (repls > 1L)
        res = aggr.fun(res)
      return(res)
    }

    # is algorithm stochastic?
    repls.a = if (algo.a %in% c("eax", "lkh")) repls else 1L
    repls.b = if (algo.b %in% c("eax", "lkh")) repls else 1L

    # need optimale tour length for PAR-score
    if (target != "tour.length") {
      opt = runSolver("concorde", instance = x)$tour.length
      algo.pars.a$opt.tour.length = opt
      algo.pars.b$opt.tour.length = opt
    }

    res.a = runAlgorihm(x, algo.a, algo.pars.a, repls.a)
    res.b = runAlgorihm(x, algo.b, algo.pars.b, repls.b)

    return(res.a / res.b)
  }
}


runner = function(job, data, ...) {
  # gather design parameters
  args = list(...)
  aggr.fun = make_aggregation_fun(args$target, CUTOFF.TIME)
  obj.fun = make_objective_fun(
    args$algo.a, args$algo.b,
    ALGO.PARS[[args$algo.a]], ALGO.PARS[[args$algo.b]],
    args$target, aggr.fun = aggr.fun, repls = REPLS,
    cutoff.time = CUTOFF.TIME)
  feats.of.interest = as.character(strsplit(args$feats, split = ",")[[1L]])
  feat.fun = make_feature_fun(feats.of.interest)

  # storage
  storage = file.path(STORAGE.DIR, sprintf("%i", job$job.id))
  if (!dir.exists(storage))
    dir.create(storage, recursive = TRUE)

  # run QD algorithm
  res = qd(
    obj.fun,
    feat.fun,
    feats.of.interest,
    n = args$n,
    max.iter = MAX.ITER,
    max.time = MAX.TIME,
    storage.path = storage)
  res$job.id = job$job.id

  saveRDS(res, file = file.path(DATA.DIR, sprintf("QD_%i.rds", job$job.id)))
  return(list(res = res, job = job))
}
