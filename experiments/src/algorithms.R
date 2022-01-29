#' @title EA for instance generation (with footprint saving)
#'
#' @description
#' Simple \eqn{(mu+1)} Evolutionary Algorithm as used in Mersmann et al.,
#' Bossek et al. etc. The difference is that the algorithm tracks its
#' footprint by storing the best solution per feature combination.
#'
#' @param obj.fun [function(x, ...)]\cr
#'   Objective function.
#'   Must return a single numeric value.
#' @param feat.fun [function(x, ...)]\cr
#'   Feature extractor.
#'   Must return a named numeric vector.
#' @param n [integer(1)]\cr
#'   Instance size.
#' @param mu [\integer(1)]\cr
#'   Population size.
#'   Defaults to 1
#' @param feats.of.interest [character]\cr
#'   Names of relevant features.
#' @param max.evals [integer(1)]\cr
#'   Maximum number of function evaluations, i.e., maximum number of TSP
#'   solver runs.
#' @param max.time [integer(1)]\cr
#'   Maximum time (in seconds).
#' @param log.every [integer(1)]\cr
#'   If not \code{NULL} the snapshot is taken every \code{log.every} iterations.
#'   Default is \code{Inf} (i.e., no time limit).
#' @param storage.path [character]\cr
#'   Path to folder where the algorithm should outsource generated instances
#'   (they are not kept in-memory due to the high number).
#' @param collection [tspgen_collection]
#'   Collection of mutation operators (see tspgen).
#' @param boundary.handling[character(1)]
#'   How to deal with out-of-bounds points? (see tspgen).
#' @param ... [any]
#'  Not used at the moment.
#' @return [data.frame] Single data frame with
#' \describe{
#'  \item{instance}{Unique consecutive ID. Instance is named instance.tsp}
#'  \item{obj}{Objective value of the instance.}
#'  \item{n.updates}{Number of times the corresponding feature combination was updated.}
#'  \item{...}{Features (see \code{feats.of.interest})}.
#' }
#' @export
ea = function(
  obj.fun, feat.fun,
  n, mu = 1L,
  feats.of.interest = NULL,
  max.evals = 10L, max.time = Inf,
  log.every = NULL,
  storage.path = "qdstorage",
  collection = tspgen::init("sophisticated"), boundary.handling = "uniform",
  ...) {

  # init storage (O(1) access on average)
  storage = datastructures::hashmap("character")

  n.keys = 0L
  scale.factor = 1000000
  n.feats = length(feats.of.interest)
  time.started = proc.time()[3L]
  n.evals = 1
  n.replaces = 1

  res = data.frame()

  updateStorage = function(coords) {
    x = netgen::makeNetwork(coords, name = "Dummy")
    x.calc =  netgen::makeNetwork(coords * scale.factor, name = "Dummy") # for calculations we scale up

    # get features/objective and calculate key
    feats = feat.fun(x.calc)[feats.of.interest]
    obj = obj.fun(x.calc)
    obj.val = obj$obj

    # check if key exists already
    key = re::collapse(sapply(feats, function(f) sprintf("%.20f", f)), sep = ",")
    key.exists = !inherits(try({storage[key]}, silent = TRUE), "try-error")

    if (!key.exists) {
      # just add new entry
      n.keys <<- n.keys + 1L
      storage[key] = makeRecord(x, id = n.keys, n.evals, feats, feats.of.interest, obj, storage.path)
      # BUG? Why do we do it here? The algorithm has than max.evals - n.keys iteration in total?
      #n.evals <<- n.evals + 1L
    } else {
      storage[key] = updateRecord(storage[key][[1]], x, feats, obj, storage.path)
    }
    return(obj.val)
  }

  # build population and store initial stuff
  P = lapply(seq_len(mu), function(i) tspgen:::getUniformMatrix(n = n))
  objs = sapply(P, updateStorage)

  repeat {
    # catf("Evaluations %i", n.evals)

    # get random solution
    parent.idx = sample(seq_len(mu), 1L)
    coords = P[[parent.idx]]

    # mutate coordinates
    coords.new = tspgen:::applyRandomMutation(collection, coords, boundary.handling)
    obj.new = updateStorage(coords.new)

    if (obj.new <= objs[parent.idx]) {
      P[[parent.idx]] = coords.new
      objs[parent.idx] = obj.new
      n.replaces = n.replaces + 1L
    }

    if (!is.null(log.every)) {
      if (length(log.every) == 1) {
        if (((n.evals %% log.every) == 0) & (n.evals != max.evals)) {
        catf("logging in iter: %i", n.evals)
          res = rbind(res, reduceResults(storage, n.evals, n.replaces))
        }
      } else {
        if (((n.evals %in% log.every)) & (n.evals != max.evals)) {
        catf("logging in iter: %i", n.evals)
          res = rbind(res, reduceResults(storage, n.evals, n.replaces))
        }
      }
    }


    # termination coniditons
    time.passed = as.numeric(proc.time()[3L] - time.started)
    if ((n.evals >= max.evals) | (time.passed > max.time)) {
      break
    }
    n.evals = n.evals + 1L
  }

  rbind(res, reduceResults(storage, n.evals, n.replaces))
}


#' @title Quality diversity algorithm for instance generation
#'
#' @description
#' This algorithm follows the MAP-elites idea quite closely. A new instance
#' is stored in any case if its feature vector has not been seen so far.
#' An instance for mutation is chosen uniformly at random from all cells in
#' contrast to the EA where the parent is chosen from the current population
#' only.
#'
#' @param obj.fun [function(x, ...)]\cr
#'   Objective function.
#'   Must return a single numeric value.
#' @param feat.fun [function(x, ...)]\cr
#'   Feature extractor.
#'   Must return a named numeric vector.
#' @param n [integer(1)]\cr
#'   Instance size.
#' @param feats.of.interest [character]\cr
#'   Names of relevant features.
#' @param max.evals [integer(1)]\cr
#'   Maximum number of function evaluations, i.e., maximum number of TSP
#'   solver runs.
#' @param max.time [integer(1)]\cr
#'   Maximum time (in seconds).
#'   Default is \code{Inf} (i.e., no time limit).
#' @param log.every [integer(1)]\cr
#'   If not \code{NULL} the snapshot is taken every \code{log.every} iterations.
#' @param storage.path [character]\cr
#'   Path to folder where the algorithm should outsource generated instances
#'   (they are not kept in-memory due to the high number).
#' @param collection [tspgen_collection]
#'   Collection of mutation operators (see tspgen).
#' @param boundary.handling[character(1)]
#'   How to deal with out-of-bounds points? (see tspgen).
#' @param ... [any]
#'  Not used at the moment.
#' @return [data.frame] Single data frame with
#' \describe{
#'  \item{instance}{Unique consecutive ID. Instance is named instance.tsp}
#'  \item{obj}{Objective value of the instance.}
#'  \item{n.updates}{Number of times the corresponding feature combination was updated.}
#'  \item{...}{Features (see \code{feats.of.interest})}.
#' }
#' @export
qd = function(
  obj.fun, feat.fun,
  n, feats.of.interest = NULL,
  max.evals = 10L, max.time = Inf,
  log.every = NULL,
  storage.path = "qdstorage",
  collection = tspgen::init("sophisticated"), boundary.handling = "uniform",
  ...) {

  # init storage (O(1) access on average)
  storage = datastructures::hashmap("character")

  n.evals = 1L
  n.keys = 0L
  scale.factor = 1000000
  n.feats = length(feats.of.interest)
  time.started = proc.time()[3L]

  res = data.frame()

  repeat {
    # catf("Evaluations %i\n", n.evals)

    # get random solution
    keys = datastructures::keys(storage)
    if (n.keys == 0L) {
      coords = tspgen:::getUniformMatrix(n = n)
    } else {
      key = sample(keys, size = 1L)
      coords = salesperson::importFromTSPlibFormat(storage[key][[1]]$instance)$coordinates
    }

    # mutate coordinates
    coords.new = tspgen:::applyRandomMutation(collection, coords, boundary.handling)
    x = netgen::makeNetwork(coords.new, name = "Dummy")
    x.calc =  netgen::makeNetwork(coords.new * scale.factor, name = "Dummy") # for calculations we scale up

    # get features/objective and calculate key
    feats.new = feat.fun(x.calc)[feats.of.interest]
    obj.new = obj.fun(x.calc)

    # check if key exists already
    key.new = re::collapse(sapply(feats.new, function(f) sprintf("%.20f", f)), sep = ",")
    #TODO: does "datastructures" package provide a "hasKey"-function?
    key.exists = !inherits(try({storage[key.new]}, silent = TRUE), "try-error")

    if (!key.exists) {
      # just add new entry
      n.keys = n.keys + 1L
      storage[key.new] = makeRecord(x, id = n.keys, n.evals, feats.new, feats.of.interest, obj.new, storage.path)
    } else {
      storage[key.new] = updateRecord(storage[key.new][[1]], x, feats.new, obj.new, storage.path)
    }

    if (!is.null(log.every)) {
      if (length(log.every) == 1) {
        if (((n.evals %% log.every) == 0) & (n.evals != max.evals)) {
        catf("logging in iter: %i", n.evals)
          res = rbind(res, reduceResults(storage, n.evals))
        }
      } else {
        if (((n.evals %in% log.every)) & (n.evals != max.evals)) {
        catf("logging in iter: %i", n.evals)
          res = rbind(res, reduceResults(storage, n.evals))
        }
      }
    }

    # termination coniditons
    time.passed = as.numeric(proc.time()[3L] - time.started)
    if ((n.evals >= max.evals) | (time.passed > max.time)) {
      break
    }
    n.evals = n.evals + 1L
  }

  rbind(res, reduceResults(storage, n.evals))
}

makeRecord = function(instance, id, iter, feats, feats.of.interest, obj.new, storage.path) {
  fn = file.path(storage.path, paste0(id, ".tsp"))
  salesperson::exportToTSPlibFormat(instance, fn, use.extended.format = FALSE)
  list(
    instance = fn,
    feature.values = re::collapse(sapply(feats, function(f) sprintf("%.10f", f)), sep = ","),
    feature.names = re::collapse(feats.of.interest, sep = ","),
    obj = obj.new$obj,
    obj.algo.a = obj.new$algo.a,
    obj.algo.b = obj.new$algo.b,
    n.updates = 0,
    n.hits = 1,
    first.hit = iter,
    id = id,
    class = "qdstorage")
}

updateRecord = function(record, instance, feats, obj.new, storage.path) {
  record.new = record
  if (obj.new$obj < record.new$obj) {
    # overwrite existing file if obj is better
    salesperson::exportToTSPlibFormat(instance, record.new$instance, use.extended.format = FALSE)
    record.new$obj = obj.new$obj
    record.new$obj.algo.a = obj.new$algo.a
    record.new$obj.algo.b = obj.new$algo.b
    record.new$n.updates = record.new$n.updates + 1L
  }
  record.new$n.hits = record.new$n.hits + 1L
  record.new
}

reduceResults = function(storage, n.evals, n.replaces = NA) {
  keys = datastructures::keys(storage)
  res = as.data.frame(
    do.call(rbind, lapply(keys, function(key) {
      as.data.frame(storage[key][[1L]])
    })))
  res$n.evals = n.evals
  res$n.replaces = n.replaces
  return(res)
}
