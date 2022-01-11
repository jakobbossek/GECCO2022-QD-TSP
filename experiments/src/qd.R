#' @title QD-like algorithm for instance generation
#'
#' @description
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
#' @param max.iter [integer(1)]\cr
#'   Maximum number of iterations.
#' @param max.time [integer(1)]\cr
#'   Maximum time (in seconds).
#'   Default is \code{Inf} (i.e., no time limit).
#' @param storage.path [character]\cr
#'   Path to folder where the algorithm should outsource generated instances
#'   (they are not kept in-memory due to the high number).
#' @param collection [tspgen_collection]
#'   Collection of mutation operators (see tspgen).
#' @param boundary.handline [character(1)]
#'   How to deal with out-of-bounds points? (see tspgen).
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
  max.iter = 10L, max.time = Inf,
  storage.path = "qdstorage",
  collection = tspgen::init("sophisticated"), boundary.handling = "uniform") {

  # init storage (O(1) access on average)
  storage = datastructures::hashmap("character")

  iter = 1L
  n.keys = 0L
  scale.factor = 1000000
  n.feats = length(feats.of.interest)
  time.started = proc.time()[3L]

  repeat {
    catf("Iter %i\n", iter)

    # get random solution
    keys = datastructures::keys(storage)
    if (n.keys == 0L) {
      coords = tspgen:::getUniformMatrix(n = n)
    } else {
      key = sample(keys, size = 1L)
      coords = salesperson::importFromTSPlibFormat(storage[key][[1]]$instance)$coordinates
    }

    # mutate coordinates
    coords.new = applyRandomMutation(collection, coords, boundary.handling)
    x = netgen::makeNetwork(coords.new, name = "Dummy")
    x.calc =  netgen::makeNetwork(coords.new * 1000000, name = "Dummy") # for calculations we scale up

    # get features/objective and calculate key
    feats.new = feat.fun(x.calc)[feats.of.interest]
    obj.new = obj.fun(x.calc)

    # check if key exists already
    key.new = re::collapse(sapply(feats.new, function(f) sprintf("%.10f", f)), sep = ",")
    #TODO: does "datastructures" package provide a "hasKey"-function?
    key.exists = !inherits(try({storage[key.new]}), "try-error")

    if (!key.exists) {
      # just add new entry
      n.keys = n.keys + 1L
      storage[key.new] = makeRecord(x, id = n.keys, feats.new, obj.new, storage.path)
    } else {
      storage[key.new] = updateRecord(storage[key.new][[1]], x, feats.new, obj.new, storage.path)
    }

    # termination coniditons
    time.passed = as.numeric(proc.time()[3L] - time.started)
    if ((iter >= max.iter) | (time.passed > max.time)) {
      break
    }
    iter = iter + 1L
  }

  reduceResults(storage, feats.of.interest)
}

makeRecord = function(instance, id, feats, obj.new, storage.path) {
  fn = file.path(storage.path, paste0(id, ".tsp"))
  salesperson::exportToTSPlibFormat(instance, fn, use.extended.format = FALSE)
  list(
    instance = fn,
    feats = feats,
    obj = obj.new,
    n.updates = 0,
    n.hits = 1,
    id = id,
    class = "qdstorage")
}

updateRecord = function(record, instance, feats, obj.new, storage.path) {
  record.new = record
  if (obj.new < record.new$obj) {
    # overwrite existing file if obj is better
    salesperson::exportToTSPlibFormat(instance, record.new$instance, use.extended.format = FALSE)
    record.new$obj = obj.new
    record.new$n.updates = record.new$n.updates + 1L
  }
  record.new$n.hits = record.new$n.hits + 1L
  record.new
}

reduceResults = function(storage, feats.of.interest) {
  keys = datastructures::keys(storage)
  res = as.data.frame(
    do.call(rbind, lapply(keys, function(key) {
      record = storage[key][[1L]]
      as.vector(c(record$id, record$obj, record$n.updates, record$n.hits, unlist(record$feats)))
    })))
  colnames(res) = c("instance", "obj", "n.updates", "n.hits", feats.of.interest)
  return(res)
}
