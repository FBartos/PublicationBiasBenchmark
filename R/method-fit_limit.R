# Enforcement of the `fit_limit` argument of `run_method()`.
#
# Methods that spend their time inside compiled sampling code (RoBMA via JAGS,
# RTMA and MMPH via Stan) never return to R's evaluator, so `setTimeLimit()`
# cannot stop them. The fit is therefore evaluated in a separate R process that
# is killed once the limit passes, which works irrespective of what the fit is
# executing and on every platform.
#
# The process is kept alive and reused across fits: starting one costs roughly
# a second, whereas a round-trip into a running one costs tens of milliseconds.
# It is discarded whenever a fit is killed or the process dies.

# Holds the worker process between fits
.fit_session <- new.env(parent = emptyenv())


# Describe how the worker should make the package available to itself
.fit_session_loader <- function() {

  list(
    name = .packageName,
    dev  = requireNamespace("pkgload", quietly = TRUE) &&
           pkgload::is_dev_package(.packageName),
    path = getNamespaceInfo(asNamespace(.packageName), "path")
  )
}

# Return a worker ready to take a fit, starting one if needed
.fit_session_get <- function() {

  session <- .fit_session$session

  if (!is.null(session) && inherits(session, "r_session") &&
      session$get_state() %in% c("idle", "ready"))
    return(session)

  session <- callr::r_session$new()

  # attach the package once, so that individual fits only pay the round-trip
  session$run(function(loader) {
    if (loader$dev) {
      pkgload::load_all(loader$path, quiet = TRUE)
    } else {
      library(loader$name, character.only = TRUE)
    }
    invisible(TRUE)
  }, list(.fit_session_loader()))

  .fit_session$session <- session

  return(session)
}

# Kill the worker and forget it, so that the next fit starts a fresh one
.fit_session_drop <- function() {

  session <- .fit_session$session

  if (!is.null(session))
    try(session$kill(), silent = TRUE)

  .fit_session$session <- NULL

  invisible(NULL)
}


# Check whether a usable `fit_limit` was supplied
.limit_is_set <- function(fit_limit) {

  if (length(fit_limit) == 0)
    return(FALSE)

  if (length(fit_limit) != 1 || !is.numeric(fit_limit))
    stop("'fit_limit' must be a single number giving the limit in minutes")

  if (!is.finite(fit_limit))
    return(FALSE)

  if (fit_limit <= 0)
    stop("'fit_limit' must be positive")

  return(TRUE)
}

# Evaluate `method()` under a wall-clock limit (in minutes)
#
# Returns either the method's result, a "try-error" (so that `run_method()`
# reports genuine method errors as it always has), or a complete failure result
# in case the limit was exceeded or the worker died without returning anything.
.method_with_limit <- function(method_name, data, settings, silent, fit_limit) {

  failure_result <- function(note) create_empty_result(
    method_name   = method_name,
    note          = note,
    extra_columns = get_method_extra_columns(method_name)
  )

  session <- .fit_session_get()

  # The worker has its own RNG, so seed it from the calling session's stream:
  # fits stay independent of each other and reproducible from the caller's seed
  seed <- sample.int(.Machine$integer.max, 1)

  session$call(function(method_name, data, settings, silent, seed) {
    set.seed(seed)
    try(PublicationBiasBenchmark::method(method_name, data, settings), silent = silent)
  }, list(method_name, data, settings, silent, seed))

  state <- session$poll_process(fit_limit * 60 * 1000)

  # Still running once the limit passed: kill the worker outright
  if (!identical(state, "ready")) {
    .fit_session_drop()
    return(failure_result(paste0("time limit exceeded with ", format(fit_limit), " minutes")))
  }

  output <- session$read()

  # The worker died without returning a result (e.g., a crash of the sampler)
  if (!is.null(output$error)) {

    if (!session$get_state() %in% c("idle", "ready"))
      .fit_session_drop()

    return(failure_result(paste0("fit failed: ", conditionMessage(output$error))))
  }

  return(output$result)
}
