test_resource <- function(directory, filename, kind = "results", method = "A", ids = 1:2,
                          dgm = "no_bias", condition = 1L) {
  path <- file.path(directory, filename)
  data <- data.frame(method = method, method_setting = "default", condition_id = condition,
                     repetition_id = ids, estimate = seq_along(ids), stringsAsFactors = FALSE)
  if (kind == "data") data <- data.frame(repetition_id = rep(ids, each = 2), yi = 1, vi = .1)
  if (kind == "measures") data <- data.frame(method = method, method_setting = "default",
    condition_id = condition, bias = .1, bias_mcse = .01, n_valid_bias = 2L)
  utils::write.csv(data, path, row.names = FALSE)
  asset <- benchmark_resource(path, dgm, kind,
    method = if (kind == "data") NULL else method,
    method_setting = if (kind == "data") NULL else "default",
    condition_ids = condition, package_version = "0.4.0",
    measures = if (kind == "measures") "bias" else NULL)
  asset$record_id <- "12345"
  asset
}

test_catalog <- function(assets, release = "test.1") list(schema_version = 1L, release = release,
  assets = assets, conditions = list(no_bias = data.frame(condition_id = 1:2, mean_effect = 0)), sandbox = FALSE)

legacy_plan_benchmark_release <- function(...) plan_benchmark_release(..., archive = FALSE)

# Mock the community gate (it is the first network action of a publication
# session). Returns an environment that records every call.
local_mock_gate <- function(env = parent.frame(), community_id = "community-uuid") {
  seen <- new.env(); seen$calls <- list()
  testthat::local_mocked_bindings(
    .require_community_maintainer = function(community, token, sandbox, roles) {
      seen$calls[[length(seen$calls) + 1L]] <- list(community = community, sandbox = sandbox, roles = roles)
      community_id
    }, .env = env)
  seen
}

# Hold the publication lock of a plan's state directory for this test, as a
# running session does, for tests of workers that delete draft files.
local_held_lock <- function(plan, env = parent.frame()) {
  .acquire_publication_lock(plan$state_directory)
  withr::defer(.release_publication_lock(plan$state_directory), envir = env)
  invisible(plan)
}

zenodo_error <- function(status, message = "request rejected") structure(
  list(message = paste0("Zenodo request failed (HTTP ", status, "): ", message), call = NULL, status = status, errors = NULL),
  class = c("zenodo_http_error", "error", "condition"))

# A scripted Zenodo community API for the gate. Every request is logged; anything
# but a GET stops the test (and stays visible in the log).
community_server <- function(community_id = "community-uuid", members = NULL, user_communities = NULL) {
  log <- new.env(); log$requests <- list()
  handler <- function(method, path, token, sandbox = FALSE, body = NULL) {
    log$requests[[length(log$requests) + 1L]] <- list(method = method, path = path, token = token, sandbox = sandbox)
    if (method != "GET") stop("Unexpected write request: ", method, " ", path)
    if (grepl("^communities/[^/?]+$", path)) return(list(id = community_id))
    if (grepl("^communities/[^/]+/members[?]", path)) {
      page <- as.integer(sub("^.*page=", "", path))
      return(if (is.function(members)) members(page) else stop(zenodo_error(403L)))
    }
    if (identical(path, "user/communities?size=1"))
      return(if (is.function(user_communities)) user_communities() else stop(zenodo_error(403L)))
    stop("Unexpected GET ", path)
  }
  list(handler = handler, log = log,
       methods = function() vapply(log$requests, `[[`, character(1), "method"),
       paths = function() vapply(log$requests, `[[`, character(1), "path"))
}
member_hit <- function(role, current = TRUE) list(role = role, is_current_user = current, member = list(type = "user"))
members_page <- function(hits, more = FALSE) list(hits = list(hits = hits),
  links = if (more) list(self = "page", `next` = "page") else list(self = "page"))

http_failure <- function(status, retry_delay = NULL) structure(list(
  message = paste0("Public resource download failed (HTTP ", status, ")."), call = NULL,
  status = status, retry_delay = retry_delay), class = c("resource_http_error", "error", "condition"))
curl_failure <- function(class, message = "transfer failed") structure(list(message = message, call = NULL),
  class = c(class, "curl_error", "error", "condition"))
