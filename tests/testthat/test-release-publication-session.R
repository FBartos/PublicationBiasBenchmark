## The community gate ------------------------------------------------------------

test_that("the gate returns the UUID for an owner or manager and only reads", {
  for (role in c("owner", "manager")) {
    server <- community_server(.benchmark_community_id, members = function(page) members_page(list(member_hit(role))))
    local_mocked_bindings(.zenodo_request = server$handler)
    expect_identical(.require_community_maintainer("publicationbiasbenchmark", "secret-token", FALSE, c("owner", "manager")),
                     "415b2de6-b6f9-444d-9109-d74752e20cd0")
    expect_identical(server$paths(), c("communities/publicationbiasbenchmark",
      "communities/415b2de6-b6f9-444d-9109-d74752e20cd0/members?size=100&page=1"))
    expect_true(all(server$methods() == "GET"))
    expect_true(all(vapply(server$log$requests, function(r) identical(r$token, "secret-token") && !r$sandbox, logical(1))))
  }
  expect_identical(.benchmark_community_id, "415b2de6-b6f9-444d-9109-d74752e20cd0")
})

test_that("the gate refuses roles outside the allowed set and odd role values", {
  for (role in list("reader", "curator", NULL, 1L, c("owner", "reader"), NA_character_, "")) {
    server <- community_server(.benchmark_community_id, members = function(page) members_page(list(member_hit(role))))
    local_mocked_bindings(.zenodo_request = server$handler)
    expect_error(.require_community_maintainer("c", "t", FALSE, c("owner", "manager")), "required")
  }
  # Community pages need the owner: a manager is refused there.
  server <- community_server(.benchmark_community_id, members = function(page) members_page(list(member_hit("manager"))))
  local_mocked_bindings(.zenodo_request = server$handler)
  expect_error(.require_community_maintainer("c", "t", FALSE, "owner"), "role 'manager'")
})

test_that("the gate separates non-members from tokens that Zenodo does not accept", {
  server <- community_server(.benchmark_community_id, user_communities = function() list(hits = list(hits = list())))
  local_mocked_bindings(.zenodo_request = server$handler)
  expect_error(.require_community_maintainer("c", "t", FALSE, "owner"), "not a member")
  expect_identical(tail(server$paths(), 1L), "user/communities?size=1")
  # An invalid, expired or other-environment token is treated as anonymous: both calls answer 403.
  server <- community_server(.benchmark_community_id)
  local_mocked_bindings(.zenodo_request = server$handler)
  expect_error(.require_community_maintainer("c", "t", FALSE, "owner"),
               "token invalid, expired, or for the other environment (sandbox vs production)", fixed = TRUE)
  # Any other status or shape fails closed.
  for (status in c(400L, 404L, 429L, 500L)) {
    server <- community_server(.benchmark_community_id, members = function(page) stop(zenodo_error(status)))
    local_mocked_bindings(.zenodo_request = server$handler)
    expect_error(.require_community_maintainer("c", "t", FALSE, "owner"), paste0("HTTP ", status))
  }
  server <- community_server(.benchmark_community_id, user_communities = function() stop(zenodo_error(500L)))
  local_mocked_bindings(.zenodo_request = server$handler)
  expect_error(.require_community_maintainer("c", "t", FALSE, "owner"), "Cannot check the Zenodo token")
  server <- community_server(.benchmark_community_id, members = function(page) list(hits = "odd"))
  local_mocked_bindings(.zenodo_request = server$handler)
  expect_error(.require_community_maintainer("c", "t", FALSE, "owner"), "Unexpected community membership response")
})

test_that("the gate refuses a production community other than the benchmark community before any member request", {
  server <- community_server("another-community-uuid", members = function(page) members_page(list(member_hit("owner"))))
  local_mocked_bindings(.zenodo_request = server$handler)
  expect_error(.require_community_maintainer("someone-else", "t", FALSE, "owner"),
               "production publication is restricted to the PublicationBiasBenchmark community", fixed = TRUE)
  expect_identical(server$paths(), "communities/someone-else")
  # The sandbox accepts any community the token maintains.
  expect_identical(.require_community_maintainer("someone-else", "t", TRUE, "owner"), "another-community-uuid")
  # A failed lookup or a missing UUID stops as well.
  local_mocked_bindings(.zenodo_request = function(...) stop(zenodo_error(404L)))
  expect_error(.require_community_maintainer("c", "t", TRUE, "owner"), "Cannot verify the community")
  local_mocked_bindings(.zenodo_request = function(...) list(slug = "no-id"))
  expect_error(.require_community_maintainer("c", "t", TRUE, "owner"), "did not return a UUID")
  expect_error(.require_community_maintainer(NULL, "t", TRUE, "owner"), "community slug or UUID")
})

test_that("the gate pages through the members up to a cap", {
  pages <- integer()
  server <- community_server(.benchmark_community_id, members = function(page) {
    pages <<- c(pages, page)
    if (page < 3L) members_page(list(member_hit("reader", FALSE)), more = TRUE) else members_page(list(member_hit("owner")))
  })
  local_mocked_bindings(.zenodo_request = server$handler)
  expect_identical(.require_community_maintainer("c", "t", FALSE, "owner"), .benchmark_community_id)
  expect_identical(pages, 1:3)
  pages <- integer()
  server <- community_server(.benchmark_community_id, members = function(page) {
    pages <<- c(pages, page); members_page(list(member_hit("reader", FALSE)), more = TRUE)
  })
  local_mocked_bindings(.zenodo_request = server$handler)
  expect_error(.require_community_maintainer("c", "t", FALSE, "owner"), "more members than the gate can page")
  expect_identical(pages, 1:50)
  # The last page without the current user is not enough either.
  server <- community_server(.benchmark_community_id, members = function(page) members_page(list(member_hit("owner", FALSE))))
  local_mocked_bindings(.zenodo_request = server$handler)
  expect_error(.require_community_maintainer("c", "t", FALSE, "owner"), "not found among the community members")
})

## The publication session ------------------------------------------------------

# Plans of both schemas for the same files; the state directory lives in `root`.
session_plans <- function(root, community = "publicationbiasbenchmark", sandbox = FALSE) {
  a <- test_resource(root, "A.csv")
  conditions <- test_catalog(list(a))$conditions
  metadata <- list(rights = list(list(id = "cc-by-4.0")))
  list(legacy = plan_benchmark_release("test.1", list(a), conditions = conditions, metadata = metadata, archive = FALSE,
         state_directory = file.path(root, "legacy"), community = community, sandbox = sandbox),
       archive = if (requireNamespace("zip", quietly = TRUE)) plan_benchmark_release("test.1", list(a), conditions = conditions,
         metadata = metadata, state_directory = file.path(root, "archive"), community = community, sandbox = sandbox))
}

# Record every request and upload; none of them may be a write when a session is refused.
local_write_recorder <- function(handler, env = parent.frame()) {
  writes <- new.env(); writes$log <- character()
  testthat::local_mocked_bindings(
    .zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
      if (method != "GET") writes$log <- c(writes$log, paste(method, path))
      handler(method, path, token, sandbox, body)
    },
    .zenodo_upload = function(path, record_id, filename, ...) writes$log <- c(writes$log, paste("UPLOAD", filename)),
    .env = env)
  writes
}

test_that("publishing needs the release identifier as confirm before anything else happens", {
  root <- withr::local_tempdir(); plans <- session_plans(root)
  for (plan in Filter(Negate(is.null), plans)) {
    server <- community_server(.benchmark_community_id, members = function(page) members_page(list(member_hit("owner"))))
    writes <- local_write_recorder(server$handler)
    for (confirm in list(NULL, "wrong", "test.2", NA_character_, "", c("test.1", "test.1"), 1, TRUE)) {
      expect_error(publish_benchmark_release(plan, "t", confirm = confirm),
                   "Publishing is irreversible: run PublicationBiasBenchmark:::verify_benchmark_release(plan) and re-run with confirm = \"test.1\"",
                   fixed = TRUE)
    }
    expect_length(server$log$requests, 0L); expect_length(writes$log, 0L)
    expect_false(dir.exists(file.path(plan$state_directory, ".lock")))
  }
})

test_that("a refused session issues no write: non-member, other environment, foreign production community", {
  root <- withr::local_tempdir(); plans <- Filter(Negate(is.null), session_plans(root))
  scenarios <- list(
    non_member = list(server = community_server(.benchmark_community_id, user_communities = function() list(hits = list(hits = list()))),
                      message = "not a member"),
    other_environment = list(server = community_server(.benchmark_community_id), message = "other environment"),
    foreign_community = list(server = community_server("foreign-uuid", members = function(page) members_page(list(member_hit("owner")))),
                             message = "restricted to the PublicationBiasBenchmark community"),
    reader = list(server = community_server(.benchmark_community_id, members = function(page) members_page(list(member_hit("reader")))),
                  message = "required"))
  for (plan in plans) for (name in names(scenarios)) {
    scenario <- scenarios[[name]]
    writes <- local_write_recorder(scenario$server$handler)
    expect_error(stage_benchmark_release(plan, "t"), scenario$message, info = name)
    expect_error(publish_benchmark_release(plan, "t", confirm = "test.1"), scenario$message, info = name)
    expect_length(writes$log, 0L)
    expect_true(all(scenario$server$methods() == "GET"), info = name)
    expect_false(dir.exists(file.path(plan$state_directory, ".lock")), info = name)
  }
})

test_that("the session checks the lock and the plan's state before the gate", {
  root <- withr::local_tempdir(); plans <- Filter(Negate(is.null), session_plans(root))
  gate <- local_mock_gate()
  for (plan in plans) {
    # A lock held by another session stops it before any network action.
    lock <- file.path(plan$state_directory, ".lock"); dir.create(lock)
    expect_error(stage_benchmark_release(plan, "t"), "is locked")
    expect_length(gate$calls, 0L); expect_true(dir.exists(lock))
    unlink(lock, recursive = TRUE)
    # A plan from an earlier package version has no identity; one without a community cannot be gated.
    old <- plan; old$identity <- NULL
    expect_error(stage_benchmark_release(old, "t"), "earlier package version; re-plan in a new state directory")
    nameless <- plan; nameless$community <- NULL
    expect_error(stage_benchmark_release(nameless, "t"), "has no community; re-plan with this package version")
    expect_length(gate$calls, 0L)
    expect_false(dir.exists(lock))
    # A state written for another plan blocks the session.
    other <- plan; other$identity$fingerprint <- strrep("0", 64)
    .update_publication_state(other, function(state) { state$groups[["1"]] <- list(record_id = "1"); state })
    expect_error(stage_benchmark_release(plan, "t"), "belongs to a different plan")
    expect_length(gate$calls, 0L)
  }
})

test_that("a session passes the token and the gate's UUID on, with the roles of its operation", {
  root <- withr::local_tempdir(); plan <- session_plans(root)$legacy
  gate <- local_mock_gate(community_id = "resolved-uuid")
  seen <- NULL
  expect_identical(.publication_session(plan, "explicit-token", c("owner"), fn = function(token, community_id) {
    seen <<- list(token = token, community_id = community_id, held = dir.exists(file.path(plan$state_directory, ".lock")))
    "result"
  }), "result")
  expect_identical(seen, list(token = "explicit-token", community_id = "resolved-uuid", held = TRUE))
  expect_identical(gate$calls[[1]]$roles, "owner"); expect_identical(gate$calls[[1]]$community, "publicationbiasbenchmark")
  expect_false(gate$calls[[1]]$sandbox)
  # The lock is released when the work fails.
  expect_error(.publication_session(plan, "t", "owner", fn = function(token, community_id) stop("work failed")), "work failed")
  expect_false(dir.exists(file.path(plan$state_directory, ".lock")))
  # The token comes from the environment of the plan's side when not given.
  withr::local_envvar(ZENODO_TOKEN = "from-environment")
  expect_identical(.publication_session(plan, NULL, "owner", fn = function(token, community_id) token), "from-environment")
  withr::local_envvar(ZENODO_TOKEN = "")
  expect_error(.publication_session(plan, NULL, "owner", fn = function(token, community_id) token), "token is not configured")
})

test_that("staging and publishing use the release roles", {
  expect_identical(.release_roles, c("owner", "manager"))
  root <- withr::local_tempdir(); plan <- session_plans(root)$legacy
  gate <- local_mock_gate()
  local_mocked_bindings(.stage_legacy_release = function(plan, token) "staged", .publish_legacy_release = function(plan, token) "published")
  expect_identical(stage_benchmark_release(plan, "t"), "staged")
  expect_identical(publish_benchmark_release(plan, "t", confirm = "test.1"), "published")
  expect_identical(gate$calls[[1]]$roles, c("owner", "manager")); expect_identical(gate$calls[[2]]$roles, c("owner", "manager"))
})

## The publication lock -----------------------------------------------------------

test_that("the lock is atomic, names its owner, and is released only with the matching nonce", {
  directory <- file.path(withr::local_tempdir(), "nested", "state")
  .acquire_publication_lock(directory)
  lock <- file.path(directory, ".lock")
  owner <- jsonlite::read_json(file.path(lock, "owner.json"))
  expect_setequal(names(owner), c("pid", "host", "started", "nonce"))
  expect_equal(owner$pid, Sys.getpid()); expect_match(owner$nonce, "^[a-f0-9]{64}$")
  expect_match(owner$started, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:]{8}Z$")
  expect_error(.acquire_publication_lock(directory), "is locked")
  expect_error(.acquire_publication_lock(directory), normalizePath(lock, winslash = "/"), fixed = TRUE)
  expect_error(.acquire_publication_lock(directory), paste0("pid ", Sys.getpid()), fixed = TRUE)
  expect_true(.release_publication_lock(directory))
  expect_false(dir.exists(lock))
  # Another session's lock, or a lock whose owner file was replaced, is not removed.
  .acquire_publication_lock(directory)
  jsonlite::write_json(list(pid = 1L, host = "other", started = .utc_now(), nonce = "foreign"), file.path(lock, "owner.json"),
                       auto_unbox = TRUE)
  expect_warning(.release_publication_lock(directory), "no longer belongs to this session")
  expect_true(dir.exists(lock))
  expect_false(.release_publication_lock(directory))   # nothing is held any more
  unlink(lock, recursive = TRUE)
})

test_that("a stale lock is reported with its owner and age but never removed automatically", {
  directory <- withr::local_tempdir(); lock <- file.path(directory, ".lock"); dir.create(lock)
  started <- format(Sys.time() - 3 * 3600, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  jsonlite::write_json(list(pid = 4242L, host = "maintainer-pc", started = started, nonce = "n"), file.path(lock, "owner.json"),
                       auto_unbox = TRUE)
  expect_error(.acquire_publication_lock(directory), "pid 4242 on maintainer-pc")
  expect_error(.acquire_publication_lock(directory), "1(79|8[0-2]) minutes ago")
  expect_error(.acquire_publication_lock(directory), "only if that session is not running")
  expect_error(.acquire_publication_lock(directory), normalizePath(lock, winslash = "/"), fixed = TRUE)
  expect_true(dir.exists(lock))
  unlink(file.path(lock, "owner.json"))
  expect_error(.acquire_publication_lock(directory), "owner unknown")
  # A lock that cannot be created is a different failure.
  blocker <- withr::local_tempfile(); writeLines("a file", blocker)
  expect_error(.acquire_publication_lock(file.path(blocker, "state")), "Cannot create the lock")
})

test_that("a failed lock release warns and the lock never leaks into the next session", {
  skip_if_not(.Platform$OS.type == "windows", "an open file only blocks removal on Windows")
  directory <- withr::local_tempdir()
  .acquire_publication_lock(directory)
  connection <- file(file.path(directory, ".lock", "busy.txt"), "w")
  expect_warning(.release_publication_lock(directory), "Could not remove the publication lock")
  close(connection)
  unlink(file.path(directory, ".lock"), recursive = TRUE)
})

test_that("planning takes the lock for both plan schemas", {
  root <- withr::local_tempdir(); a <- test_resource(root, "A.csv")
  conditions <- test_catalog(list(a))$conditions
  for (archive in c(FALSE, TRUE)) {
    skip_if(archive && !requireNamespace("zip", quietly = TRUE))
    state <- file.path(root, paste0("state-", archive)); dir.create(file.path(state, ".lock"), recursive = TRUE)
    expect_error(plan_benchmark_release("test.1", list(a), conditions = conditions, metadata = list(), state_directory = state,
      archive = archive, community = "test-community"), "is locked", info = archive)
    expect_false(file.exists(file.path(state, "plan.rds")))
    unlink(file.path(state, ".lock"), recursive = TRUE)
    plan <- plan_benchmark_release("test.1", list(a), conditions = conditions, metadata = list(), state_directory = state,
      archive = archive, community = "test-community")
    expect_true(file.exists(file.path(state, "plan.rds"))); expect_false(dir.exists(file.path(state, ".lock")))
    expect_identical(plan$community, "test-community")
  }
  expect_error(plan_benchmark_release("test.1", list(a), conditions = conditions, metadata = list(),
    state_directory = file.path(root, "none"), archive = FALSE, community = NULL), "community")
})

## Plan identity and publication state -------------------------------------------

test_that("plan identity binds the state to the release, community, environment, contents and limits", {
  root <- withr::local_tempdir()
  a <- test_resource(root, "A.csv"); b <- test_resource(root, "B.csv", method = "B")
  conditions <- test_catalog(list(a))$conditions
  make <- function(files = list(a), dir = withr::local_tempdir(), ...) {
    args <- list(release = "test.1", files = files, conditions = conditions, metadata = list(), state_directory = dir,
                 archive = FALSE, community = "c")
    do.call(plan_benchmark_release, modifyList(args, list(...)))
  }
  plan <- make()
  identity <- plan$identity
  expect_setequal(names(identity), c("version", "release", "sandbox", "community", "fingerprint"))
  expect_identical(identity$version, 2L); expect_identical(identity$release, "test.1")
  expect_false(identity$sandbox); expect_identical(identity$community, "c")
  expect_match(identity$fingerprint, "^[a-f0-9]{64}$")
  # The state directory is not part of the identity; everything else is.
  expect_identical(make()$identity, identity)
  expect_false(identical(make(list(a, b))$identity$fingerprint, identity$fingerprint))
  expect_false(identical(make(community = "d")$identity, identity))
  expect_false(identical(make(sandbox = TRUE)$identity, identity))
  expect_false(identical(make(max_files = 10L)$identity$fingerprint, identity$fingerprint))
  expect_false(identical(make(max_bytes = 1e6)$identity$fingerprint, identity$fingerprint))
  expect_false(identical(make(release = "test.2")$identity$release, "test.1"))
  expect_true(.identity_equal(identity, jsonlite::fromJSON(.canonical_json(identity), simplifyVector = FALSE)))
  # The fingerprint is canonical JSON, not R serialization.
  expect_identical(.canonical_fingerprint(list(a = 1, b = "x")), digest::digest('{"a":1,"b":"x"}', algo = "sha256", serialize = FALSE))
  # A saved plan from the same inputs is recognised; changed inputs are not.
  directory <- withr::local_tempdir()
  make(dir = directory)
  expect_identical(make(dir = directory)$identity, identity)
  expect_error(make(list(a, b), dir = directory), "belongs to a different plan")
  expect_error(make(dir = directory, community = "other"), "belongs to a different plan")
  saved <- readRDS(file.path(directory, "plan.rds")); saved$identity <- NULL
  saveRDS(saved, file.path(directory, "plan.rds"))
  expect_error(make(dir = directory), "earlier package version; re-plan in a new state directory")
})

test_that("an archive plan's resume fingerprint is canonical and its state identity follows the membership", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); originals <- withr::local_tempdir()
  a <- test_resource(originals, "A.csv"); b <- test_resource(originals, "B.csv", method = "B")
  conditions <- test_catalog(list(a))$conditions
  make <- function(files, dir, ...) do.call(plan_benchmark_release, modifyList(list(release = "test.1", files = files,
    conditions = conditions, metadata = list(rights = list(list(id = "cc-by-4.0"))), state_directory = dir, community = "c"),
    list(...)))
  plan <- make(list(a, b), file.path(root, "one"))
  expect_match(plan$fingerprint, "^[a-f0-9]{64}$")
  expect_identical(make(list(a, b), file.path(root, "two"))$fingerprint, plan$fingerprint)
  expect_identical(make(list(a, b), file.path(root, "one"))$identity, plan$identity)
  expect_false(identical(make(list(a), file.path(root, "three"))$fingerprint, plan$fingerprint))
  expect_false(identical(make(list(a, b), file.path(root, "four"), community = "d")$fingerprint, plan$fingerprint))
  expect_false(identical(make(list(a, b), file.path(root, "five"), max_archive_bytes = 1e6)$fingerprint, plan$fingerprint))
  expect_false(identical(make(list(a, b), file.path(root, "six"), max_files = 50L)$fingerprint, plan$fingerprint))
  expect_error(make(list(a), file.path(root, "one")), "belongs to a different plan")
  saved <- readRDS(file.path(root, "one", "plan.rds")); saved$identity <- NULL
  saveRDS(saved, file.path(root, "one", "plan.rds"))
  expect_error(make(list(a, b), file.path(root, "one")), "earlier package version; re-plan in a new state directory")
})

test_that("state belongs to its plan: writes need the identity, read-only checks tolerate its absence", {
  root <- withr::local_tempdir(); plan <- session_plans(root)$legacy
  expect_identical(.publication_state(plan, "write"), list(groups = list(), catalog_record = NULL))
  .update_publication_state(plan, function(state) { state$groups[["1"]] <- list(record_id = "7"); state })
  state <- .publication_state(plan)
  expect_true(.identity_equal(state$identity, plan$identity))
  expect_identical(state$groups[["1"]]$record_id, "7")
  other <- plan; other$identity$release <- "test.9"
  expect_error(.publication_state(other, "write"), "belongs to a different plan")
  expect_error(.publication_state(other, "read"), "belongs to a different plan")
  expect_error(.update_publication_state(other, function(state) state), "belongs to a different plan")
  # State written by an earlier package version has no identity.
  path <- file.path(plan$state_directory, "state.json")
  legacy_state <- jsonlite::read_json(path); legacy_state$identity <- NULL
  jsonlite::write_json(legacy_state, path, auto_unbox = TRUE)
  expect_error(.publication_state(plan, "write"), "written by an earlier package version; re-plan in a new state directory")
  expect_error(.update_publication_state(plan, function(state) state), "earlier package version")
  expect_message(restored <- .publication_state(plan, "read"), "no plan identity")
  expect_identical(restored$groups[["1"]]$record_id, "7")
})

test_that("state structure is validated for the known fields only", {
  root <- withr::local_tempdir(); plan <- list(state_directory = root, sandbox = TRUE)
  path <- file.path(root, "state.json")
  write_state <- function(text) writeLines(text, path)
  write_state('{"groups":[],"versions":{},"inclusions":{},"initialized":[],"catalog_record":null,"future_field":{"a":1}}')
  expect_silent(.publication_state(plan))
  write_state('{"groups":{"1":{"record_id":"1"}},"catalog_record":{"record_id":"2"},"unknown":[1,2]}')
  expect_silent(.publication_state(plan))
  for (broken in c('{"groups":"x"}', '{"versions":[1,2]}', '{"inclusions":3}', '{"initialized":[{"a":1}]}',
                   '{"catalog_record":"x"}', '[1,2,3]', '{broken', '')) {
    write_state(broken)
    expect_error(.publication_state(plan), "unreadable or has an invalid structure", info = broken)
  }
  # Unreadable state or a missing state with history names the newest history file; nothing is rewritten.
  history <- file.path(root, "state-history"); dir.create(history)
  writeLines("{}", file.path(history, "20260101T000000.000001-0001-state.json"))
  writeLines("{}", file.path(history, "20260101T000000.000002-0002-state.json"))
  write_state("{broken")
  expect_error(.publication_state(plan), "20260101T000000.000002-0002-state.json")
  expect_error(.publication_state(plan), "Nothing was changed online")
  expect_identical(readLines(path), "{broken")
  unlink(path)
  expect_error(.publication_state(plan), "is missing although history exists")
  expect_error(.publication_state(plan), "20260101T000000.000002-0002-state.json")
  unlink(history, recursive = TRUE)
  expect_identical(.publication_state(plan), list(groups = list(), catalog_record = NULL))
  write_state("{broken")
  expect_error(.publication_state(plan), "No state history exists")
})

test_that("state updates re-read the state, so no update is lost to a stale copy", {
  root <- withr::local_tempdir(); plan <- list(state_directory = root, sandbox = TRUE)
  stale <- .publication_state(plan)
  .update_publication_state(plan, function(state) { state$groups[["1"]] <- list(record_id = "1"); state })
  .update_publication_state(plan, function(state) { state$versions[["catalog"]] <- list(record_id = "9"); state })
  .update_publication_state(plan, function(state) { state$groups[["2"]] <- list(record_id = "2"); state })
  state <- .publication_state(plan)
  expect_setequal(names(state$groups), c("1", "2")); expect_identical(state$versions$catalog$record_id, "9")
  expect_error(.update_publication_state(plan, function(state) NULL), "must return the new state")
  expect_length(list.files(file.path(root, "state-history")), 3L)
  expect_false(any(grepl("[.]previous$|[.]tmp$", list.files(root, recursive = TRUE))))
  expect_null(stale$versions)
})

## Verified writes -------------------------------------------------------------------

test_that("a verified write is skipped when unchanged and keeps a numbered history of every version", {
  root <- withr::local_tempdir(); path <- file.path(root, "file.json"); history <- file.path(root, "history")
  expect_true(.write_json_verified(path, list(a = 1), history_dir = history))
  expect_false(.write_json_verified(path, list(a = 1), history_dir = history))
  expect_length(list.files(history), 1L)
  expect_true(.write_json_verified(path, list(a = 2), history_dir = history))
  expect_true(.write_json_verified(path, list(a = 3), history_dir = history))
  entries <- list.files(history)
  expect_length(entries, 3L)
  expect_match(entries, "^[0-9]{8}T[0-9]{6}[.][0-9]{6}-000[123]-file[.]json$")
  expect_identical(substr(entries, 24, 27), sprintf("%04d", 1:3))
  expect_identical(entries, sort(entries))
  content <- vapply(file.path(history, entries), function(f) rawToChar(readBin(f, "raw", file.info(f)$size)), character(1), USE.NAMES = FALSE)
  expect_identical(content, c('{"a":1}\n', '{"a":2}\n', '{"a":3}\n'))
  expect_identical(rawToChar(.read_bytes(path)), '{"a":3}\n')
  # Files are LF-terminated whatever the platform; no temporary file remains.
  bytes <- .read_bytes(path)
  expect_false(as.raw(13) %in% bytes); expect_identical(bytes[length(bytes)], as.raw(10))
  expect_identical(list.files(root), c("file.json", "history"))
})

test_that("a file written by older code is added to the history before it is replaced", {
  root <- withr::local_tempdir(); path <- file.path(root, "file.json"); history <- file.path(root, "history")
  writeBin(charToRaw("{\"old\":true}\r\n"), path)
  .write_json_verified(path, list(new = TRUE), history_dir = history)
  entries <- list.files(history, full.names = TRUE)
  expect_length(entries, 2L)
  expect_identical(rawToChar(.read_bytes(entries[1])), "{\"old\":true}\r\n")
  expect_identical(rawToChar(.read_bytes(entries[2])), "{\"new\":true}\n")
  # An old file that already is the newest history entry is not added again.
  .write_json_verified(path, list(newer = TRUE), history_dir = history)
  expect_length(list.files(history), 3L)
})

test_that("history entries are verified copies and a failing copy stops before anything is replaced", {
  root <- withr::local_tempdir(); path <- file.path(root, "file.json"); history <- file.path(root, "history")
  .write_json_verified(path, list(a = 1), history_dir = history)
  waits <- numeric(); attempts <- 0L
  local_mocked_bindings(.file_retry_wait = function(seconds) waits <<- c(waits, seconds),
    .file_copy = function(from, to) { attempts <<- attempts + 1L; if (attempts < 3L) FALSE else file.copy(from, to) })
  expect_true(.write_json_verified(path, list(a = 2), history_dir = history))
  expect_identical(attempts, 3L); expect_identical(waits, c(0.2, 0.2))
  local_mocked_bindings(.file_copy = function(from, to) FALSE)
  waits <- numeric()
  expect_error(.write_json_verified(path, list(a = 3), history_dir = history), "history copy of file.json.*nothing was replaced")
  expect_length(waits, 4L)
  expect_identical(rawToChar(.read_bytes(path)), '{"a":2}\n')
})

test_that("a failing rename keeps the old file, the complete new copy and the history", {
  root <- withr::local_tempdir(); path <- file.path(root, "file.json"); history <- file.path(root, "history")
  .write_json_verified(path, list(a = 1), history_dir = history)
  renames <- 0L; waits <- numeric()
  local_mocked_bindings(.file_rename = function(from, to) { renames <<- renames + 1L; FALSE },
                        .file_retry_wait = function(seconds) waits <<- c(waits, seconds))
  expect_error(.write_json_verified(path, list(a = 2), history_dir = history), "Cannot replace .*file.json")
  expect_identical(renames, 5L); expect_identical(waits, rep(0.2, 4))
  expect_identical(rawToChar(.read_bytes(path)), '{"a":1}\n')
  leftovers <- list.files(root, pattern = "[.]tmp$", full.names = TRUE)
  expect_length(leftovers, 1L); expect_identical(rawToChar(.read_bytes(leftovers)), '{"a":2}\n')
  expect_length(list.files(history), 2L)
})

test_that("a failed verification restores the previous version, or removes a first version", {
  root <- withr::local_tempdir(); path <- file.path(root, "file.json"); history <- file.path(root, "history")
  expect_error(.write_json_verified(path, list(a = 1), history_dir = history, verify = function(p) FALSE),
               "failed verification; the previous version was restored")
  expect_false(file.exists(path))
  .write_json_verified(path, list(a = 1), history_dir = history)
  expect_error(.write_json_verified(path, list(a = 2), history_dir = history, verify = function(p) stop("does not validate")),
               "failed verification \\(does not validate\\); the previous version was restored")
  expect_identical(rawToChar(.read_bytes(path)), '{"a":1}\n')
  expect_false(any(grepl("[.]tmp$", list.files(root))))
  # Nothing is verified for a skipped write, and the verifier sees the installed path.
  seen <- NULL
  expect_true(.write_json_verified(path, list(a = 3), history_dir = history, verify = function(p) { seen <<- p; TRUE }))
  expect_identical(seen, path)
  expect_false(.write_json_verified(path, list(a = 3), history_dir = history, verify = function(p) stop("not called")))
  # Data that is not JSON never reaches the disk.
  expect_error(.write_json_verified(path, list(a = new.env())))
  expect_identical(rawToChar(.read_bytes(path)), '{"a":3}\n')
})

test_that("a failed restore after a failed verification names the file that holds the previous bytes", {
  root <- withr::local_tempdir(); path <- file.path(root, "plan.rds")
  .write_file_verified(path, charToRaw("old bytes"))
  renames <- 0L
  # The new file is installed; every later rename (the restore) fails.
  local_mocked_bindings(.file_retry_wait = function(seconds) NULL,
    .file_rename = function(from, to) { renames <<- renames + 1L; renames == 1L && file.rename(from, to) })
  message <- tryCatch(.write_file_verified(path, charToRaw("new bytes"), verify = function(p) FALSE),
                      error = conditionMessage)
  expect_match(message, "failed verification; restoring the previous version failed", fixed = TRUE)
  expect_match(message, paste0(path, " holds the unverified new bytes"), fixed = TRUE)
  previous <- list.files(root, pattern = "^plan[.]rds-previous-.*[.]tmp$", full.names = TRUE)
  expect_length(previous, 1L)
  expect_identical(rawToChar(.read_bytes(previous)), "old bytes")
  expect_match(message, "the previous bytes are in ", fixed = TRUE)
  expect_match(message, basename(previous), fixed = TRUE)
  expect_false(grepl("history", message))
  # With a history, the message also points to it.
  history <- file.path(root, "history"); renames <- 0L
  expect_true(.write_file_verified(file.path(root, "state.json"), charToRaw("{}"), history_dir = history))
  renames <- 0L
  expect_error(.write_file_verified(file.path(root, "state.json"), charToRaw("{\"a\":1}"), verify = function(p) FALSE,
                                    history_dir = history), paste0("the history in ", history, " keeps every version"), fixed = TRUE)
})

test_that("RDS files are written verified and unchanged files are skipped", {
  root <- withr::local_tempdir(); path <- file.path(root, "plan.rds")
  object <- list(data = data.frame(x = 1:3), label = "plan")
  expect_true(.write_rds_verified(path, object))
  expect_false(.write_rds_verified(path, object))
  expect_identical(readRDS(path), object)
  expect_true(.write_rds_verified(path, c(object, list(more = TRUE))))
  expect_identical(readRDS(path)$more, TRUE)
  expect_identical(list.files(root), "plan.rds")
})

test_that("catalogs are verified by reading them back, with the publication block intact", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); a <- test_resource(root, "A.csv")
  plan <- plan_benchmark_release("test.1", list(a), conditions = test_catalog(list(a))$conditions, metadata = list(),
    state_directory = file.path(root, "state"), community = "c")
  catalog <- .public_catalog(plan$catalog)
  catalog$publication <- list(catalog_record_id = "5", catalog_concept_doi = "10.test/c", community_id = "uuid",
                              storage_dois = list(no_bias = "10.test/1"))
  path <- file.path(plan$state_directory, "release.json")
  .write_release_catalog(plan, catalog)
  written <- benchmark_catalog(path)
  expect_identical(written$publication$storage_dois$no_bias, "10.test/1")
  bytes <- .read_bytes(path)
  expect_identical(bytes[length(bytes)], as.raw(10)); expect_false(as.raw(13) %in% bytes)
  expect_length(list.files(file.path(plan$state_directory, "catalog-history")), 1L)
  # A catalog that does not validate when read back, or whose publication block changed, is rolled back.
  catalog$publication$catalog_record_id <- "6"
  original <- .parse_json_bytes
  local_mocked_bindings(.parse_json_bytes = function(bytes) {
    parsed <- original(bytes); if (!is.null(parsed$publication)) parsed$publication$community_id <- "tampered"; parsed
  })
  expect_error(.write_release_catalog(plan, catalog), "failed verification.*restored")
  expect_identical(benchmark_catalog(path)$publication$catalog_record_id, "5")
  local_mocked_bindings(.parse_json_bytes = original, .validate_catalog = function(catalog) stop("invalid catalog"))
  expect_error(.write_release_catalog(plan, catalog), "invalid catalog")
})

## Plan identity is recomputed by the session ------------------------------------------------

test_that("a plan edited after planning is refused under the lock before the gate and any request", {
  root <- withr::local_tempdir(); plans <- Filter(Negate(is.null), session_plans(root))
  gate <- local_mock_gate()
  server <- community_server(.benchmark_community_id, members = function(page) members_page(list(member_hit("owner"))))
  writes <- local_write_recorder(server$handler)
  edits <- list(
    release = function(p) { p$catalog$release <- "test.9"; p },
    sandbox = function(p) { p$sandbox <- TRUE; p },
    community = function(p) { p$community <- "someone-elses-community"; p },
    group_membership = function(p) { p$groups <- lapply(p$groups, function(group) group[0]); p },
    metadata = function(p) { p$metadata$rights <- list(list(id = "mit")); p },
    packing_limit = function(p) { p$max_files <- 10L; p },
    catalog_content = function(p) { p$catalog$assets[[1]]$sha256 <- strrep("0", 64); p },
    catalog_family = function(p) { p$catalog_record_id <- "123"; p },
    changed_dgms = function(p) { p$changed_dgms <- character(); p },
    upload_selection = function(p) { p$groups <- list(); p },
    # Every field of an upload entry decides what is uploaded.
    entry_id = function(p) { p$groups[[1]][[1]]$id <- "no_bias/results/other"; p },
    entry_filename = function(p) { p$groups[[1]][[1]]$filename <- "other.csv"; p },
    entry_sha256 = function(p) { p$groups[[1]][[1]]$sha256 <- strrep("1", 64); p },
    entry_md5 = function(p) { p$groups[[1]][[1]]$md5 <- strrep("2", 32); p },
    entry_size = function(p) { p$groups[[1]][[1]]$size <- p$groups[[1]][[1]]$size + 1; p },
    entry_dgm = function(p) { p$groups[[1]][[1]]$dgm <- "other_dgm"; p })
  for (plan in plans) {
    archive <- identical(plan$catalog$schema_version, 2L)
    for (name in names(edits)) {
      if (!archive && name %in% c("catalog_family", "changed_dgms")) next
      edited <- edits[[name]](plan)
      expect_identical(edited$identity, plan$identity)   # the stored identity is untouched
      expect_error(stage_benchmark_release(edited, "t"), "modified after it was planned", info = paste(name, archive))
      expect_error(publish_benchmark_release(edited, "t", confirm = edited$catalog$release), "modified after it was planned",
                   info = paste(name, archive))
      expect_false(dir.exists(file.path(plan$state_directory, ".lock")), info = paste(name, archive))
      expect_false(file.exists(file.path(plan$state_directory, "state.json")), info = paste(name, archive))
    }
  }
  expect_length(gate$calls, 0L); expect_length(server$log$requests, 0L); expect_length(writes$log, 0L)
  # The untouched plan, and a copy reloaded from plan.rds, recompute to their stored identity;
  # the local file path of an upload is verified against its hashes when it is uploaded instead.
  for (plan in plans) {
    moved <- plan; moved$groups[[1]][[1]]$local_path <- file.path(tempdir(), "elsewhere.csv")
    expect_true(.identity_equal(.plan_identity(moved), plan$identity))
    expect_true(.identity_equal(.plan_identity(plan), plan$identity))
    expect_true(.identity_equal(.plan_identity(readRDS(file.path(plan$state_directory, "plan.rds"))), plan$identity))
  }
})

## Verification checks the state and the staged catalog first -------------------------------

local_no_requests <- function(env = parent.frame()) {
  seen <- new.env(); seen$count <- 0L
  testthat::local_mocked_bindings(.zenodo_request = function(...) {
    seen$count <- seen$count + 1L; stop("A request was issued")
  }, .env = env)
  seen
}
# A catalog staged for `plan`, with record IDs filled in like a staged release.json.
staged_catalog <- function(plan) {
  catalog <- if (identical(plan$catalog$schema_version, 2L)) .public_catalog(plan$catalog) else {
    plan$catalog$assets <- lapply(plan$catalog$assets, function(x) { x$local_path <- NULL; x }); plan$catalog
  }
  catalog
}

test_that("both verify paths read the state first and stop on bad state before any request", {
  root <- withr::local_tempdir(); plans <- Filter(Negate(is.null), session_plans(root))
  requests <- local_no_requests()
  for (plan in plans) {
    path <- file.path(plan$state_directory, "state.json"); schema <- plan$catalog$schema_version
    # Unreadable state, and missing state with a history, name the newest history file.
    writeLines("{broken", path)
    expect_error(verify_benchmark_release(plan, "t"), "unreadable or has an invalid structure", info = schema)
    history <- file.path(plan$state_directory, "state-history"); dir.create(history, showWarnings = FALSE)
    writeLines("{}", file.path(history, "20260101T000000.000001-0001-state.json"))
    expect_error(verify_benchmark_release(plan, "t"), "20260101T000000.000001-0001-state.json", info = schema)
    unlink(path)
    expect_error(verify_benchmark_release(plan, "t"), "is missing although history exists", info = schema)
    unlink(history, recursive = TRUE)
    # State of another plan.
    other <- plan; other$identity$fingerprint <- strrep("0", 64)
    .update_publication_state(other, function(state) { state$versions[["x"]] <- list(record_id = "1"); state })
    expect_error(verify_benchmark_release(plan, "t"), "belongs to a different plan", info = schema)
    # State without an identity: allowed to read, with a notice.
    state <- jsonlite::read_json(path); state$identity <- NULL
    jsonlite::write_json(state, path, auto_unbox = TRUE)
    expect_message(expect_error(verify_benchmark_release(plan, "t"), "not been fully staged|A request was issued"),
                   "no plan identity", info = schema)
    unlink(c(path, file.path(plan$state_directory, "state-history")), recursive = TRUE)
  }
  expect_identical(requests$count, 0L)
})

test_that("both verify paths stop when the staged catalog belongs to another release or inventory", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); plans <- session_plans(root)
  requests <- local_no_requests()
  for (plan in plans) {
    schema <- plan$catalog$schema_version
    path <- file.path(plan$state_directory, "release.json")
    catalog <- staged_catalog(plan)
    # The plan's own staged catalog passes the check.
    expect_true(.check_staged_catalog(plan, catalog))
    other_release <- catalog; other_release$release <- "other.release"
    .write_catalog_file(path, other_release)
    expect_error(verify_benchmark_release(plan, "t"), "belongs to release 'other.release', not to this plan's release 'test.1'", info = schema)
    different <- catalog
    if (identical(schema, 2L)) different$archives[[1]]$filename <- "another-archive.zip" else different$assets[[1]]$sha256 <- strrep("e", 64)
    .write_catalog_file(path, different)
    expect_error(verify_benchmark_release(plan, "t"), if (identical(schema, 2L)) "different archive inventory" else "lists different files",
                 info = schema)
    if (identical(schema, 2L)) {
      members <- catalog; members$archives[[1]]$members[[1]]$sha256 <- strrep("d", 64)
      expect_error(.check_staged_catalog(plan, members), "different archive inventory")
    }
    # Record IDs are the one thing a staged catalog may change.
    staged <- catalog
    if (identical(schema, 2L)) staged$archives <- lapply(staged$archives, function(a) { a$record_id <- "777"; a }) else
      staged$assets <- lapply(staged$assets, function(a) { a$record_id <- "777"; a })
    expect_true(.check_staged_catalog(plan, staged))
  }
  expect_identical(requests$count, 0L)
})

## Rollback of a failed verification ---------------------------------------------------------

test_that("a first version that cannot be removed is not reported as restored", {
  root <- withr::local_tempdir(); path <- file.path(root, "plan.rds")
  local_mocked_bindings(.file_remove = function(path) 1L)
  message <- tryCatch(.write_file_verified(path, charToRaw("new bytes"), verify = function(p) FALSE), error = conditionMessage)
  expect_false(grepl("the previous version was restored", message, fixed = TRUE))
  expect_match(message, paste0("restoring the previous version failed, so ", path, " holds the unverified new bytes"), fixed = TRUE)
  expect_identical(rawToChar(.read_bytes(path)), "new bytes")
  local_mocked_bindings(.file_remove = function(path) unlink(path))
  expect_match(tryCatch(.write_file_verified(path, charToRaw("newer"), verify = function(p) FALSE), error = conditionMessage),
               "the previous version was restored", fixed = TRUE)
  expect_identical(rawToChar(.read_bytes(path)), "new bytes")
  unlink(path)
  expect_match(tryCatch(.write_file_verified(path, charToRaw("first"), verify = function(p) FALSE), error = conditionMessage),
               "the previous version was restored", fixed = TRUE)
  expect_false(file.exists(path))
})

test_that("a restoration only counts when the destination then holds the previous bytes", {
  root <- withr::local_tempdir(); path <- file.path(root, "plan.rds")
  .write_file_verified(path, charToRaw("old bytes"))
  # The restore rename claims success but leaves the new bytes in place.
  renames <- 0L
  local_mocked_bindings(.file_retry_wait = function(seconds) NULL,
    .file_rename = function(from, to) { renames <<- renames + 1L; if (renames == 1L) file.rename(from, to) else TRUE })
  message <- tryCatch(.write_file_verified(path, charToRaw("new bytes"), verify = function(p) FALSE), error = conditionMessage)
  expect_false(grepl("the previous version was restored", message, fixed = TRUE))
  expect_match(message, paste0(path, " holds the unverified new bytes"), fixed = TRUE)
  expect_match(message, "the previous bytes are in ", fixed = TRUE)
  previous <- list.files(root, pattern = "^plan[.]rds-previous-.*[.]tmp$", full.names = TRUE)
  expect_length(previous, 1L); expect_identical(rawToChar(.read_bytes(previous)), "old bytes")
  # The previous bytes cannot even be written next to the file: reported, nothing renamed.
  unlink(previous)
  local_mocked_bindings(.file_rename = function(from, to) file.rename(from, to))
  .write_file_verified(path, charToRaw("old bytes"))
  local_mocked_bindings(.read_bytes = function(path) {
    if (grepl("-previous-", path, fixed = TRUE)) stop("cannot read back") else readBin(path, "raw", file.info(path)$size)
  })
  message <- tryCatch(.write_file_verified(path, charToRaw("new bytes"), verify = function(p) FALSE), error = conditionMessage)
  expect_match(message, "restoring the previous version failed", fixed = TRUE)
  expect_false(grepl("the previous bytes are in", message, fixed = TRUE))
  expect_length(list.files(root, pattern = "-previous-"), 0L)
})

test_that("a failed verification with the installed file held open reports the file that remains (Windows)", {
  skip_if_not(.Platform$OS.type == "windows", "an open file only blocks removal and replacement on Windows")
  root <- withr::local_tempdir(); path <- file.path(root, "plan.rds"); history <- file.path(root, "history")
  local_mocked_bindings(.file_retry_wait = function(seconds) NULL)
  hold <- NULL
  opener <- function(p) { hold <<- file(p, "rb"); FALSE }
  # First version: the file cannot be deleted while it is open.
  message <- tryCatch(.write_file_verified(path, charToRaw("first bytes"), verify = opener), error = conditionMessage)
  close(hold)
  expect_false(grepl("the previous version was restored", message, fixed = TRUE))
  expect_match(message, paste0("restoring the previous version failed, so ", path, " holds the unverified new bytes"), fixed = TRUE)
  expect_identical(rawToChar(.read_bytes(path)), "first bytes")
  # Replacement: the previous bytes cannot be renamed over the open file and stay intact next to it.
  .write_file_verified(path, charToRaw("old bytes"), history_dir = history)
  message <- tryCatch(.write_file_verified(path, charToRaw("new bytes"), verify = opener, history_dir = history), error = conditionMessage)
  close(hold)
  expect_match(message, paste0(path, " holds the unverified new bytes"), fixed = TRUE)
  previous <- list.files(root, pattern = "^plan[.]rds-previous-.*[.]tmp$", full.names = TRUE)
  expect_length(previous, 1L)
  expect_match(message, "the previous bytes are in ", fixed = TRUE)
  expect_match(message, basename(previous), fixed = TRUE)
  expect_match(message, "keeps every version", fixed = TRUE)
  expect_identical(rawToChar(.read_bytes(previous)), "old bytes")
  expect_identical(rawToChar(.read_bytes(path)), "new bytes")
  # Without an open handle the same failure is restored completely.
  unlink(previous); .write_file_verified(path, charToRaw("old bytes"))
  expect_match(tryCatch(.write_file_verified(path, charToRaw("newest"), verify = function(p) FALSE), error = conditionMessage),
               "the previous version was restored", fixed = TRUE)
  expect_identical(rawToChar(.read_bytes(path)), "old bytes")
})

## Storage base records and earlier versions -----------------------------------------------------------

# A second archive release on top of a published first one: the changed DGM is imported into
# a new version of the storage record "55".
base_release_plan <- function(root) {
  a <- test_resource(root, "A.csv"); b <- test_resource(root, "B.csv", method = "B")
  metadata <- list(rights = list(list(id = "cc-by-4.0")))
  first <- plan_benchmark_release("test.1", list(a, b), conditions = test_catalog(list(a))$conditions, metadata = metadata,
    state_directory = file.path(root, "first"), community = "c")
  base <- .public_catalog(first$catalog)
  base$publication <- list(catalog_record_id = "123", catalog_concept_doi = "10.test/concept")
  base$archives <- lapply(base$archives, function(x) { x$record_id <- "55"; x })
  base$assets <- lapply(base$assets, function(x) { x$record_id <- "55"; x })
  corrected <- test_resource(root, "A.csv", ids = 3:4)
  plan_benchmark_release("test.2", list(corrected), previous = base, replace = a$id, metadata = metadata,
    state_directory = file.path(root, "second"), community = "c")
}

test_that("the identity binds the storage base records and the earlier catalog's schema", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); plan <- base_release_plan(root)
  expect_identical(plan$changed_dgms, "no_bias")
  expect_identical(.storage_base_id(plan, "no_bias"), "55")
  gate <- local_mock_gate()
  server <- community_server(.benchmark_community_id, members = function(page) members_page(list(member_hit("owner"))))
  writes <- local_write_recorder(server$handler)
  edits <- list(
    # Version 1 catalogs have no native storage version to import: execution would start a new family.
    previous_schema = function(p) { p$previous$schema_version <- 1L; p },
    base_record = function(p) { p$previous$archives <- lapply(p$previous$archives, function(a) { a$record_id <- "999"; a }); p },
    no_previous = function(p) { p$previous <- NULL; p })
  for (name in names(edits)) {
    edited <- edits[[name]](plan)
    expect_identical(edited$identity, plan$identity)
    expect_error(stage_benchmark_release(edited, "t"), "modified after it was planned", info = name)
    expect_error(publish_benchmark_release(edited, "t", confirm = "test.2"), "modified after it was planned", info = name)
    expect_false(dir.exists(file.path(plan$state_directory, ".lock")), info = name)
  }
  expect_length(gate$calls, 0L); expect_length(server$log$requests, 0L); expect_length(writes$log, 0L)
  expect_true(.identity_equal(.plan_identity(plan), plan$identity))
  # The binding is what execution resolves, not a copy of the previous catalog.
  edited <- edits$base_record(plan)
  expect_identical(.storage_base_id(edited, "no_bias"), "999")
  expect_false(.identity_equal(.plan_identity(edited), plan$identity))
})

## Identity format version ---------------------------------------------------------------------------

test_that("a plan or state with an older or missing identity version must be re-planned", {
  root <- withr::local_tempdir(); plans <- Filter(Negate(is.null), session_plans(root))
  gate <- local_mock_gate()
  server <- community_server(.benchmark_community_id, members = function(page) members_page(list(member_hit("owner"))))
  writes <- local_write_recorder(server$handler)
  message <- "This plan was created by an earlier package version; re-plan in a new state directory."
  for (plan in plans) {
    expect_identical(plan$identity$version, 2L)
    schema <- plan$catalog$schema_version
    older <- plan; older$identity$version <- 1L
    missing <- plan; missing$identity$version <- NULL
    empty <- plan; empty$identity <- list()
    absent <- plan; absent["identity"] <- list(NULL)
    for (candidate in list(older, missing, empty, absent))
      for (call in list(function(p) stage_benchmark_release(p, "t"), function(p) publish_benchmark_release(p, "t", confirm = "test.1")))
        expect_error(call(candidate), message, fixed = TRUE, info = schema)
    expect_false(dir.exists(file.path(plan$state_directory, ".lock")))
    # Planning again in the directory of a plan saved with the older identity stops with the same message.
    saved <- readRDS(file.path(plan$state_directory, "plan.rds"))
    saved$identity$version <- 1L
    saveRDS(saved, file.path(plan$state_directory, "plan.rds"))
    a <- test_resource(root, "A.csv")
    expect_error(plan_benchmark_release("test.1", list(a), conditions = test_catalog(list(a))$conditions, metadata = list(rights = list(list(id = "cc-by-4.0"))),
      state_directory = plan$state_directory, archive = identical(schema, 2L), community = "publicationbiasbenchmark"),
      message, fixed = TRUE, info = schema)
    saved$identity$version <- NULL
    saveRDS(saved, file.path(plan$state_directory, "plan.rds"))
    expect_error(plan_benchmark_release("test.1", list(a), conditions = test_catalog(list(a))$conditions, metadata = list(rights = list(list(id = "cc-by-4.0"))),
      state_directory = plan$state_directory, archive = identical(schema, 2L), community = "publicationbiasbenchmark"),
      message, fixed = TRUE, info = schema)
    # State written with an older identity format is refused for writes and noted when read.
    path <- file.path(plan$state_directory, "state.json")
    jsonlite::write_json(list(groups = list(), identity = list(version = 1L, release = "test.1", sandbox = FALSE, community = "x", fingerprint = "f")),
                         path, auto_unbox = TRUE)
    expect_error(.publication_state(plan, "write"), "written by an earlier package version; re-plan in a new state directory")
    expect_message(.publication_state(plan, "read"), "older plan identity")
    unlink(path)
  }
  expect_length(gate$calls, 0L); expect_length(server$log$requests, 0L); expect_length(writes$log, 0L)
})
