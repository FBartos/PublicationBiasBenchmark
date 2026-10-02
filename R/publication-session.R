# Maintainer-only publication machinery: the community gate, the publication
# session (confirm, token, lock, state identity, gate), the publication lock,
# verified file writes with history, the locked publication state and the single
# remote delete. Nothing here is exported.

# The production community that benchmark releases may be published to.
.benchmark_community_id <- "415b2de6-b6f9-444d-9109-d74752e20cd0"

.release_roles <- c("owner", "manager")

# Gate: stop unless the token's account holds one of `roles` in the community.
# Fails closed on every unexpected answer and returns the community UUID.
# This protects the package's entry points; it is not a security boundary for
# Zenodo itself.
.require_community_maintainer <- function(community, token, sandbox, roles) {
  refuse <- function(...) stop(..., call. = FALSE)
  if (!.scalar_string(community)) refuse("A community slug or UUID is required.")
  record <- tryCatch(
    .zenodo_request("GET", paste0("communities/", utils::URLencode(community, reserved = TRUE)), token, sandbox),
    error = function(error) refuse("Cannot verify the community '", community, "': ", conditionMessage(error)))
  id <- record$id
  if (!.scalar_string(id)) refuse("Community resolution did not return a UUID.")
  if (!isTRUE(sandbox) && !identical(id, .benchmark_community_id))
    refuse("Refusing to proceed: production publication is restricted to the PublicationBiasBenchmark community.")
  for (page in seq_len(50L)) {
    members <- tryCatch(
      .zenodo_request("GET", sprintf("communities/%s/members?size=100&page=%d", id, page), token, sandbox),
      zenodo_http_error = function(error) error)
    if (inherits(members, "zenodo_http_error")) {
      if (isTRUE(members$status == 403L)) .reject_community_access(token, sandbox)
      refuse("Cannot read the community membership (HTTP ", members$status, ").")
    }
    hits <- if (is.list(members) && is.list(members$hits)) members$hits$hits
    if (!is.list(hits)) refuse("Unexpected community membership response.")
    mine <- Filter(function(hit) is.list(hit) && isTRUE(hit$is_current_user), hits)
    if (length(mine)) {
      role <- mine[[1]]$role
      if (!.scalar_string(role) || !role %in% roles)
        refuse("The Zenodo token holds the role '", paste(as.character(unlist(role)), collapse = ", "),
               "' in the community; one of ", paste0("'", roles, "'", collapse = ", "), " is required.")
      return(id)
    }
    if (!is.list(members$links) || is.null(members$links[["next"]]))
      refuse("The Zenodo token's account was not found among the community members.")
  }
  refuse("The community has more members than the gate can page through; refusing to proceed.")
}

# A 403 on the member list means either "not a member" or "token not accepted"
# (an invalid or other-environment token is treated as anonymous); the token's
# own community list tells them apart.
.reject_community_access <- function(token, sandbox) {
  check <- tryCatch(.zenodo_request("GET", "user/communities?size=1", token, sandbox),
                    zenodo_http_error = function(error) error)
  if (inherits(check, "zenodo_http_error")) {
    if (isTRUE(check$status == 403L))
      stop("The Zenodo token is not accepted: token invalid, expired, or for the other environment (sandbox vs production).", call. = FALSE)
    stop("Cannot check the Zenodo token (HTTP ", check$status, ").", call. = FALSE)
  }
  stop("The Zenodo token's account is not a member of the community.", call. = FALSE)
}

# The only place that checks confirm, takes the publication lock and runs the
# gate, in this order: confirm (local), token, lock, state identity, gate (the
# first network action). fn(token, community_id) never locks, gates or confirms.
.publication_session <- function(plan, token, roles, confirm = NULL, require_confirm = FALSE, fn) {
  if (require_confirm && !(.scalar_string(confirm) && identical(confirm, plan$catalog$release)))
    stop("Publishing is irreversible: run PublicationBiasBenchmark:::verify_benchmark_release(plan) and re-run with confirm = \"",
         plan$catalog$release, "\"", call. = FALSE)
  token <- .publication_token(plan, token)
  .acquire_publication_lock(plan$state_directory)
  on.exit(.release_publication_lock(plan$state_directory), add = TRUE)
  if (is.null(plan$identity))
    stop("This plan was created by an earlier package version; re-plan in a new state directory.", call. = FALSE)
  if (!.scalar_string(plan$community))
    stop("This plan has no community; re-plan with this package version.", call. = FALSE)
  .publication_state(plan, "write")
  community_id <- .require_community_maintainer(plan$community, token, plan$sandbox, roles)
  fn(token, community_id)
}

## Publication lock ----------------------------------------------------------

# Locks held by this process: normalised state directory -> nonce.
.publication_locks <- new.env(parent = emptyenv())

.lock_key <- function(state_directory) normalizePath(state_directory, winslash = "/", mustWork = FALSE)

.utc_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

.new_nonce <- function() {
  digest::digest(paste(Sys.getpid(), Sys.info()[["nodename"]], format(Sys.time(), "%Y%m%d%H%M%OS6"),
                       tempfile(), proc.time()[["elapsed"]]), algo = "sha256", serialize = FALSE)
}

.read_lock_owner <- function(lock) {
  tryCatch(suppressWarnings(jsonlite::read_json(file.path(lock, "owner.json"))), error = function(error) NULL)
}

.lock_owner_text <- function(lock) {
  owner <- .read_lock_owner(lock)
  if (!is.list(owner) || !.scalar_string(owner$started)) return("owner unknown")
  started <- as.POSIXct(owner$started, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  age <- if (is.na(started)) "age unknown" else
    sprintf("%d minutes ago", as.integer(round(as.numeric(difftime(Sys.time(), started, units = "mins")))))
  paste0("pid ", paste(unlist(owner$pid), collapse = ""), " on ", paste(unlist(owner$host), collapse = ""),
         ", started ", owner$started, " UTC, ", age)
}

# Atomic lock: creating the directory either succeeds or fails.
.acquire_publication_lock <- function(state_directory) {
  if (!.scalar_string(state_directory)) stop("A publication state directory is required.", call. = FALSE)
  dir.create(state_directory, recursive = TRUE, showWarnings = FALSE)
  key <- .lock_key(state_directory)
  lock <- file.path(key, ".lock")
  if (!dir.exists(key) || !suppressWarnings(dir.create(lock, showWarnings = FALSE))) {
    if (dir.exists(lock))
      stop("The publication state directory is locked (", .lock_owner_text(lock), "). Remove ", lock,
           " only if that session is not running, then retry.", call. = FALSE)
    stop("Cannot create the lock in ", key, ".", call. = FALSE)
  }
  nonce <- .new_nonce()
  owner <- list(pid = Sys.getpid(), host = Sys.info()[["nodename"]], started = .utc_now(), nonce = nonce)
  tryCatch(.write_json_verified(file.path(lock, "owner.json"), owner),
           error = function(error) { unlink(lock, recursive = TRUE); stop(error) })
  assign(key, nonce, envir = .publication_locks)
  invisible(key)
}

# Removes the lock only while owner.json still carries this session's nonce.
.release_publication_lock <- function(state_directory) {
  key <- .lock_key(state_directory)
  nonce <- .publication_locks[[key]]
  if (is.null(nonce)) return(invisible(FALSE))
  rm(list = key, envir = .publication_locks)
  lock <- file.path(key, ".lock")
  owner <- .read_lock_owner(lock)
  if (!identical(owner$nonce, nonce)) {
    warning("The publication lock ", lock, " no longer belongs to this session and was not removed.", call. = FALSE)
    return(invisible(FALSE))
  }
  unlink(lock, recursive = TRUE)
  if (dir.exists(lock)) {
    warning("Could not remove the publication lock ", lock, "; remove it before the next run.", call. = FALSE)
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

.with_publication_lock <- function(state_directory, fn) {
  .acquire_publication_lock(state_directory)
  on.exit(.release_publication_lock(state_directory), add = TRUE)
  fn()
}

.assert_publication_lock <- function(plan) {
  if (!.scalar_string(plan$state_directory))
    stop("Remote deletions require the plan's state directory.", call. = FALSE)
  key <- .lock_key(plan$state_directory)
  nonce <- .publication_locks[[key]]
  owner <- .read_lock_owner(file.path(key, ".lock"))
  if (is.null(nonce) || !identical(owner$nonce, nonce))
    stop("Remote deletions require this process to hold the publication lock of ", key, ".", call. = FALSE)
  invisible(TRUE)
}

## Verified file writes ------------------------------------------------------

.read_bytes <- function(path) readBin(path, "raw", n = file.info(path)$size)

# Small wrappers so that tests can fail the file operations and skip the waits.
.file_copy <- function(from, to) file.copy(from, to, overwrite = FALSE)
.file_rename <- function(from, to) file.rename(from, to)
.file_retry_wait <- function(seconds) Sys.sleep(seconds)

.retry_file_operation <- function(operation, attempts = 5L, wait = 0.2) {
  for (attempt in seq_len(attempts)) {
    done <- tryCatch(isTRUE(suppressWarnings(operation())), error = function(error) FALSE)
    if (done) return(TRUE)
    if (attempt < attempts) .file_retry_wait(wait)
  }
  FALSE
}

.history_prefix_length <- 28L  # "<%Y%m%dT%H%M%OS6>-<%04d>-"

# History entries of one basename, oldest first (the names sort chronologically).
.history_entries <- function(history_dir, name) {
  files <- list.files(history_dir, pattern = "^[0-9]{8}T[0-9]{6}[.][0-9]{6}-[0-9]{4}-")
  files <- sort(files[substring(files, .history_prefix_length + 1L) == name])
  if (length(files)) file.path(history_dir, files) else character()
}

.history_target <- function(history_dir, name) {
  stamp <- format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC")
  counter <- length(.history_entries(history_dir, name)) + 1L
  repeat {
    target <- file.path(history_dir, sprintf("%s-%04d-%s", stamp, counter, name))
    if (!file.exists(target)) return(target)
    counter <- counter + 1L
  }
}

# Copy a source file into the history under a unique name and check its MD5.
.add_history_entry <- function(source, history_dir, name, bytes) {
  target <- .history_target(history_dir, name)
  expected <- digest::digest(bytes, algo = "md5", serialize = FALSE)
  copied <- .retry_file_operation(function() .file_copy(source, target))
  if (!copied || !file.exists(target) || !identical(unname(tools::md5sum(target)), expected))
    stop("Cannot keep a verified history copy of ", name, " in ", history_dir,
         "; nothing was replaced.", call. = FALSE)
  target
}

# Replace `path` with `bytes` only after the new bytes have been written
# completely next to it and a history copy exists. Returns FALSE when the file
# already has these bytes. `verify(path)` runs on the installed file; when it
# fails the previous bytes are restored.
.write_file_verified <- function(path, bytes, verify = NULL, history_dir = NULL) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  name <- basename(path)
  old <- if (file.exists(path)) .read_bytes(path) else NULL
  if (!is.null(old) && identical(old, bytes)) return(invisible(FALSE))
  temporary <- tempfile(paste0(name, "-"), tmpdir = dirname(path), fileext = ".tmp")
  complete <- tryCatch({ writeBin(bytes, temporary); identical(.read_bytes(temporary), bytes) },
                       error = function(error) FALSE, warning = function(warning) FALSE)
  if (!complete) {
    unlink(temporary)
    stop("Cannot write a complete copy of ", name, "; nothing was replaced.", call. = FALSE)
  }
  if (!is.null(history_dir)) {
    dir.create(history_dir, recursive = TRUE, showWarnings = FALSE)
    entries <- .history_entries(history_dir, name)
    # A file from older code may not be in the history yet.
    if (!is.null(old) && (!length(entries) || !identical(.read_bytes(entries[length(entries)]), old)))
      .add_history_entry(path, history_dir, name, old)
    .add_history_entry(temporary, history_dir, name, bytes)
  }
  if (!.retry_file_operation(function() .file_rename(temporary, path)))
    stop("Cannot replace ", path, "; the complete new copy remains at ", temporary,
         if (!is.null(history_dir)) paste0(" and the history in ", history_dir), ".", call. = FALSE)
  reason <- NULL
  installed <- tryCatch({
    if (!identical(.read_bytes(path), bytes)) stop("the installed bytes differ")
    is.null(verify) || isTRUE(verify(path))
  }, error = function(error) { reason <<- conditionMessage(error); FALSE })
  if (!installed) {
    # The previous bytes are first written completely next to the file, so a
    # failing restore leaves them under a name that the message reports.
    previous <- NULL
    restored <- if (is.null(old)) { unlink(path); !file.exists(path) } else {
      previous <- tempfile(paste0(name, "-previous-"), tmpdir = dirname(path), fileext = ".tmp")
      saved <- tryCatch({ writeBin(old, previous); identical(.read_bytes(previous), old) },
                        error = function(error) FALSE, warning = function(warning) FALSE)
      if (!saved) { unlink(previous); previous <- NULL }
      saved && .retry_file_operation(function() .file_rename(previous, path))
    }
    stop("The new ", name, " failed verification", if (!is.null(reason)) paste0(" (", reason, ")"),
         if (restored) "; the previous version was restored." else paste0(
           "; restoring the previous version failed, so ", path, " holds the unverified new bytes",
           if (!is.null(previous)) paste0("; the previous bytes are in ", previous),
           if (!is.null(history_dir)) paste0("; the history in ", history_dir, " keeps every version"),
           "."), call. = FALSE)
  }
  invisible(TRUE)
}

.json_bytes <- function(object, pretty = FALSE) {
  text <- as.character(jsonlite::toJSON(object, auto_unbox = TRUE, pretty = pretty, null = "null",
                                        digits = NA, dataframe = "rows"))
  c(charToRaw(enc2utf8(text)), as.raw(10L))
}

.parse_json_bytes <- function(bytes) {
  text <- rawToChar(bytes); Encoding(text) <- "UTF-8"
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

# JSON files are LF-terminated, UTF-8, and must parse before anything is replaced.
.write_json_verified <- function(path, object, pretty = FALSE, history_dir = NULL, verify = NULL) {
  bytes <- .json_bytes(object, pretty)
  parsed <- tryCatch(.parse_json_bytes(bytes), error = function(error) NULL)
  if (!is.list(parsed)) stop("Cannot serialise ", basename(path), " as JSON; nothing was replaced.", call. = FALSE)
  .write_file_verified(path, bytes, verify, history_dir)
}

.write_rds_verified <- function(path, object) {
  scratch <- tempfile(fileext = ".rds"); on.exit(unlink(scratch), add = TRUE)
  saveRDS(object, scratch)
  if (!identical(readRDS(scratch), object)) stop("Cannot serialise ", basename(path), "; nothing was replaced.", call. = FALSE)
  .write_file_verified(path, .read_bytes(scratch), verify = function(installed) identical(readRDS(installed), object))
}

## Plan identity and publication state ----------------------------------------

.canonical_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = "null"))
.canonical_fingerprint <- function(x) digest::digest(.canonical_json(x), algo = "sha256", serialize = FALSE)

.strip_local_paths <- function(catalog) {
  catalog$assets <- lapply(catalog$assets, function(x) { x$local_path <- NULL; x })
  if (!is.null(catalog$archives)) catalog$archives <- lapply(catalog$archives, function(x) { x$local_path <- NULL; x })
  catalog
}

# What a state directory is bound to: the catalog without local paths, the
# group/archive membership, the packing limits, community, environment, release.
.plan_identity <- function(plan) {
  archive <- identical(plan$catalog$schema_version, 2L)
  membership <- if (archive) lapply(plan$catalog$archives, function(a)
    list(id = a$id, members = vapply(a$members, `[[`, character(1), "id"))) else
    lapply(plan$groups, function(group) vapply(group, `[[`, character(1), "id"))
  list(version = 1L, release = plan$catalog$release, sandbox = isTRUE(plan$sandbox), community = plan$community,
       fingerprint = .canonical_fingerprint(list(catalog = .strip_local_paths(plan$catalog), membership = membership,
         max_files = plan$max_files, max_bytes = plan$max_bytes, max_archive_bytes = plan$max_archive_bytes,
         community = plan$community, sandbox = isTRUE(plan$sandbox), release = plan$catalog$release)))
}

.identity_equal <- function(a, b) {
  fields <- c("version", "release", "sandbox", "community", "fingerprint")
  flat <- function(x) vapply(fields, function(field) paste(as.character(unlist(x[[field]])), collapse = ","), character(1))
  is.list(a) && is.list(b) && identical(flat(a), flat(b))
}

.named_or_empty <- function(x) is.null(x) || (is.list(x) && (!length(x) || !is.null(names(x))))

# Known fields only; unknown keys are allowed.
.valid_state <- function(state) {
  is.list(state) && (!length(state) || !is.null(names(state))) &&
    (is.null(state$groups) || is.list(state$groups)) &&
    .named_or_empty(state$versions) && .named_or_empty(state$inclusions) && .named_or_empty(state$initialized) &&
    (is.null(state$catalog_record) || is.list(state$catalog_record))
}

# The newest history file to copy back when state.json is missing or unreadable.
.state_backup <- function(directory) {
  entries <- .history_entries(file.path(directory, "state-history"), "state.json")
  if (length(entries)) return(entries[length(entries)])
  previous <- file.path(directory, "state.json.previous")
  if (file.exists(previous)) previous else NULL
}

# mode "write" (every update) requires the state to belong to the plan; "read"
# (read-only verification) accepts a state without identity with a message.
.publication_state <- function(plan, mode = c("write", "read")) {
  mode <- match.arg(mode)
  directory <- plan$state_directory
  path <- file.path(directory, "state.json")
  fail <- function(what) {
    backup <- .state_backup(directory)
    stop("The publication state ", what, " Nothing was changed online. ",
         if (is.null(backup)) "No state history exists in this directory." else
           paste0("Check the newest history file ", backup, " and copy it to ", path, ", then retry."), call. = FALSE)
  }
  if (!file.exists(path)) {
    if (!is.null(.state_backup(directory))) fail(paste0("file ", path, " is missing although history exists."))
    return(list(groups = list(), catalog_record = NULL))
  }
  state <- tryCatch(suppressWarnings(jsonlite::read_json(path, simplifyVector = FALSE)), error = function(error) NULL)
  if (!.valid_state(state)) fail(paste0("file ", path, " is unreadable or has an invalid structure."))
  if (is.null(state$identity)) {
    if (mode == "write" && !is.null(plan$identity))
      stop("The publication state was written by an earlier package version; re-plan in a new state directory.", call. = FALSE)
    if (mode == "read" && !is.null(plan$identity))
      message("The publication state has no plan identity (written by an earlier package version).")
  } else if (!.identity_equal(state$identity, plan$identity)) {
    stop("The publication state in ", directory, " belongs to a different plan (release, community, environment or contents differ); use a new state directory.", call. = FALSE)
  }
  state
}

# The only writer of state.json: re-read, apply, save (verified, with history).
.update_publication_state <- function(plan, fn) {
  state <- .publication_state(plan, "write")
  updated <- fn(state)
  if (!is.list(updated)) stop("A publication state update must return the new state.", call. = FALSE)
  if (is.null(updated$identity) && !is.null(plan$identity)) updated$identity <- plan$identity
  .write_json_verified(file.path(plan$state_directory, "state.json"), updated, pretty = TRUE,
    history_dir = file.path(plan$state_directory, "state-history"),
    verify = function(path) is.list(jsonlite::read_json(path, simplifyVector = FALSE)))
  invisible(updated)
}

## Remote deletion --------------------------------------------------------------

.deletion_log <- function(plan, event, record_id, key, reason, details = list()) {
  line <- jsonlite::toJSON(c(list(event = event, time = .utc_now(), pid = Sys.getpid(),
                                  record_id = as.character(record_id), key = key, reason = reason), details),
                           auto_unbox = TRUE, null = "null", digits = NA)
  connection <- file(file.path(plan$state_directory, "deletions.log"), "ab")
  on.exit(close(connection), add = TRUE)
  cat(line, "\n", file = connection, sep = "")
}

# The only code that issues a DELETE. A draft file is deleted only when it is
# provably redundant: a pending entry created by this publication state
# (pending-reset) or an import of bytes that the published base record holds
# (superseded-import). The log is for audit only and is never read.
.delete_draft_file <- function(plan, record_id, key, reason = c("pending-reset", "superseded-import"),
                               token, base_id = NULL) {
  reason <- match.arg(reason)
  .assert_publication_lock(plan)
  record_id <- as.character(record_id)
  draft_files <- paste0(.zenodo_base(plan$sandbox), "/records/", record_id, "/draft/files")
  if (.record_is_published(plan, record_id, token))
    stop("Record ", record_id, " is already published; refusing to delete '", key, "'.", call. = FALSE)
  files <- .zenodo_request("GET", paste0("records/", record_id, "/draft/files"), token, plan$sandbox)
  entry <- .zenodo_file_entry(files, key)
  if (is.null(entry))
    stop("Draft file '", key, "' of record ", record_id, " no longer exists; nothing was deleted.", call. = FALSE)
  if (reason == "pending-reset") {
    holds_bytes <- !is.null(entry$size) || !is.null(entry$checksum)
    if (!identical(entry$status, "pending") || !holds_bytes)
      stop("Refusing to delete draft file '", key, "' of record ", record_id,
           ": it is not a pending file holding uploaded bytes. Nothing was deleted.", call. = FALSE)
    journal <- .publication_state(plan, "write")$initialized[[record_id]][[key]]
    if (!.scalar_string(journal$created) || !.scalar_string(entry$created) || !identical(entry$created, journal$created))
      stop("Refusing to delete draft file '", key, "' of record ", record_id,
           ": it holds data, but this publication state did not create it. Nothing was deleted. ",
           "Inspect ", draft_files, " and resolve it manually.", call. = FALSE)
  } else {
    if (!.scalar_string(base_id) || !.record_is_published(plan, base_id, token))
      stop("Refusing to delete '", key, "': the base record is not published. Nothing was deleted.", call. = FALSE)
    base_entry <- .zenodo_file_entry(.zenodo_request("GET", paste0("records/", base_id, "/files"), token, plan$sandbox), key)
    same <- !is.null(base_entry) && .scalar_string(entry$checksum) && identical(entry$checksum, base_entry$checksum) &&
      is.numeric(entry$size) && length(entry$size) == 1L && is.numeric(base_entry$size) &&
      length(base_entry$size) == 1L && !is.na(entry$size) && entry$size == base_entry$size
    if (!isTRUE(same))
      stop("Refusing to delete '", key, "': the published base record does not hold identical bytes. Nothing was deleted.", call. = FALSE)
  }
  details <- list(status = entry$status, created = entry$created, checksum = entry$checksum, size = entry$size)
  .deletion_log(plan, "intent", record_id, key, reason, details)
  .zenodo_request("DELETE", paste0("records/", record_id, "/draft/files/", utils::URLencode(key, reserved = TRUE)),
                  token, plan$sandbox)
  tryCatch(.deletion_log(plan, "done", record_id, key, reason, details), error = function(error)
    warning("Could not append to the deletion log: ", conditionMessage(error), call. = FALSE))
  invisible(TRUE)
}
