.archive_unit <- function(asset) {
  kind <- if (identical(asset$measure, "pairwise")) "pairwise" else asset$kind
  paste(c(asset$dgm, kind, if (kind %in% c("results", "measures")) c(asset$method, asset$method_setting)), collapse = "--")
}
.asset_inputs <- function(asset, assets) {
  declared <- unlist(asset$dependencies, use.names = FALSE)
  closed_methods <- asset$kind == "pairwise" || identical(asset$measure, "pairwise") || isTRUE(asset$replacement)
  if (length(declared) && (isTRUE(asset$dependencies_explicit) || closed_methods)) {
    # Existing catalogs carry frozen ID/hash descriptors, input resources use IDs.
    if (is.list(asset$dependencies[[1]]) && !is.null(asset$dependencies[[1]]$id))
      declared <- vapply(asset$dependencies, `[[`, character(1), "id")
    ids <- vapply(assets, `[[`, character(1), "id")
    if (length(setdiff(declared, ids))) stop("Unknown computation input for ", asset$id, call. = FALSE)
    selected <- assets[match(declared, ids)]
    expected_kind <- if (asset$kind == "results") "data" else if (asset$kind %in% c("measures", "pairwise")) "results" else NULL
    if (is.null(expected_kind) || any(!vapply(selected, function(x) identical(x$kind, expected_kind) && identical(x$dgm, asset$dgm), logical(1))))
      stop("Computation inputs have an incompatible kind or DGM: ", asset$id, call. = FALSE)
    if (asset$kind %in% c("measures", "pairwise")) {
      # Aggregate over all shards of the methods that actually supplied inputs.
      # An old table cannot depend on a method first introduced in a later release.
      groups <- unique(vapply(selected, function(x) paste(x$method, x$method_setting, sep = "/"), character(1)))
      selected <- Filter(function(x) x$kind == "results" && identical(x$dgm, asset$dgm) &&
        length(intersect(unlist(x$condition_ids), unlist(asset$condition_ids))) > 0L &&
        paste(x$method, x$method_setting, sep = "/") %in% groups, assets)
    }
  } else {
    selected <- Filter(function(x) {
      same <- identical(x$dgm, asset$dgm) && length(intersect(unlist(x$condition_ids), unlist(asset$condition_ids))) > 0L
      if (!same) return(FALSE)
      if (asset$kind == "results") return(x$kind == "data")
      if (asset$kind %in% c("measures", "pairwise")) {
        if (x$kind != "results") return(FALSE)
        if (asset$kind == "pairwise" || identical(asset$measure, "pairwise") || isTRUE(asset$replacement)) return(TRUE)
        return(identical(x$method, asset$method) && identical(x$method_setting, asset$method_setting))
      }
      FALSE
    }, assets)
  }
  lapply(selected, function(x) list(id = x$id, sha256 = x$sha256))
}
.validate_release_dependencies <- function(assets, base, files, replace) {
  if (is.null(base)) return(invisible(TRUE))
  supplied <- vapply(files, `[[`, character(1), "id")
  for (old in base$assets) {
    if (!old$kind %in% c("results", "measures", "pairwise")) next
    current <- Filter(function(x) identical(x$id, old$id), assets)
    if (!length(current)) next
    before <- .asset_inputs(old, base$assets)
    # Infer current coverage with the old declaration (including replacement
    # methods); additions to an ordinary aggregate also invalidate its measures.
    reference <- old
    if (!length(old$dependencies) && (isTRUE(old$replacement) || old$kind == "pairwise" || identical(old$measure, "pairwise")))
      reference$dependencies <- before
    after <- .asset_inputs(reference, assets)
    canonical <- function(x) x[order(vapply(x, `[[`, character(1), "id"))]
    if (!identical(canonical(before), canonical(after)) && !(old$id %in% replace && old$id %in% supplied))
      stop("Stale derived asset '", old$id, "': recompute and supply it with explicit replace after its inputs change.", call. = FALSE)
  }
  invisible(TRUE)
}
.archive_source <- function(asset, base) {
  if (!is.null(asset$local_path) && .file_verified(asset$local_path, asset$sha256, asset$size, asset$md5)) return(asset$local_path)
  if (is.null(base)) stop("Unverified local file: ", asset$filename, call. = FALSE)
  .download_catalog_assets(base, list(asset), progress = FALSE, max_try = 3)
  .asset_cache_path(asset)
}
.split_archive_members <- function(assets, limit, data = FALSE) {
  if (any(vapply(assets, `[[`, numeric(1), "size") > limit))
    stop("A single member exceeds the uncompressed archive cap; write smaller worker shards: ",
         paste(vapply(Filter(function(x) x$size > limit, assets), `[[`, character(1), "filename"), collapse = ", "), call. = FALSE)
  if (data) {
    groups <- split(assets, vapply(assets, function(x) as.character(min(unlist(x$condition_ids))), character(1)))
    groups <- groups[order(as.numeric(names(groups)))]
    # Preserve whole conditions where possible. A condition larger than the cap
    # is split at file boundaries, without rewriting any original file.
    pieces <- list()
    for (group in groups) {
      if (sum(vapply(group, `[[`, numeric(1), "size")) > limit) pieces <- c(pieces, .split_archive_members(group, limit))
      else pieces[[length(pieces) + 1L]] <- group
    }
  } else pieces <- lapply(assets[order(vapply(assets, `[[`, character(1), "id"))], list)
  parts <- list(); part <- list(); bytes <- 0
  for (piece in pieces) {
    size <- sum(vapply(piece, `[[`, numeric(1), "size"))
    if (length(part) && (bytes + size > limit || length(part) + length(piece) > 65534L)) {
      parts[[length(parts) + 1L]] <- part; part <- list(); bytes <- 0
    }
    part <- c(part, piece); bytes <- bytes + size
  }
  if (length(part)) parts[[length(parts) + 1L]] <- part
  parts
}
.verify_local_archive_members <- function(path, members) {
  .zip_inventory(path, members)
  directory <- tempfile("verify-archive-"); dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  for (member in members) {
    utils::unzip(path, files = member$filename, exdir = directory, junkpaths = TRUE, unzip = "internal")
    extracted <- file.path(directory, member$filename)
    if (!.file_verified(extracted, member$sha256, member$size, member$md5))
      stop("Cannot recover archive: member differs from the planned source: ", member$filename, call. = FALSE)
    unlink(extracted)
  }
  invisible(TRUE)
}
.write_archive_descriptor <- function(descriptor, path) {
  temporary <- tempfile("archive-state-", tmpdir = dirname(path), fileext = ".json")
  backup <- paste0(temporary, ".previous")
  on.exit(unlink(c(temporary, backup)), add = TRUE)
  jsonlite::write_json(descriptor, temporary, auto_unbox = TRUE, null = "null", digits = NA)
  # write_json may warn on a full filesystem: parse the completed temporary
  # bytes before replacing any usable state.
  verified <- jsonlite::read_json(temporary)
  if (!identical(verified$sha256, descriptor$sha256) || !identical(verified$build_fingerprint, descriptor$build_fingerprint))
    stop("Could not persist a complete archive descriptor.", call. = FALSE)
  if (file.exists(path) && !file.rename(path, backup)) stop("Cannot preserve archive state.", call. = FALSE)
  if (!file.rename(temporary, path)) {
    if (file.exists(backup)) file.rename(backup, path)
    stop("Cannot install complete archive state.", call. = FALSE)
  }
  invisible(TRUE)
}
.build_release_archive <- function(plan, unit, part, assets, base) {
  if (!requireNamespace("zip", quietly = TRUE)) stop("Install the suggested 'zip' package to build publication archives.", call. = FALSE)
  members <- lapply(assets, function(x) list(id = x$id, filename = x$filename, size = x$size, sha256 = x$sha256, md5 = x$md5))
  if (any(!vapply(assets, function(x) .portable_filename(x$filename), logical(1))) ||
      anyDuplicated(tolower(vapply(assets, `[[`, character(1), "filename"))))
    stop("Archives require unique, flat, portable ASCII member filenames.", call. = FALSE)
  suffix <- if (assets[[1]]$kind == "data") {
    conditions <- sort(unique(unlist(lapply(assets, `[[`, "condition_ids"))))
    sprintf("--c%04d-%04d", min(conditions), max(conditions))
  } else ""
  filename <- paste0(unit, suffix, "--", plan$catalog$release, if (part > 1L) sprintf("--part-%03d", part), ".zip")
  if (!.portable_filename(filename)) stop("Download unit cannot form a portable ZIP filename.", call. = FALSE)
  directory <- file.path(plan$state_directory, "archives"); dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(directory, filename); descriptor_path <- paste0(path, ".json")
  fingerprint <- digest::digest(list(members, plan$catalog$package_version), algo = "sha256")
  if (file.exists(descriptor_path)) {
    descriptor <- tryCatch(jsonlite::read_json(descriptor_path), error = function(error) NULL)
    if (!is.null(descriptor)) {
      if (!identical(descriptor$build_fingerprint, fingerprint) || !.file_verified(path, descriptor$sha256, descriptor$size, descriptor$md5))
        stop("Persisted archive differs from this plan: ", filename, call. = FALSE)
      descriptor$local_path <- path
      return(descriptor)
    }
  }
  recovered <- file.exists(path)
  if (recovered) {
    # The ZIP was installed before an interrupted manifest write. Keep those
    # exact bytes, recovering state only after every member matches this recipe.
    .verify_local_archive_members(path, members)
  } else {
    stage <- tempfile("zip-members-", tmpdir = directory); dir.create(stage)
    on.exit(unlink(stage, recursive = TRUE), add = TRUE)
    for (asset in assets) {
      source <- .archive_source(asset, base)
      if (nzchar(Sys.readlink(source))) stop("Symbolic-link inputs are not permitted.", call. = FALSE)
      staged <- file.path(stage, asset$filename)
      if (!file.copy(source, staged) || !.file_verified(staged, asset$sha256, asset$size, asset$md5))
        stop("Source changed while staging ZIP member: ", asset$filename, call. = FALSE)
    }
    temporary <- tempfile("archive-", tmpdir = directory, fileext = ".zip")
    on.exit(unlink(temporary), add = TRUE)
    zip::zipr(temporary, files = vapply(assets, `[[`, character(1), "filename"), root = stage, include_directories = FALSE)
    .zip_inventory(temporary, members)
    if (!file.rename(temporary, path)) stop("Cannot persist publication ZIP.", call. = FALSE)
  }
  descriptor <- list(id = paste0(unit, "--", plan$catalog$release, "--", part), unit = unit, part = part,
    dgm = assets[[1]]$dgm, kind = if (identical(assets[[1]]$measure, "pairwise")) "pairwise" else assets[[1]]$kind,
    filename = filename, record_id = "0", size = as.numeric(file.info(path)$size),
    sha256 = digest::digest(file = path, algo = "sha256", serialize = FALSE), md5 = unname(tools::md5sum(path)),
    uncompressed_size = sum(vapply(assets, `[[`, numeric(1), "size")), members = members,
    packaging_version = plan$catalog$package_version, first_release = plan$catalog$release,
    manifest_recovered = recovered, build_fingerprint = fingerprint)
  .write_archive_descriptor(descriptor, descriptor_path)
  descriptor$local_path <- path
  descriptor
}
.plan_archive_release <- function(catalog, base, files, replace, metadata, state_directory,
                                  max_files, max_bytes, community, catalog_record_id, catalog_concept_doi, max_archive_bytes) {
  if (!.scalar_string(community)) stop("Archive publication requires a community slug or UUID.", call. = FALSE)
  if (!is.numeric(max_archive_bytes) || length(max_archive_bytes) != 1L || !is.finite(max_archive_bytes) ||
      max_archive_bytes <= 0 || max_archive_bytes > .archive_byte_limit) stop("Invalid uncompressed archive cap.", call. = FALSE)
  for (asset in catalog$assets) for (identifier in c(asset$dgm, asset$method, asset$method_setting))
    if (!.portable_filename(identifier) || grepl("--", identifier, fixed = TRUE)) stop("DGM, method and setting identifiers must be portable and cannot contain '--'.", call. = FALSE)
  if (!is.null(base)) {
    for (dgm in names(base$conditions)) {
      old <- .catalog_conditions(base, dgm); current <- .catalog_conditions(catalog, dgm)
      index <- match(old$condition_id, current$condition_id)
      if (nrow(old) != nrow(current) || anyNA(index) || !setequal(names(old), names(current)) ||
          !identical(jsonlite::toJSON(old, dataframe = "rows", digits = NA),
            jsonlite::toJSON(current[index, names(old), drop = FALSE], dataframe = "rows", digits = NA)))
        stop("New conditions are not allowed for a published DGM; create a new DGM.", call. = FALSE)
    }
    old_ids <- vapply(base$assets, `[[`, character(1), "id")
    for (asset in catalog$assets) if (asset$dgm %in% names(base$conditions) && asset$kind %in% c("data", "metadata", "archive") && !asset$id %in% old_ids)
      stop("New data, metadata or source archive assets are not allowed for a published DGM.", call. = FALSE)
    .validate_release_dependencies(catalog$assets, base, files, replace)
  }
  state_directory <- normalizePath(state_directory, winslash = "/", mustWork = FALSE)
  dir.create(state_directory, recursive = TRUE, showWarnings = FALSE)
  catalog$schema_version <- 2L
  plan <- list(catalog = catalog, previous = base, metadata = metadata, state_directory = state_directory,
    sandbox = isTRUE(catalog$sandbox), community = community, max_files = max_files, max_bytes = max_bytes,
    catalog_record_id = catalog_record_id, catalog_concept_doi = catalog_concept_doi,
    max_archive_bytes = max_archive_bytes)
  if (is.null(plan$catalog_record_id) && !is.null(base$publication$catalog_record_id)) plan$catalog_record_id <- base$publication$catalog_record_id
  if (is.null(plan$catalog_concept_doi) && !is.null(base$publication$catalog_concept_doi)) plan$catalog_concept_doi <- base$publication$catalog_concept_doi
  if (!is.null(base) && base$schema_version == 1L && is.null(plan$catalog_record_id))
    stop("Consolidation requires the existing catalog_record_id and catalog_concept_doi.", call. = FALSE)
  if (!is.null(plan$catalog_record_id) && (!grepl("^[0-9]+$", plan$catalog_record_id) || !.scalar_string(plan$catalog_concept_doi)))
    stop("An existing catalog family requires its record ID and concept DOI.", call. = FALSE)
  fingerprint <- digest::digest(plan, algo = "sha256")
  plan_path <- file.path(state_directory, "plan.rds")
  if (file.exists(plan_path)) {
    saved <- readRDS(plan_path)
    if (!identical(saved$fingerprint, fingerprint)) stop("Existing publication state belongs to a different plan.", call. = FALSE)
    for (archive in Filter(function(x) !is.null(x$local_path), saved$catalog$archives))
      if (!.file_verified(archive$local_path, archive$sha256, archive$size, archive$md5)) stop("Persisted archive changed.", call. = FALSE)
    return(saved)
  }
  units <- split(catalog$assets, vapply(catalog$assets, .archive_unit, character(1)))
  archives <- list(); changed <- character()
  for (unit in names(units)) {
    assets <- units[[unit]]
    previous_archives <- if (!is.null(base) && base$schema_version == 2L) Filter(function(x) identical(x$unit, unit), base$archives) else list()
    previous_assets <- if (length(previous_archives)) Filter(function(x) .archive_unit(x) == unit, base$assets) else list()
    signature <- function(xs) stats::setNames(vapply(xs, `[[`, character(1), "sha256"), vapply(xs, `[[`, character(1), "id"))
    unchanged <- identical(signature(previous_assets)[sort(names(signature(previous_assets)))], signature(assets)[sort(names(signature(assets)))])
    if (length(previous_archives) && unchanged) {
      archives <- c(archives, previous_archives)
      next
    }
    changed <- c(changed, assets[[1]]$dgm)
    if (assets[[1]]$kind == "data" && length(previous_archives)) {
      # Keep established data chunk membership; split only an affected chunk
      # that has grown beyond the cap after an explicit correction.
      parts <- list(); remaining <- assets
      for (old in previous_archives) {
        member_ids <- vapply(old$members, `[[`, character(1), "id")
        group <- Filter(function(x) x$id %in% member_ids, remaining)
        old_hashes <- stats::setNames(vapply(old$members, `[[`, character(1), "sha256"), member_ids)
        new_hashes <- signature(group)
        if (identical(old_hashes[sort(names(old_hashes))], new_hashes[sort(names(new_hashes))])) {
          archives[[length(archives) + 1L]] <- old
        } else if (length(group)) parts <- c(parts, .split_archive_members(group, max_archive_bytes, data = TRUE))
        remaining <- Filter(function(x) !x$id %in% member_ids, remaining)
      }
      if (length(remaining)) stop("Unexpected new dataset members in a frozen DGM.", call. = FALSE)
    } else parts <- .split_archive_members(assets, max_archive_bytes, data = assets[[1]]$kind == "data")
    for (i in seq_along(parts)) archives[[length(archives) + 1L]] <- .build_release_archive(plan, unit, i, parts[[i]], base)
  }
  # Every archive in a changed DGM will be imported/rebound to one new version.
  changed <- unique(changed)
  archives <- lapply(archives, function(x) { if (x$dgm %in% changed) x$record_id <- "0"; x })
  catalog$archives <- archives
  catalog$assets <- lapply(catalog$assets, function(asset) {
    matches <- Filter(function(a) any(vapply(a$members, function(m) identical(m$id, asset$id) && identical(m$sha256, asset$sha256), logical(1))), archives)
    if (length(matches) != 1L) stop("Asset does not map to exactly one ZIP member.", call. = FALSE)
    asset$archive_id <- matches[[1]]$id; asset$record_id <- matches[[1]]$record_id
    if (!length(asset$dependencies) && !is.null(base) &&
        (isTRUE(asset$replacement) || asset$kind == "pairwise" || identical(asset$measure, "pairwise"))) {
      old <- Filter(function(x) identical(x$id, asset$id), base$assets)
      if (length(old) && !(asset$id %in% replace && asset$id %in% vapply(files, `[[`, character(1), "id")))
        asset$dependencies <- .asset_inputs(old[[1]], base$assets)
    }
    asset$dependencies <- .asset_inputs(asset, catalog$assets)
    asset
  })
  plan$catalog <- .validate_catalog(catalog); plan$changed_dgms <- changed
  plan$groups <- lapply(changed, function(dgm) Filter(function(x) x$dgm == dgm && !is.null(x$local_path), archives))
  names(plan$groups) <- changed
  plan$fingerprint <- fingerprint
  report <- benchmark_packing_report(plan)
  report_path <- file.path(state_directory, "packing-report.csv")
  utils::write.csv(report, report_path, row.names = FALSE)
  over <- report$files > max_files | report$compressed_bytes > max_bytes
  if (any(over)) stop("DGM packing exceeds the configured quota: ", paste(sprintf("%s (%d/%d files, %.0f/%.0f bytes)",
    report$dgm[over], report$files[over], max_files, report$compressed_bytes[over], max_bytes), collapse = "; "),
    ". Packing report: ", report_path, call. = FALSE)
  saveRDS(plan, plan_path)
  audit <- do.call(rbind, lapply(catalog$assets, function(x) {
    old <- if (is.null(base)) list() else Filter(function(a) identical(a$id, x$id), base$assets)
    data.frame(id = x$id, source_record_id = if (length(old)) old[[1]]$record_id else NA_character_,
      source_filename = if (length(old)) old[[1]]$filename else NA_character_, archive_id = x$archive_id,
      member = x$filename, sha256 = x$sha256, size = x$size, stringsAsFactors = FALSE)
  }))
  utils::write.csv(audit, file.path(state_directory, "asset-audit.csv"), row.names = FALSE)
  plan
}

#' Inspect a Benchmark Publication Packing Report
#' @param plan A plan returned by plan_benchmark_release.
#' @return A data frame with per-DGM physical file counts, compressed and
#' uncompressed sizes, upload volume and remaining record quota.
#' @export
benchmark_packing_report <- function(plan) {
  archives <- plan$catalog$archives
  if (!length(archives)) stop("A packing report requires an archive publication plan.", call. = FALSE)
  do.call(rbind, lapply(split(archives, vapply(archives, `[[`, character(1), "dgm")), function(xs) {
    bytes <- sum(vapply(xs, `[[`, numeric(1), "size"))
    uploads <- Filter(function(x) !is.null(x$local_path), xs)
    data.frame(dgm = xs[[1]]$dgm, files = length(xs), compressed_bytes = bytes,
      uncompressed_bytes = sum(vapply(xs, function(x) sum(vapply(x$members, `[[`, numeric(1), "size")), numeric(1))),
      upload_bytes = sum(vapply(uploads, `[[`, numeric(1), "size")), free_files = plan$max_files - length(xs), free_bytes = plan$max_bytes - bytes)
  }))
}
