# Archive members deliberately use the same content-addressed cache as schema 1.
.archive_byte_limit <- 2000000000
.portable_filename <- function(x) .safe_filename(x) && grepl("^[ -~]+$", x) && nchar(x, type = "bytes") <= 240L
.catalog_archive <- function(catalog, id) {
  matches <- Filter(function(x) identical(x$id, id), catalog$archives)
  if (length(matches) != 1L) stop("Unknown or duplicate archive reference: ", id, call. = FALSE)
  matches[[1]]
}
.archive_cache_path <- function(archive) file.path(.get_path(), "archives", archive$sha256, archive$filename)
.resource_reference_url <- function(catalog, asset) {
  reference <- if (is.null(asset$archive_id)) asset else .catalog_archive(catalog, asset$archive_id)
  .zenodo_file_url(reference$record_id, reference$filename, isTRUE(catalog$sandbox))
}
.validate_archive_catalog <- function(catalog) {
  if (!is.list(catalog$archives) || !length(catalog$archives)) stop("Schema 2 requires an archive inventory.", call. = FALSE)
  ids <- vapply(catalog$archives, function(x) if (.scalar_string(x$id)) x$id else "", character(1))
  if (any(!nzchar(ids)) || anyDuplicated(ids)) stop("Invalid or duplicate archive IDs.", call. = FALSE)
  for (archive in catalog$archives) {
    if (!.scalar_string(archive$dgm) || !.portable_filename(archive$filename) ||
        !.scalar_string(archive$record_id) || !grepl("^[0-9]+$", archive$record_id) ||
        !.scalar_string(archive$sha256) || !grepl("^[a-f0-9]{64}$", archive$sha256) ||
        !.scalar_string(archive$md5) || !grepl("^[a-f0-9]{32}$", archive$md5) ||
        !is.numeric(archive$size) || length(archive$size) != 1L || !is.finite(archive$size) || archive$size <= 0 ||
        !length(archive$members)) stop("Invalid physical archive descriptor.", call. = FALSE)
    names <- vapply(archive$members, function(x) if (.portable_filename(x$filename)) x$filename else "", character(1))
    if (any(!nzchar(names)) || anyDuplicated(tolower(names))) stop("Unsafe or colliding archive members.", call. = FALSE)
    for (member in archive$members) {
      if (!.scalar_string(member$sha256) || !grepl("^[a-f0-9]{64}$", member$sha256) ||
          !.scalar_string(member$md5) || !grepl("^[a-f0-9]{32}$", member$md5) ||
          !is.numeric(member$size) || length(member$size) != 1L || !is.finite(member$size) || member$size < 0)
        stop("Invalid archive member inventory.", call. = FALSE)
    }
    if (sum(vapply(archive$members, `[[`, numeric(1), "size")) > .archive_byte_limit)
      stop("Archive exceeds the 2,000,000,000-byte uncompressed cap.", call. = FALSE)
  }
  for (asset in catalog$assets) {
    if (!.scalar_string(asset$archive_id)) stop("Schema 2 assets need archive references.", call. = FALSE)
    archive <- .catalog_archive(catalog, asset$archive_id)
    member <- Filter(function(x) identical(x$filename, asset$filename), archive$members)
    if (!identical(archive$dgm, asset$dgm) || !identical(archive$unit, .archive_unit(asset)) ||
        !identical(archive$record_id, asset$record_id) || length(member) != 1L ||
        !identical(member[[1]]$sha256, asset$sha256) || !identical(member[[1]]$md5, asset$md5) || member[[1]]$size != asset$size)
      stop("Logical asset differs from its archive member inventory.", call. = FALSE)
  }
  for (dgm in unique(vapply(catalog$archives, `[[`, character(1), "dgm"))) {
    archives <- Filter(function(x) x$dgm == dgm, catalog$archives)
    records <- unique(vapply(archives, `[[`, character(1), "record_id"))
    if (length(records) != 1L) stop("A release must use one storage version per DGM.", call. = FALSE)
    if (anyDuplicated(tolower(vapply(archives, `[[`, character(1), "filename"))))
      stop("Colliding physical archive filenames.", call. = FALSE)
  }
  asset_ids <- vapply(catalog$assets, `[[`, character(1), "id")
  for (asset in catalog$assets) for (input in asset$dependencies) {
    if (!is.list(input) || !.scalar_string(input$id) || !input$id %in% asset_ids)
      stop("Invalid computation input reference.", call. = FALSE)
    source <- catalog$assets[[match(input$id, asset_ids)]]
    kind <- if (asset$kind == "results") "data" else if (asset$kind %in% c("measures", "pairwise")) "results" else NULL
    if (!identical(source$dgm, asset$dgm) || !identical(source$kind, kind) || !identical(source$sha256, input$sha256))
      stop("Invalid or stale computation input hash.", call. = FALSE)
  }
  invisible(TRUE)
}

# Inspect the central directory ourselves: utils::unzip(list = TRUE) does not
# expose Unix file types. Only flat ASCII regular files using store/deflate and
# non-ZIP64, single-disk archives are accepted by the portable reader.
.zip_inventory <- function(path, members) {
  stream <- file(path, "rb"); on.exit(close(stream), add = TRUE)
  bytes <- as.numeric(file.info(path)$size)
  fail <- function() stop("Unsafe, unsupported, or corrupt ZIP archive.", call. = FALSE)
  read_exact <- function(n) { x <- readBin(stream, "raw", n); if (length(x) != n) fail(); x }
  uint <- function(x, start, n) sum(as.numeric(x[start + seq_len(n) - 1L]) * 256^(seq_len(n) - 1L))
  if (!is.finite(bytes) || bytes < 22) fail()
  tail_size <- min(bytes, 65557); seek(stream, bytes - tail_size, origin = "start")
  tail <- read_exact(tail_size)
  candidates <- which(tail == as.raw(0x50)); end <- NULL
  for (i in rev(candidates)) if (i + 21 <= length(tail) &&
      identical(tail[i + 0:3], as.raw(c(0x50, 0x4b, 0x05, 0x06))) && i + 21 + uint(tail, i + 20, 2) == length(tail)) {
    end <- tail[i + 0:21]; break
  }
  if (is.null(end) || uint(end, 5, 2) != 0 || uint(end, 7, 2) != 0) fail()
  count <- uint(end, 11, 2); directory_size <- uint(end, 13, 4); offset <- uint(end, 17, 4)
  if (count == 65535 || uint(end, 9, 2) != count || count != length(members) || offset + directory_size > bytes - 22) fail()
  seek(stream, offset, origin = "start"); inventory <- list()
  for (i in seq_len(count)) {
    header <- read_exact(46)
    if (!identical(header[1:4], as.raw(c(0x50, 0x4b, 0x01, 0x02)))) fail()
    flags <- uint(header, 9, 2); compression <- uint(header, 11, 2)
    size <- uint(header, 25, 4); compressed <- uint(header, 21, 4)
    name_length <- uint(header, 29, 2); extra <- uint(header, 31, 2); comment <- uint(header, 33, 2)
    name_raw <- read_exact(name_length)
    if (any(as.integer(name_raw) < 32 | as.integer(name_raw) > 126)) fail()
    name <- rawToChar(name_raw); mode <- floor(uint(header, 39, 4) / 65536)
    type <- bitwAnd(as.integer(mode), 61440L)
    if (!.portable_filename(name) || bitwAnd(as.integer(flags), 1L) != 0 || !compression %in% c(0, 8) ||
        !type %in% c(0L, 32768L) || uint(header, 35, 2) != 0 || size > .archive_byte_limit ||
        uint(header, 43, 4) + 30 + name_length + compressed > offset) fail()
    read_exact(extra + comment)
    inventory[[i]] <- list(filename = name, size = size, local_offset = uint(header, 43, 4))
  }
  if (seek(stream) != offset + directory_size) fail()
  names <- vapply(inventory, `[[`, character(1), "filename")
  if (anyDuplicated(tolower(names))) fail()
  expected <- vapply(members, `[[`, character(1), "filename")
  if (!setequal(names, expected)) fail()
  for (entry in inventory) {
    member <- members[[match(entry$filename, expected)]]
    if (entry$size != member$size) fail()
    seek(stream, entry$local_offset, origin = "start"); local <- read_exact(30)
    if (!identical(local[1:4], as.raw(c(0x50, 0x4b, 0x03, 0x04))) ||
        uint(local, 27, 2) != nchar(entry$filename, type = "bytes") ||
        !identical(rawToChar(read_exact(uint(local, 27, 2))), entry$filename)) fail()
  }
  invisible(TRUE)
}

.install_member <- function(source, asset) {
  destination <- .asset_cache_path(asset)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  # Copy onto the destination filesystem before renaming; tempdir may be on a
  # different drive. Retain a corrupt previous copy until the replacement is ready.
  staged <- tempfile("member-", tmpdir = dirname(destination)); backup <- paste0(staged, ".previous")
  on.exit(unlink(c(staged, backup)), add = TRUE)
  if (!file.copy(source, staged) || !.file_verified(staged, asset$sha256, asset$size, asset$md5))
    stop("Archive member failed size or hash verification: ", asset$filename, call. = FALSE)
  if (file.exists(destination) && !file.rename(destination, backup)) stop("Cannot replace cached member.", call. = FALSE)
  if (!file.rename(staged, destination)) {
    if (file.exists(backup)) file.rename(backup, destination)
    stop("Cannot install verified archive member.", call. = FALSE)
  }
  invisible(TRUE)
}
.extract_archive_members <- function(path, archive, assets) {
  .zip_inventory(path, archive$members)
  directory <- tempfile("extract-"); dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  for (asset in assets) {
    extracted <- utils::unzip(path, files = asset$filename, exdir = directory, junkpaths = TRUE, unzip = "internal")
    target <- file.path(directory, asset$filename)
    if (length(extracted) != 1L || !.file_verified(target, asset$sha256, asset$size, asset$md5))
      stop("Archive member failed size or hash verification: ", asset$filename, call. = FALSE)
    .install_member(target, asset); unlink(target)
  }
  invisible(TRUE)
}
.pending_downloads <- function(catalog, assets, overwrite = FALSE) {
  pending <- Filter(function(x) overwrite || !.file_verified(.asset_cache_path(x), x$sha256, x$size, x$md5), assets)
  ids <- unique(vapply(Filter(function(x) !is.null(x$archive_id), pending), `[[`, character(1), "archive_id"))
  archives <- lapply(ids, function(id) .catalog_archive(catalog, id))
  transfers <- Filter(function(a) overwrite || !.file_verified(.archive_cache_path(a), a$sha256, a$size, a$md5), archives)
  direct <- Filter(function(x) is.null(x$archive_id), pending)
  list(assets = pending, archives = archives, bytes = sum(vapply(c(direct, transfers), `[[`, numeric(1), "size")),
       files = length(direct) + length(transfers))
}
.download_catalog_assets <- function(catalog, assets, progress = TRUE, max_try = 10, overwrite = FALSE) {
  pending <- .pending_downloads(catalog, assets, overwrite)
  for (asset in Filter(function(x) is.null(x$archive_id), pending$assets))
    .fetch_verified(.resource_reference_url(catalog, asset), .asset_cache_path(asset), asset$sha256, asset$size, asset$md5,
                    progress, max_try, overwrite)
  for (archive in pending$archives) {
    path <- .archive_cache_path(archive)
    .fetch_verified(.zenodo_file_url(archive$record_id, archive$filename, isTRUE(catalog$sandbox)), path,
                    archive$sha256, archive$size, archive$md5, progress, max_try, overwrite)
    .extract_archive_members(path, archive, Filter(function(x) identical(x$archive_id, archive$id), pending$assets))
  }
  invisible(TRUE)
}

#' Remove Cached Benchmark Archives
#' @description Remove ZIP copies whose selected catalog members all have verified
#' extracted copies. Logical member files and historical catalogs are retained.
#' @inheritParams benchmark_catalog
#' @param dry_run Report removable archives without deleting them.
#' @return A data frame with archive IDs, paths, byte sizes and removal status.
#' @export
prune_benchmark_archives <- function(release = NULL, dgm_name = NULL, dry_run = TRUE) {
  catalog <- benchmark_catalog(release)
  archives <- Filter(function(x) is.null(dgm_name) || x$dgm %in% dgm_name, catalog$archives)
  rows <- lapply(archives, function(a) {
    selected <- Filter(function(x) identical(x$archive_id, a$id), catalog$assets)
    path <- .archive_cache_path(a)
    removable <- length(selected) > 0L && file.exists(path) && all(vapply(selected, function(x)
      .file_verified(.asset_cache_path(x), x$sha256, x$size, x$md5), logical(1)))
    removed <- removable && !dry_run && unlink(path) == 0L
    data.frame(archive_id = a$id, path = path, size = a$size, removable = removable, removed = removed)
  })
  if (!length(rows)) return(data.frame(archive_id = character(), path = character(), size = numeric(), removable = logical(), removed = logical()))
  do.call(rbind, rows)
}
