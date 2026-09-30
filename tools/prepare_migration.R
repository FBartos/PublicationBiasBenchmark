# Prepare a faithful OSF baseline plus measures partitioned by method.
# Run Rscript --vanilla tools/prepare_migration.R from the repository root.
# data.table is used only by this maintainer script, not by package readers.
library(data.table)
setDTthreads(2L)
library(jsonlite)
library(digest)
for (path in list.files("R", pattern = "\\.R$", full.names = TRUE)) sys.source(path, envir = .GlobalEnv)
state <- "resources/migration"
inventory <- read_json(file.path(state, "inventory.json"), simplifyVector = FALSE)
source_root <- file.path(state, "source")
prepared_root <- file.path(state, "prepared")
dir.create(prepared_root, recursive = TRUE, showWarnings = FALSE)
conditions <- list(); assets <- list(); checks <- list()
package_version <- read.dcf("DESCRIPTION")[1, "Version"]

add_asset <- function(asset, path) {
  asset$filename <- paste(asset$dgm, asset$kind, basename(path), sep = "__")
  asset$local_path <- normalizePath(path, winslash = "/")
  asset$record_id <- "0"
  assets[[length(assets) + 1L]] <<- asset
}

for (dgm in names(inventory$nodes)) {
  conditions[[dgm]] <- dgm_conditions(dgm)
  expected_conditions <- conditions[[dgm]]$condition_id
  observed_conditions <- integer()
  for (original in Filter(function(x) x$dgm == dgm, inventory$assets)) {
    path <- file.path(source_root, dgm, original$path)
    if (!.file_verified(path, original$sha256, original$size, original$md5)) stop("Source file changed: ", path)
    asset <- list(id = paste(dgm, original$path, sep = "/"), dgm = dgm,
                  kind = if (original$kind == "measures") "archive" else original$kind,
                  size = original$size, sha256 = original$sha256, md5 = original$md5,
                  package_version = NULL, source = original)
    if (original$kind %in% c("data", "results")) {
      table <- fread(path, showProgress = FALSE)
      if (!"repetition_id" %in% names(table) || anyNA(table$repetition_id)) stop("Invalid repetition IDs: ", path)
      if (original$kind == "data") {
        session <- file.path(source_root, dgm, "metadata", "dgm-sessionInfo.txt")
        if (file.exists(session)) {
          stamp <- grep("PublicationBiasBenchmark_[0-9]", readLines(session, warn = FALSE), value = TRUE)
          if (length(stamp)) asset$package_version <- sub(".*PublicationBiasBenchmark_([0-9.]+).*", "\\1", stamp[1])
        }
        condition <- as.integer(sub("\\.csv$", "", basename(path)))
        if (!condition %in% expected_conditions) stop("Unrecognized source condition: ", path)
        observed_conditions <- c(observed_conditions, condition)
        asset$condition_ids <- list(condition)
        asset$repetition_ids <- as.list(sort(unique(table$repetition_id)))
        asset$rows <- nrow(table)
        asset$coverage <- list(list(condition_id = condition,
          repetitions = length(unique(table$repetition_id)), ranges = .repetition_ranges(table$repetition_id)))
      } else {
        method <- unique(table$method); setting <- unique(table$method_setting)
        # Four archived WILS example files incorrectly say "default". Their
        # worker mapping, filenames, archived measures and reproduced fits all
        # identify the example setting. Retain the original bytes as an archive
        # and create an explicit label correction; numerical results are intact.
        if (identical(original$path, "results/WILS-example.csv") && identical(setting, "default")) {
          asset$kind <- "archive"
          add_asset(asset, path)
          original_id <- asset$id
          table$method_setting <- "example"
          corrected_path <- file.path(prepared_root, paste0(dgm, "__WILS-example-corrected.csv"))
          input <- file(path, "rt"); output <- file(corrected_path, "wt")
          repeat {
            lines <- readLines(input, n = 10000L, warn = FALSE)
            if (!length(lines)) break
            # Keep every numerical token exactly as archived, including precision.
            lines <- sub(',"default",([0-9]+),([0-9]+)$', ',"example",\\1,\\2', lines)
            writeLines(lines, output)
          }
          close(input); close(output)
          path <- corrected_path
          asset <- benchmark_resource(path, dgm, "results", "WILS", "example",
            package_version = NULL, id = paste(dgm, "results", "WILS-example-corrected.csv", sep = "/"))
          asset$source <- original
          asset$derived_from <- list(original_id)
          asset$correction <- "Corrected method_setting from default to example; all numerical values are unchanged."
          asset$transformed_with_package_version <- package_version
          setting <- "example"
        }
        if (length(method) != 1L || length(setting) != 1L) stop("Mixed method or setting: ", path)
        if (anyNA(table[, .(method, method_setting, condition_id, repetition_id)]) ||
            anyDuplicated(table[, .(method, method_setting, condition_id, repetition_id)])) stop("Overlapping result keys: ", path)
        if (length(setdiff(table$condition_id, expected_conditions))) stop("Unexpected result conditions: ", path)
        asset$method <- method; asset$method_setting <- setting
        asset$condition_ids <- as.list(sort(unique(table$condition_id)))
        asset$repetition_ids <- as.list(sort(unique(table$repetition_id)))
        asset$rows <- nrow(table)
        # Only infer a generation version when archived session provenance
        # explicitly covers this method/setting in its worker script.
        worker <- file.path(source_root, dgm, "metadata", "worker-results.R")
        session <- file.path(source_root, dgm, "metadata", "worker-sessionInfo.txt")
        if (file.exists(worker) && file.exists(session)) {
          script <- paste(readLines(worker, warn = FALSE), collapse = "\n")
          pattern <- paste0('"', method, '"')
          stamp <- grep("PublicationBiasBenchmark_[0-9]", readLines(session, warn = FALSE), value = TRUE)
          if (grepl(pattern, script, fixed = TRUE) && length(stamp))
            asset$package_version <- sub(".*PublicationBiasBenchmark_([0-9.]+).*", "\\1", stamp[1])
        }
        counts <- table[, .N, by = condition_id]
        asset$coverage <- lapply(seq_len(nrow(counts)), function(i) list(condition_id = counts$condition_id[i],
          repetitions = counts$N[i], ranges = .repetition_ranges(table$repetition_id[table$condition_id == counts$condition_id[i]])))
      }
      checks[[length(checks) + 1L]] <- data.frame(dgm = dgm, kind = original$kind,
        file = original$path, rows = nrow(table), repetitions = length(unique(table$repetition_id)))
      rm(table); gc(FALSE)
    }
    add_asset(asset, path)
  }
  if (!setequal(expected_conditions, observed_conditions)) stop("Missing source dataset conditions for ", dgm)

  # Preserve original condition metadata byte-for-byte, but pin the canonical
  # condition definitions from the merged source: some old metadata files were
  # misassigned or serialized incorrectly and do not describe their DGM.
  condition_path <- file.path(prepared_root, paste0(dgm, "__conditions.csv"))
  portable_conditions <- conditions[[dgm]]
  for (column in names(portable_conditions)) if (is.list(portable_conditions[[column]]))
    portable_conditions[[column]] <- vapply(portable_conditions[[column]], function(x) toJSON(x, auto_unbox = FALSE), character(1))
  write.csv(portable_conditions, condition_path, row.names = FALSE)
  canonical <- benchmark_resource(condition_path, dgm, "metadata", package_version = package_version,
                                  id = paste(dgm, "metadata", "canonical-conditions.csv", sep = "/"))
  canonical$provenance <- "Frozen condition definitions from merged PR #9; original OSF metadata is also preserved."
  add_asset(canonical, condition_path)

  for (replacement in c(FALSE, TRUE)) {
    filenames <- list.files(file.path(source_root, dgm, "measures"), pattern = "\\.csv$", full.names = TRUE)
    filenames <- filenames[grepl("replacement", basename(filenames)) == replacement]
    wide <- NULL; measure_names <- character(); metric_coverage <- list()
    for (path in filenames) {
      table <- fread(path, showProgress = FALSE)
      if (!all(c("method", "method_setting", "condition_id") %in% names(table)))
        stop("Unexpected source measures format: ", path)
      metric <- sub("(-replacement)?\\.csv$", "", basename(path))
      if (grepl("pairwise", metric)) stop("Pairwise tables require separate method-pair partitioning: ", path)
      keys <- c("method", "method_setting", "condition_id")
      if (anyNA(table[, ..keys]) || anyDuplicated(table[, ..keys])) stop("Duplicate source measure keys: ", path)
      metric_coverage[[metric]] <- split(table$condition_id, paste(table$method, table$method_setting, sep = "/"))
      auxiliary <- intersect(c("n_valid", "replaced"), names(table))
      setnames(table, auxiliary, paste0(auxiliary, "_", metric))
      wide <- if (is.null(wide)) table else merge(wide, table, by = keys, all = TRUE, sort = FALSE)
      measure_names <- c(measure_names, metric)
    }
    if (is.null(wide)) next
    for (group in split(wide, by = c("method", "method_setting"), keep.by = TRUE)) {
      method <- unique(group$method); setting <- unique(group$method_setting)
      name <- paste0(dgm, "__", method, "-", setting, if (replacement) "-replacement", ".csv")
      path <- file.path(prepared_root, name)
      write.csv(group, path, row.names = FALSE)
      asset <- benchmark_resource(path, dgm, "measures", method, setting,
                                   sort(unique(group$condition_id)), package_version = NULL,
                                   replacement = replacement, measures = measure_names,
                                   id = paste(dgm, "measures", paste0(method, "-", setting, if (replacement) "-replacement"), sep = "/"))
      asset$derived_from <- vapply(Filter(function(x) x$dgm == dgm && x$kind == "archive" &&
                                        grepl("replacement", x$source$path) == replacement, assets), `[[`, character(1), "id")
      asset$transformed_with_package_version <- package_version
      key <- paste(method, setting, sep = "/")
      asset$measure_conditions <- lapply(metric_coverage, function(x) as.list(x[[key]]))
      asset$measures <- as.list(names(Filter(length, asset$measure_conditions)))
      add_asset(asset, path)
    }
  }
  cat("Validated and prepared", dgm, "\n")
}

metadata <- list(resource_type = list(id = "dataset"), licenses = list(list(id = "cc-by-4.0")),
  creators = list(
    list(person_or_org = list(type = "personal", given_name = "František", family_name = "Bartoš", identifiers = list(list(scheme = "orcid", identifier = "0000-0002-0018-5573")))),
    list(person_or_org = list(type = "personal", given_name = "Samuel", family_name = "Pawel", identifiers = list(list(scheme = "orcid", identifier = "0000-0003-2779-320X")))),
    list(person_or_org = list(type = "personal", given_name = "Björn S.", family_name = "Siepe", identifiers = list(list(scheme = "orcid", identifier = "0000-0002-9558-4648")))),
    list(person_or_org = list(type = "personal", given_name = "Petr", family_name = "Čala"))),
  related_identifiers = list(list(identifier = "https://osf.io/exf3m/", scheme = "url", relation_type = list(id = "isderivedfrom"))),
  keywords = list("publication bias", "meta-analysis", "simulation", "benchmark", "PublicationBiasBenchmark"))
plan <- plan_benchmark_release("2026.1", assets, conditions, metadata = metadata,
                               state_directory = file.path(state, "publication"), package_version = package_version,
                               source_commit = "5dfb486", provenance = list(source = inventory$source,
                                 captured_at = inventory$captured_at, license = inventory$license,
                                 nodes = inventory$nodes))
write.csv(do.call(rbind, checks), file.path(state, "source-validation.csv"), row.names = FALSE)
cat("Prepared", length(assets), "assets in", length(plan$groups), "component records.\n")
