# Audit a prepared baseline before publication; never modifies source files.
library(data.table)
setDTthreads(2L)
pkgload::load_all(quiet = TRUE)
plan <- readRDS("resources/migration/publication/plan.rds")
catalog <- plan$catalog
inventory <- jsonlite::read_json("resources/migration/inventory.json")
originals <- Filter(function(x) is.null(x$derived_from) && !is.null(x$source), catalog$assets)
stopifnot(length(originals) == length(inventory$assets), !anyDuplicated(vapply(originals, `[[`, "", "id")))
by_id <- setNames(originals, vapply(originals, `[[`, "", "id"))
for (original in inventory$assets) {
  asset <- by_id[[paste(original$dgm, original$path, sep = "/")]]
  stopifnot(identical(asset$sha256, original$sha256), identical(asset$md5, original$md5), asset$size == original$size)
}
for (asset in Filter(function(x) !is.null(x$correction), catalog$assets)) {
  original <- fread(file.path("resources/migration/source", asset$dgm, asset$source$path))
  corrected <- fread(asset$local_path)
  stopifnot(all(corrected$method_setting == "example"))
  original$method_setting <- "example"
  stopifnot(isTRUE(all.equal(original, corrected, tolerance = 0)))
}
comparisons <- 0L
for (asset in Filter(function(x) x$kind == "measures", catalog$assets)) {
  prepared <- fread(asset$local_path)
  for (metric in unlist(asset$measures)) {
    filename <- paste0(metric, if (isTRUE(asset$replacement)) "-replacement", ".csv")
    source <- fread(file.path("resources/migration/source", asset$dgm, "measures", filename))
    source <- source[method == asset$method & method_setting == asset$method_setting]
    auxiliary <- intersect(c("n_valid", "replaced"), names(source))
    setnames(source, auxiliary, paste0(auxiliary, "_", metric))
    expected <- source[order(condition_id)]
    actual <- prepared[condition_id %in% source$condition_id][order(condition_id), names(source), with = FALSE]
    missing <- prepared[!condition_id %in% source$condition_id]
    metric_columns <- setdiff(names(source), c("method", "method_setting", "condition_id"))
    stopifnot(all(is.na(missing[, ..metric_columns])))
    stopifnot(nrow(expected) == nrow(actual))
    for (column in names(expected)) {
      # CSV readers infer logical for a column that is entirely missing after
      # partitioning. Compare values while allowing that inference difference.
      if (all(is.na(expected[[column]])) && all(is.na(actual[[column]]))) next
      comparison <- all.equal(expected[[column]], actual[[column]], tolerance = 1e-12, check.attributes = FALSE)
      if (!isTRUE(comparison)) stop(asset$id, ": ", metric, "/", column, ": ", paste(comparison, collapse = "; "))
    }
    comparisons <- comparisons + 1L
  }
}
invisible(PublicationBiasBenchmark:::.validate_catalog(catalog))
PublicationBiasBenchmark:::.validate_plan_coverage(catalog$assets)
roundtrip <- tempfile(fileext = ".json")
jsonlite::write_json(catalog, roundtrip, auto_unbox = TRUE, dataframe = "rows", null = "null", digits = NA)
parsed <- benchmark_catalog(roundtrip)
for (dgm in names(catalog$conditions))
  stopifnot(isTRUE(all.equal(PublicationBiasBenchmark:::.catalog_conditions(parsed, dgm),
                            PublicationBiasBenchmark:::.catalog_conditions(catalog, dgm), check.attributes = FALSE)))
unlink(roundtrip)
PublicationBiasBenchmark.options(resources_directory = "resources/migration/reader-smoke")
selected <- Filter(function(x) x$dgm == "no_bias" && ((x$kind == "data" && identical(x$condition_ids, list(1L))) ||
  (x$kind %in% c("results", "measures") && x$method == "RMA" && x$method_setting == "default")), catalog$assets)
for (asset in selected) {
  path <- PublicationBiasBenchmark:::.asset_cache_path(asset)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  file.copy(asset$local_path, path, overwrite = TRUE)
}
stopifnot(nrow(retrieve_dgm_dataset("no_bias", 1, 1, release = catalog)) == 10L)
stopifnot(nrow(retrieve_dgm_results("no_bias", "RMA", "default", 1, 1, release = catalog)) == 1L)
stopifnot(nrow(retrieve_dgm_measures("no_bias", "bias", "RMA", "default", 1, release = catalog)) == 1L)
report <- list(source_files = length(originals), source_bytes = sum(vapply(originals, `[[`, 0, "size")),
  catalog_assets = length(catalog$assets), component_records = length(plan$groups),
  measure_comparisons = comparisons, label_corrections = 4L, reader_smoke = "passed")
jsonlite::write_json(report, "resources/migration/audit.json", auto_unbox = TRUE, pretty = TRUE)
print(report)
