# Publish the verified baseline and pin it only after public verification succeeds.
# Run from the repository root. ZENODO_TOKEN must be configured locally.
if (file.exists(".Renviron")) readRenviron(".Renviron")
pkgload::load_all(quiet = TRUE)
plan <- readRDS("resources/migration/publication/plan.rds")
entry <- publish_benchmark_release(plan)
registry_path <- "inst/extdata/benchmark-releases.json"
registry <- jsonlite::read_json(registry_path)
other <- Filter(function(x) !identical(x$release, entry$release), registry$releases)
registry$releases <- c(other, list(entry))
registry$default_release <- entry$release
jsonlite::write_json(registry, registry_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
cat("Verified and pinned release ", entry$release, " at https://doi.org/", entry$doi, "\n", sep = "")
