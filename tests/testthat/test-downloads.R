test_that("failed download verification removes rejected files for retry", {

  resource_path <- tempfile("download-resources-")
  dir.create(resource_path)
  withr::defer(unlink(resource_path, recursive = TRUE))
  withr::local_envvar(OSF_PAT = "test-only")

  local_mocked_bindings(
    .get_path = function() resource_path,
    PublicationBiasBenchmark.get_option = function(...) FALSE
  )

  osf_files <- data.frame(name = c("complete.csv", "incomplete.csv"))
  osf_files$meta <- list(list(attributes = list(size = 9)), list(attributes = list(size = 9)))
  download_calls <- list()

  local_mocked_bindings(
    osf_retrieve_node = function(...) NULL,
    osf_ls_files = function(...) osf_files,
    osf_download = function(x, path, ...) {
      download_calls[[length(download_calls) + 1L]] <<- x$name
      for (file_name in x$name) {
        contents <- if (file_name == "incomplete.csv" && length(download_calls) == 1L) "bad" else "complete"
        writeBin(charToRaw(paste0(contents, "\n")), file.path(path, file_name))
      }
    },
    .package = "osfr"
  )

  expect_error(
    .download_dgm_fun("no_bias", "measures", FALSE, FALSE, 1),
    "Could not download complete measures files after 1 attempts: incomplete.csv",
    fixed = TRUE
  )
  expect_true(file.exists(file.path(resource_path, "no_bias", "measures", "complete.csv")))
  expect_false(file.exists(file.path(resource_path, "no_bias", "measures", "incomplete.csv")))

  expect_true(.download_dgm_fun("no_bias", "measures", FALSE, FALSE, 1))
  expect_equal(download_calls[[2]], "incomplete.csv")
  expect_equal(file.info(file.path(resource_path, "no_bias", "measures", "incomplete.csv"))$size, 9)
})
