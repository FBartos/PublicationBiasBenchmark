#' Update a Benchmark Community's About and Curation Pages
#' @description Explicitly update the community's HTML pages while preserving
#' its identity, other metadata, submission policies and membership settings.
#' This is separate from release publication; review and approve production
#' page content before calling it. Sandbox uses a separate account and token.
#' The function is maintainer-only and not exported
#' (`PublicationBiasBenchmark:::update_benchmark_community_pages()`). It requires
#' `confirm`, checks that the token's account owns the community, and saves the
#' current pages to `backup_directory` before replacing them.
#' @param community Community slug or UUID.
#' @param about HTML for the About page.
#' @param curation_policy HTML for the curation policy page.
#' @param sandbox Use sandbox.zenodo.org.
#' @param token Zenodo token; defaults to the matching environment variable.
#' @param confirm Required: the `community` value, repeated explicitly.
#' @param backup_directory Directory that receives a verified JSON copy of the
#' current pages before they are replaced.
#' @return The verified updated community metadata, invisibly.
#' @keywords internal
update_benchmark_community_pages <- function(community, about, curation_policy, sandbox = FALSE, token = NULL,
                                             confirm = NULL,
                                             backup_directory = tools::R_user_dir("PublicationBiasBenchmark", "data")) {
  if (!.scalar_string(community) || !.scalar_string(about) || !.scalar_string(curation_policy) ||
      nchar(about) > 50000L || nchar(curation_policy) > 50000L)
    stop("Provide a community and nonempty page HTML of at most 50,000 characters each.", call. = FALSE)
  if (!(.scalar_string(confirm) && identical(confirm, community)))
    stop("Replacing community pages is irreversible: re-run with confirm = \"", community, "\"", call. = FALSE)
  token <- .publication_token(list(sandbox = sandbox), token)
  community_id <- .require_community_maintainer(community, token, sandbox, "owner")
  record <- .zenodo_request("GET", paste0("communities/", community_id), token, sandbox)
  # The previous pages are saved (verified) before anything is replaced.
  backup <- file.path(backup_directory, paste0("community-pages-", community_id, "-",
                                               format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"), ".json"))
  tryCatch(.write_json_verified(backup, list(community_id = community_id, saved = .utc_now(),
      page = record$metadata$page, curation_policy = record$metadata$curation_policy), pretty = TRUE),
    error = function(error) stop("Cannot save a backup of the current community pages (", conditionMessage(error),
                                 "); the pages were not changed.", call. = FALSE))
  metadata <- record$metadata; metadata$page <- about; metadata$curation_policy <- curation_policy
  body <- list(slug = record$slug, metadata = metadata, access = record$access)
  if (length(record$custom_fields)) body$custom_fields <- record$custom_fields
  .zenodo_request("PUT", paste0("communities/", record$id), token, sandbox, body)
  result <- .zenodo_request("GET", paste0("communities/", record$id), token, sandbox)
  if (!identical(result$id, record$id) || !identical(result$access, record$access) ||
      !.scalar_string(result$metadata$page) || !.scalar_string(result$metadata$curation_policy))
    stop("Community page update did not preserve its identity/policies or retain its pages.", call. = FALSE)
  invisible(result)
}
