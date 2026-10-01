#' Update a Benchmark Community's About and Curation Pages
#' @description Explicitly update the community's HTML pages while preserving
#' its identity, other metadata, submission policies and membership settings.
#' This is separate from release publication; review and approve production
#' page content before calling it. Sandbox uses a separate account and token.
#' @param community Community slug or UUID.
#' @param about HTML for the About page.
#' @param curation_policy HTML for the curation policy page.
#' @param sandbox Use sandbox.zenodo.org.
#' @param token Zenodo token; defaults to the matching environment variable.
#' @return The verified updated community metadata, invisibly.
#' @export
update_benchmark_community_pages <- function(community, about, curation_policy, sandbox = FALSE, token = NULL) {
  if (!.scalar_string(community) || !.scalar_string(about) || !.scalar_string(curation_policy) ||
      nchar(about) > 50000L || nchar(curation_policy) > 50000L)
    stop("Provide a community and nonempty page HTML of at most 50,000 characters each.", call. = FALSE)
  token <- .publication_token(list(sandbox = sandbox), token)
  record <- .zenodo_request("GET", paste0("communities/", utils::URLencode(community, reserved = TRUE)), token, sandbox)
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
