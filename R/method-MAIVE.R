maive_or_default <- function(x, default) {
  if (is.null(x)) default else x
}

maive_unique_strings <- function(x) {
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  unique(x)
}

maive_validate_input <- function(data) {
  yi       <- data[["yi"]]
  sei      <- data[["sei"]]
  ni       <- data[["ni"]]
  study_id <- data[["study_id"]]

  if (is.null(yi) || length(yi) == 0) {
    stop("Effect sizes (yi) are required for MAIVE. ",
      "The data must include a 'yi' column with effect size estimates.",
      call. = FALSE
    )
  }

  if (is.null(sei) || length(sei) == 0) {
    stop("Standard errors (sei) are required for MAIVE. ",
      "The data must include a 'sei' column with standard errors.",
      call. = FALSE
    )
  }

  if (is.null(ni) || length(ni) == 0) {
    stop("Sample sizes (ni) are required for MAIVE. ",
      "The data must include a 'ni' column with positive sample sizes. ",
      "MAIVE uses inverse sample sizes (1/N) as instruments for variance.",
      call. = FALSE
    )
  }

  if (length(yi) < 3) {
    stop("MAIVE requires at least 3 effect size estimates for reliable estimation",
      call. = FALSE
    )
  }

  if (any(is.na(yi))) {
    stop("Effect sizes (yi) contain missing values", call. = FALSE)
  }

  if (any(is.na(sei))) {
    stop("Standard errors (sei) contain missing values", call. = FALSE)
  }

  if (any(sei <= 0)) {
    stop("All standard errors must be positive (sei > 0)", call. = FALSE)
  }

  if (any(is.na(ni))) {
    stop("Sample sizes (ni) contain missing values", call. = FALSE)
  }

  if (any(ni <= 0)) {
    stop("All sample sizes must be positive (ni > 0)", call. = FALSE)
  }

  maive_data <- data.frame(
    bs = yi,
    sebs = sei,
    Ns = ni
  )

  if (!is.null(study_id)){
    if (length(unique(study_id)) >= (length(study_id) - 3)) {
      message("'study_id' input ignored because the number of studies matches the number of clusters")
    } else {
      maive_data$study_id <- study_id      
    }
  }

  list(
    yi = yi,
    sei = sei,
    ni = ni,
    study_id = study_id,
    maive_data = maive_data
  )
}

maive_normalize_settings <- function(settings) {
  defaults <- list(
    use_waive = FALSE,
    method = 3L,
    weight = 0L,
    instrument = 1L,
    studylevel = 0L,
    SE = 0L,
    AR = 1L,
    first_stage = 0L
  )

  if (is.null(settings)) {
    return(defaults)
  }

  for (name in names(defaults)) {
    defaults[[name]] <- maive_or_default(settings[[name]], defaults[[name]])
  }

  defaults
}

maive_prepare_call <- function(input, settings) {
  call_args <- maive_normalize_settings(settings)
  adjustment_notes <- character(0)

  if (is.null(input$study_id) && call_args$studylevel > 0) {
    call_args$studylevel <- 0
    adjustment_notes <- c(adjustment_notes, "studylevel auto-adjusted to 0 (no study_id)")
  }

  if (!is.null(input$study_id) && call_args$studylevel == 0) {
    n_clusters <- length(unique(input$study_id))
    if (n_clusters < length(input$study_id)) {
      call_args$studylevel <- 2
      adjustment_notes <- c(
        adjustment_notes,
        "studylevel auto-adjusted to 2 (cluster) due to repeated study_id"
      )
    }
  }

  if (!is.null(input$study_id) && call_args$studylevel %/% 2 == 1) {
    n_clusters <- length(unique(input$study_id))
    if (n_clusters < 2) {
      call_args$studylevel <- call_args$studylevel %% 2
      adjustment_notes <- c(
        adjustment_notes,
        "cluster component dropped (only 1 cluster in study_id)"
      )
    }
  }

  if (call_args$instrument == 1 && length(unique(input$ni)) < 2) {
    call_args$instrument <- 0
    call_args$AR <- 0
    adjustment_notes <- c(
      adjustment_notes,
      "instrument auto-adjusted to 0 (ni has no variation); AR disabled"
    )
  }

  if (call_args$AR == 1 && length(input$yi) > 5000) {
    call_args$AR <- 0
    adjustment_notes <- c(adjustment_notes, "AR disabled automatically for n > 5000")
  }

  if (!call_args$method %in% 1:4) {
    stop("Invalid method parameter: must be 1 (PET), 2 (PEESE), 3 (PET-PEESE), or 4 (EK)",
      call. = FALSE
    )
  }

  if (!call_args$weight %in% 0:2) {
    stop("Invalid weight parameter: must be 0 (none), 1 (inverse-variance), or 2 (MAIVE-adjusted)",
      call. = FALSE
    )
  }

  if (!call_args$instrument %in% 0:1) {
    stop("Invalid instrument parameter: must be 0 (no IV) or 1 (use IV)",
      call. = FALSE
    )
  }

  list(
    call_args = call_args,
    adjustment_notes = adjustment_notes
  )
}

maive_warning_notes <- function(captured_warnings) {
  if (length(captured_warnings) == 0) {
    character(0)
  } else {
    paste0("MAIVE warning: ", captured_warnings)
  }
}

maive_error_context <- function(n_studies, call_args) {
  paste0(
    "Context: n=", n_studies,
    ", method=", call_args$method,
    ", weight=", call_args$weight,
    ", instrument=", call_args$instrument,
    ", studylevel=", call_args$studylevel,
    ", SE=", call_args$SE,
    ", AR=", call_args$AR,
    ", first_stage=", call_args$first_stage,
    ", use_waive=", call_args$use_waive
  )
}

maive_run_model <- function(method_name, maive_data, call_args, adjustment_notes, n_studies) {
  runner <- if (isTRUE(call_args$use_waive)) MAIVE::waive else MAIVE::maive
  model_args <- c(
    list(dat = maive_data),
    call_args[c("method", "weight", "instrument", "studylevel", "SE", "AR", "first_stage")]
  )
  captured_warnings <- character(0)

  result <- tryCatch(
    withCallingHandlers(
      do.call(runner, model_args),
      warning = function(w) {
        captured_warnings <<- maive_unique_strings(c(captured_warnings, conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      note_parts <- c(
        adjustment_notes,
        maive_warning_notes(captured_warnings),
        paste0(
          "MAIVE execution failed: ",
          conditionMessage(e),
          "\n",
          maive_error_context(n_studies, call_args)
        )
      )

      create_empty_result(
        method_name = method_name,
        note = paste(note_parts, collapse = "; "),
        extra_columns = method_extra_columns.MAIVE(method_name)
      )
    }
  )

  if (is.data.frame(result) && isTRUE(result$convergence[1] == FALSE)) {
    return(result)
  }

  list(
    result = result,
    captured_warnings = captured_warnings
  )
}

maive_detect_instrument_strength <- function(maive_result, first_stage_f, instrument_enabled) {
  reported_strength <- maive_result[["instrument_strength"]]
  if (!is.null(reported_strength) &&
    length(reported_strength) == 1 &&
    !is.na(reported_strength) &&
    nzchar(reported_strength)) {
    return(as.character(reported_strength))
  }

  if (!isTRUE(instrument_enabled)) {
    return("not_applicable")
  }

  if (!is.numeric(first_stage_f) || length(first_stage_f) != 1 || is.na(first_stage_f)) {
    return("unknown")
  }

  if (first_stage_f < 1) {
    return("very_weak")
  }

  if (first_stage_f < 10) {
    return("weak")
  }

  "strong"
}

maive_extract_inference <- function(maive_result) {
  estimate <- maive_result$beta
  standard_error <- maive_result$SE

  if (is.null(estimate) || is.na(estimate)) {
    stop("MAIVE returned NULL or NA estimate", call. = FALSE)
  }

  if (is.null(standard_error) || is.na(standard_error) || standard_error <= 0) {
    stop("MAIVE returned invalid standard error (NULL, NA, or non-positive)", call. = FALSE)
  }

  list(
    estimate = estimate,
    standard_error = standard_error
  )
}

maive_extract_ci <- function(maive_result, estimate, standard_error) {
  ar_ci <- maive_result$AR_CI
  is_valid_ar_ci <- !is.null(ar_ci) &&
    is.numeric(ar_ci) &&
    length(ar_ci) == 2 &&
    !anyNA(ar_ci) &&
    is.finite(ar_ci[1]) &&
    is.finite(ar_ci[2])

  if (is_valid_ar_ci) {
    return(list(
      ci_lower = ar_ci[1],
      ci_upper = ar_ci[2],
      used_ar_ci = TRUE,
      ar_ci_available = TRUE
    ))
  }

  list(
    ci_lower = estimate - 1.96 * standard_error,
    ci_upper = estimate + 1.96 * standard_error,
    used_ar_ci = FALSE,
    ar_ci_available = !is.null(ar_ci) &&
      (is.character(ar_ci) || (is.numeric(ar_ci) && anyNA(ar_ci)))
  )
}

maive_parse_first_stage_f <- function(f_test) {
  if (!is.null(f_test) && is.character(f_test) && f_test == "NA") {
    return(NA_real_)
  }

  if (is.null(f_test)) {
    return(NA_real_)
  }

  as.numeric(f_test)
}

maive_extract_diagnostics <- function(maive_result, instrument_enabled) {
  first_stage_f <- maive_parse_first_stage_f(maive_result[["F-test"]])
  hausman_stat <- maive_or_default(maive_result$Hausman, NA_real_)
  bias_p_value <- maive_or_default(
    maive_result$pbias_pval,
    maive_result[["pub bias p-value"]]
  )

  if (is.null(bias_p_value)) {
    bias_p_value <- NA_real_
  }

  instrument_strength <- maive_detect_instrument_strength(
    maive_result = maive_result,
    first_stage_f = first_stage_f,
    instrument_enabled = instrument_enabled
  )

  list(
    first_stage_f = first_stage_f,
    hausman_stat = hausman_stat,
    bias_p_value = bias_p_value,
    instrument_strength = instrument_strength,
    weak_iv = isTRUE(instrument_enabled) &&
      is.numeric(first_stage_f) &&
      length(first_stage_f) == 1 &&
      !is.na(first_stage_f) &&
      first_stage_f < 10
  )
}

maive_build_note <- function(adjustment_notes,
                             captured_warnings,
                             ci_info,
                             diagnostics,
                             instrument_enabled) {
  note_parts <- adjustment_notes

  if (diagnostics$weak_iv) {
    note_parts <- c(
      sprintf(
        "PublicationBiasBenchmark invalidated the MAIVE estimate because first-stage F-statistic (%.3f) is below 10.",
        diagnostics$first_stage_f
      ),
      note_parts
    )
  } else if (isTRUE(instrument_enabled) &&
    is.na(diagnostics$first_stage_f) &&
    identical(diagnostics$instrument_strength, "unknown")) {
    note_parts <- c(
      "Instrument strength could not be diagnosed because the first-stage F-statistic is NA.",
      note_parts
    )
  }

  if (!diagnostics$weak_iv) {
    if (ci_info$used_ar_ci) {
      note_parts <- c(note_parts, "Using Anderson-Rubin CI")
    } else if (ci_info$ar_ci_available) {
      note_parts <- c(note_parts, "AR CI attempted but unavailable; using Wald CI")
    }
  }

  note_parts <- maive_unique_strings(c(
    note_parts,
    maive_warning_notes(captured_warnings)
  ))

  if (length(note_parts) == 0) {
    NA_character_
  } else {
    paste(note_parts, collapse = "; ")
  }
}

maive_build_result <- function(method_name,
                               inference,
                               ci_info,
                               p_value,
                               note,
                               diagnostics) {
  data.frame(
    method = method_name,
    estimate = inference$estimate,
    standard_error = inference$standard_error,
    ci_lower = ci_info$ci_lower,
    ci_upper = ci_info$ci_upper,
    p_value = p_value,
    BF = NA_real_,
    convergence = TRUE,
    note = note,
    first_stage_f = diagnostics$first_stage_f,
    hausman_stat = diagnostics$hausman_stat,
    bias_p_value = diagnostics$bias_p_value,
    used_ar_ci = ci_info$used_ar_ci,
    ar_ci_available = ci_info$ar_ci_available,
    instrument_strength = diagnostics$instrument_strength,
    stringsAsFactors = FALSE
  )
}

#' @title MAIVE: Meta-Analysis Instrumental Variable Estimator
#'
#' @author Petr Cala \email{cala.p@@seznam.cz}
#'
#' @description
#' Implements the MAIVE method for publication bias correction using
#' instrumental variable estimation with variance instrumentation. MAIVE
#' addresses spurious precision in meta-analysis by instrumenting standard
#' errors with inverse sample sizes, providing consistent estimates even
#' when precision is manipulated through p-hacking 
#' \insertCite{irsova2025spurious}{PublicationBiasBenchmark}.
#'
#' The method implements several estimators:
#' \itemize{
#'   \item PET (Precision-Effect Test): Linear precision-effect model
#'   \item PEESE (Precision-Effect Estimate with Standard Error): Quadratic model
#'   \item PET-PEESE: Conditional selection based on PET significance
#'   \item EK (Endogenous Kink): Flexible bias function with kink point
#'   \item WAIVE: Robust variant with outlier downweighting
#' }
#'
#' @param method_name Method identifier (automatically passed by framework)
#' @param data Data frame with yi (effect sizes), sei (standard errors),
#'   ni (sample sizes), and optionally study_id for clustering
#' @param settings List of method settings from method_settings.MAIVE()
#'
#' @return Single-row data frame with standardized output columns:
#'   \describe{
#'     \item{method}{Method identifier}
#'     \item{estimate}{Meta-analytic effect size estimate}
#'     \item{standard_error}{Standard error of estimate}
#'     \item{ci_lower, ci_upper}{95% confidence interval bounds (Anderson-Rubin if available)}
#'     \item{p_value}{Two-tailed p-value}
#'     \item{BF}{Bayes factor (NA for MAIVE)}
#'     \item{convergence}{Logical convergence indicator}
#'     \item{note}{Error messages if any}
#'     \item{first_stage_f}{First-stage F-statistic for instrument strength}
#'     \item{hausman_stat}{Hausman test statistic comparing IV vs OLS}
#'     \item{bias_p_value}{P-value for publication bias test}
#'     \item{used_ar_ci}{Whether Anderson-Rubin CI was used}
#'     \item{ar_ci_available}{Whether AR CI was computed successfully}
#'     \item{instrument_strength}{Instrument strength classification returned by
#'       MAIVE or derived from the first-stage F-statistic}
#'   }
#'
#' @details
#' MAIVE uses inverse sample sizes (1/N) as instruments for variances (SE^2)
#' in the first stage, then uses the instrumented variances in second-stage
#' PET/PEESE models. This approach provides consistent estimation when
#' precision is endogenous due to p-hacking or selective reporting.
#'
#' The Anderson-Rubin confidence interval is robust to weak instruments and
#' is automatically computed for unweighted IV estimators when feasible
#' (n < 5000). For weighted estimators or large samples, standard CIs are used.
#' PublicationBiasBenchmark targets MAIVE 0.2.4 and persists upstream MAIVE
#' warnings in the standardized \code{note} column.
#'
#' WAIVE extends MAIVE by downweighting: (1) negative residuals (spurious
#' precision) using exponential decay, and (2) extreme residuals (|z| > 2)
#' as potential outliers. This provides additional robustness against
#' publication bias and outliers.
#' When IV is used and the first-stage F-statistic is numeric and below 10,
#' PublicationBiasBenchmark blanks the standardized inferential outputs
#' (\code{estimate}, \code{standard_error}, \code{ci_lower}, \code{ci_upper},
#' \code{p_value}) to \code{NA} while keeping \code{convergence = TRUE}. These
#' rows remain available as diagnostics and are treated as missing estimates in
#' downstream performance summaries rather than as convergence failures.
#'
#' Available settings (see method_settings.MAIVE()):
#' \describe{
#'   \item{default}{PET-PEESE with IV, unweighted, levels first-stage}
#'   \item{PET}{PET with IV, unweighted}
#'   \item{PEESE}{PEESE with IV, unweighted}
#'   \item{EK}{Endogenous Kink model with IV}
#'   \item{weighted}{PET-PEESE with MAIVE-adjusted weighting}
#'   \item{WAIVE}{Robust WAIVE variant with outlier downweighting}
#'   \item{log_first_stage}{PET-PEESE with log-linear first stage}
#'   \item{no_IV}{Standard PET-PEESE without instrumentation (baseline)}
#' }
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso [run_method()], [method_settings()], [method_extra_columns()]
#'
#' @examples
#' \dontrun{
#' # Generate test data
#' data <- simulate_dgm("Stanley2017", condition_id = 1)
#'
#' # Apply default MAIVE (PET-PEESE with IV)
#' result <- run_method("MAIVE", data, "default")
#'
#' # Apply WAIVE variant
#' result_waive <- run_method("MAIVE", data, "WAIVE")
#'
#' # Apply weighted MAIVE
#' result_weighted <- run_method("MAIVE", data, "weighted")
#'
#' # View available configurations
#' method_settings("MAIVE")
#' }
#'
#' @export
method.MAIVE <- function(method_name, data, settings) {
  input <- maive_validate_input(data)
  prepared_call <- maive_prepare_call(input, settings)
  model_run <- maive_run_model(
    method_name = method_name,
    maive_data = input$maive_data,
    call_args = prepared_call$call_args,
    adjustment_notes = prepared_call$adjustment_notes,
    n_studies = length(input$yi)
  )

  if (is.data.frame(model_run)) {
    return(model_run)
  }

  inference <- maive_extract_inference(model_run$result)
  ci_info <- maive_extract_ci(
    maive_result = model_run$result,
    estimate = inference$estimate,
    standard_error = inference$standard_error
  )
  p_value <- 2 * (1 - stats::pnorm(abs(inference$estimate / inference$standard_error)))
  diagnostics <- maive_extract_diagnostics(
    maive_result = model_run$result,
    instrument_enabled = prepared_call$call_args$instrument == 1
  )
  note <- maive_build_note(
    adjustment_notes = prepared_call$adjustment_notes,
    captured_warnings = model_run$captured_warnings,
    ci_info = ci_info,
    diagnostics = diagnostics,
    instrument_enabled = prepared_call$call_args$instrument == 1
  )

  if (diagnostics$weak_iv) {
    inference$estimate <- NA_real_
    inference$standard_error <- NA_real_
    ci_info$ci_lower <- NA_real_
    ci_info$ci_upper <- NA_real_
    p_value <- NA_real_
  }

  maive_build_result(
    method_name = method_name,
    inference = inference,
    ci_info = ci_info,
    p_value = p_value,
    note = note,
    diagnostics = diagnostics
  )
}


#' @export
#' @noRd
method_settings.MAIVE <- function(method_name) {
  list(
    default = list(
      method = 3,
      weight = 0,
      instrument = 1,
      studylevel = 2,
      SE = 0,
      AR = 1,
      first_stage = 0,
      use_waive = FALSE
    ),
    PET = list(
      method = 1,
      weight = 0,
      instrument = 1,
      studylevel = 2,
      SE = 0,
      AR = 1,
      first_stage = 0,
      use_waive = FALSE
    ),
    PEESE = list(
      method = 2,
      weight = 0,
      instrument = 1,
      studylevel = 2,
      SE = 0,
      AR = 1,
      first_stage = 0,
      use_waive = FALSE
    ),
    EK = list(
      method = 4,
      weight = 0,
      instrument = 1,
      studylevel = 2,
      SE = 0,
      AR = 0,
      first_stage = 0,
      use_waive = FALSE
    ),
    weighted = list(
      method = 3,
      weight = 2,
      instrument = 1,
      studylevel = 2,
      SE = 0,
      AR = 1,
      first_stage = 0,
      use_waive = FALSE
    ),
    WAIVE = list(
      method = 3,
      weight = 0,
      instrument = 1,
      studylevel = 2,
      SE = 0,
      AR = 0,
      first_stage = 0,
      use_waive = TRUE
    ),
    log_first_stage = list(
      method = 3,
      weight = 0,
      instrument = 1,
      studylevel = 2,
      SE = 0,
      AR = 1,
      first_stage = 1,
      use_waive = FALSE
    ),
    no_IV = list(
      method = 3,
      weight = 0,
      instrument = 0,
      studylevel = 2,
      SE = 0,
      AR = 0,
      first_stage = 0,
      use_waive = FALSE
    )
  )
}


#' @export
#' @noRd
method_extra_columns.MAIVE <- function(method_name) {
  c(
    "first_stage_f",
    "hausman_stat",
    "bias_p_value",
    "used_ar_ci",
    "ar_ci_available",
    "instrument_strength"
  )
}
