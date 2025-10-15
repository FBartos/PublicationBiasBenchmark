### Utilities ----
# Local/Remote pathing with nested child paths is extremely buggy. This function makes sure that all paths are properly set
child_path <- function(x) {
  # 1) Prefer the directory of the currently knitted Rmd (works for pkgdown/knitr in CI and locally)
  p <- tryCatch({
    here_dir <- dirname(knitr::current_input(dir = TRUE))
    file.path(here_dir, x)
  }, error = function(e) "")
  if (is.character(p) && length(p) == 1 && nzchar(p) && file.exists(p)) {
    return(normalizePath(p, winslash = "/", mustWork = TRUE))
  }

  # 2) Try to get the package name safely (for installed builds)
  pkg <- tryCatch(utils::packageName(), error = function(e) NULL)
  if (is.null(pkg) || length(pkg) != 1 || is.na(pkg) || !nzchar(pkg)) {
    pkg <- tryCatch(desc::desc_get("Package")[[1]], error = function(e) NA_character_)
  }

  # 3) Try the installed vignettes dir (works when pkgdown builds from an installed copy)
  if (!is.null(pkg) && is.character(pkg) && nzchar(pkg)) {
    p <- tryCatch(system.file("vignettes", x, package = pkg), error = function(e) "")
    if (is.character(p) && length(p) == 1 && nzchar(p) && file.exists(p)) {
      return(normalizePath(p, winslash = "/", mustWork = TRUE))
    }
  }

  # 4) Fallback to source tree heuristics without requiring rprojroot
  candidates <- c(
    file.path("vignettes", x),
    file.path(getwd(), "vignettes", x)
  )
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates)) {
    return(normalizePath(candidates[[1]], winslash = "/", mustWork = TRUE))
  }

  stop(sprintf("Could not resolve child path for '%s'", x))
}

### Tables ----
make_table_summary <- function(results, common_scale = TRUE) {

  table_summary <- do.call(rbind, lapply(unique(results$label), function(ml)
    with(results[results$label == ml,],
         data.frame(
           "Method"         = unique(method),
           "Setting"        = unique(method_setting),
           "Convergence"    = mean(convergence),
           "Bias"           = if (common_scale) mean(bias,         na.rm = TRUE) else mean(bias_rank,         na.rm = TRUE),
           "Emp_se"         = if (common_scale) mean(empirical_se, na.rm = TRUE) else mean(empirical_se_rank, na.rm = TRUE),
           "RMSE"           = if (common_scale) mean(rmse,         na.rm = TRUE) else mean(rmse_rank,         na.rm = TRUE),
           "Coverage"       = mean(coverage, na.rm = TRUE),
           "CI_width"       = if (common_scale) mean(mean_ci_width,  na.rm = TRUE) else mean(mean_ci_width_rank,  na.rm = TRUE),
           "interval_score" = if (common_scale) mean(interval_score, na.rm = TRUE) else mean(interval_score_rank, na.rm = TRUE),
           "Error"          = mean(power[H0],  na.rm = TRUE),
           "Power"          = mean(power[!H0], na.rm = TRUE),
           "neg_LR"         = mean(negative_likelihood_ratio[!H0], na.rm = TRUE),
           "pos_LR"         = mean(positive_likelihood_ratio[!H0], na.rm = TRUE)
         ))
  ))

  return(table_summary)
}
make_rank_summary  <- function(table_summary) {

  rank_summary <- table_summary

  # lower is better
  for (measure in c("Emp_se", "RMSE", "Error", "CI_width", "neg_LR", "interval_score")) {
    rank_summary[[measure]]  <- rank(table_summary[[measure]], ties.method = "min", na.last = TRUE)
  }

  # higher is better
  for (measure in c("Convergence", "Coverage", "Power", "pos_LR")) {
    rank_summary[[measure]]  <- rank(-table_summary[[measure]], ties.method = "min", na.last = TRUE)
  }

  # closer to 0 is better
  for (measure in c("Bias")) {
    rank_summary[[measure]]  <- rank(abs(table_summary[[measure]]), ties.method = "min", na.last = TRUE)
  }

  return(rank_summary)
}

### Plots ----
create_raincloud_plot <- function(data, y_var, y_label, ylim_range = NULL, reference_line = NULL, title_text = NULL, rank = FALSE) {
  # Generate colors for methods (using a color palette)
  n_methods     <- length(unique(data$label))
  method_colors <- hcl.colors(n = n_methods, "Batlow", alpha = 0.7)
  names(method_colors) <- unique(data$label)

  # Cap values at axis limits if ylim_range is provided
  if (!rank && !is.null(ylim_range)) {
    data[[y_var]] <- pmax(ylim_range[1], pmin(ylim_range[2], data[[y_var]]))
  }

  ## create slightly different plots for ordinary and rank variables
  if (rank) {
    y_var   <- paste0(y_var, "_rank")
    y_label <- paste0("Rank(", y_label, ")")
    tab <- table(data$label, data[,y_var])
    rank_props <- as.data.frame(prop.table(tab, margin = 1) * 100)
    colnames(rank_props) <- c("label", y_var, "percentage")
    rank_props[,y_var] <- as.numeric(as.character(rank_props[,y_var]))
    p <- ggplot(data, aes(x = label, y = .data[[y_var]], fill = label, color = label)) +
      geom_point(data = rank_props, aes(size = percentage)) +
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        alpha = 0.7,
        median.linewidth = 1,
        median.colour = "#000000CC"
      )
  } else {
    p <- ggplot(data, aes(x = label, y = .data[[y_var]], fill = label, color = label)) +
      ggdist::stat_halfeye(
        adjust = 0.5,
        width = 0.6,
        .width = 0,
        justification = -0.2,
        point_colour = NA,
        alpha = 0.7
      ) +
      geom_boxplot(
        width = 0.15,
        outlier.shape = NA,
        alpha = 0.7
      ) +
      geom_point(
        position = position_jitter(width = 0.05, height = 0),
        size = 1,
        alpha = 0.3
      )
  }
  p <- p +
    scale_fill_manual(values = method_colors) +
    scale_color_manual(values = method_colors) +
    labs(
      x = "",
      y = y_label,
      title = title_text
    )

  # Add reference line if provided
  if (!rank && !is.null(reference_line)) {
    p <- p + geom_hline(yintercept = reference_line, linetype = "dashed", alpha = 0.7)
  }

  p <- p +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 10),
      axis.text.y = element_text(size = 8)
    )

  # Set y-axis limits if provided
  if (!rank && !is.null(ylim_range)) {
    p <- p + coord_flip(ylim = ylim_range)
  } else {
    p <- p + coord_flip()
  }

  return(p)
}
### Texts ----
generic_overview_text   <- function(dgm_names, results) {

  dgm_names <- sapply(dgm_names, function(name) {
    # Check if name ends with a 4-digit number
    if (grepl("\\d{4}$", name)) {
      # Extract year from end
      year <- sub(".*?(\\d{4})$", "\\1", name)
      sprintf("[%s (%s)](../reference/dgm.%s.html)", name, year, name)
    } else {
      sprintf("[%s](../reference/dgm.%s.html)", name, name)
    }
    }, USE.NAMES = FALSE)

  if (length(dgm_names) == 1) {
    dgm_names <- paste0(dgm_names, " data-generating mechanism")
  } else if (length(dgm_names) == 2) {
    dgm_names <- paste0(paste0(dgm_names, collapse = " and "), " data-generating mechanisms")
  } else if (length(dgm_names) > 2) {
    dgm_names <- paste0(paste0(dgm_names[-length(dgm_names)], collapse = ", "), ", and ", dgm_names[length(dgm_names)], " data-generating mechanisms")
  }

  n_conditions <- length(unique(paste0(results$dgm_name, "-", results$condition_id)))

  text <- sprintf(
    "These results are based on %1$s with a total of %2$i conditions.",
    dgm_names,
    n_conditions
    )

  return(text)
}
conditional_text        <- function() {
  paste(
    'The results below are conditional on method convergence.',
    'Note that the methods might differ in convergence rate and are therefore not compared on the same data sets.'
  )
}
method_replacement_text <- function() {
  paste(
    'The results below incorporate method replacement to handle non-convergence.',
    'If a method fails to converge, its results are replaced with the results from a simpler method (e.g., random-effects meta-analysis without publication bias adjustment).',
    'This emulates what a data analyst may do in practice in case a method does not converge.',
    'However, note that these results do not correspond to "pure" method performance as they might combine multiple different methods.',
    'See [Method Replacement Strategy](Results_Method_Replacement.html) for details of the method replacement specification.'
  )
}
