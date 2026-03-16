#' @title Endogenous Kink Method
#'
#' @author František Bartoš \email{f.bartos96@@gmail.com}
#'
#' @description
#' Implements the endogenous kink (EK) method proposed by Bom and Rachinger for
#' publication bias correction in meta-analysis. This method modifies the PET-PEESE
#' approach by incorporating a non-linear relationship between publication bias and
#' standard errors through a kinked regression specification. The method recognizes
#' that when the true effect is non-zero, there is minimal publication selection
#' when standard errors are very small (since most estimates are significant), but
#' selection increases as standard errors grow. The kink point is endogenously
#' determined using a two-step procedure based on the confidence interval of the
#' initial effect estimate. See
#' \insertCite{bom2019kinked;textual}{PublicationBiasBenchmark} for details.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes) and sei (standard errors)
#' @param settings List of method settings (no settings version are implemented)
#'
#' @return Data frame with EK results
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' # Generate some example data
#' data <- data.frame(
#'   yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
#'   sei = c(0.1, 0.15, 0.08, 0.12, 0.09)
#' )
#'
#' # Apply EK method
#' result <- run_method("EK", data)
#' print(result)
#'
#' @export
method.EK <- function(method_name, data, settings = NULL) {

  # Extract data
  effect_sizes    <- data$yi
  standard_errors <- data$sei

  # Check input
  if (length(effect_sizes) < 2)
    stop("At least 2 estiamtes required for EK analysis", call. = FALSE)


  res <- .method_EK_EK.est(d = effect_sizes, v = standard_errors^2)

  estimate     <- res$value[res$term == "b0" & res$variable == "estimate"]
  estimate_se  <- res$value[res$term == "b0" & res$variable == "std.error"]
  estimate_lci <- res$value[res$term == "b0" & res$variable == "conf.low"]
  estimate_uci <- res$value[res$term == "b0" & res$variable == "conf.high"]
  estimate_p   <- res$value[res$term == "b0" & res$variable == "p.value"]

  convergence <- TRUE
  note        <- NA

  return(data.frame(
    method           = method_name,
    estimate         = estimate,
    standard_error   = estimate_se,
    ci_lower         = estimate_lci,
    ci_upper         = estimate_uci,
    p_value          = estimate_p,
    BF               = NA,
    convergence      = convergence,
    note             = note
  ))
}

#' @export
method_settings.EK <- function(method_name) {

  settings <- list(
    "default" = list()
  )

  return(settings)
}

#' @export
method_extra_columns.EK <- function(method_name)
  character(0) # modify if EK method needs additional columns



### additional computation functions ----
# Imported and slightly modified from Hong & Reed 2021
# (https://osf.io/pr4mb/)
.method_EK_EK.est <- function(d, v, tb=1.96, ts=1.96) {
  se <- sqrt(v)
  m <- length(v)

  ## Estimating FPP
  FP1 <- summary(stats::lm(d~se, weight=1/(v)))
  if(FP1$coefficient[1,4]>0.05){
    alpha1 <- as.numeric(FP1$coefficient[1,1])
    Q <- sum((as.numeric(FP1$residuals)/se)^2)
  }else{
    FP2 <- summary(stats::lm(d~v, weight=1/(v)))
    alpha1 <- as.numeric(FP2$coefficient[1,1])
    Q <- sum((as.numeric(FP2$residuals)/se)^2)
  }

  ## Computing a (critical value) and g(.)
  wi <- 1/(v)
  sig_eta2 <- max(0, m*((Q/(m-2))-1)/sum(wi))
  a <- (((alpha1^2)-tb*tb*sig_eta2)/((tb+ts)*alpha1))*(alpha1>(tb*sqrt(sig_eta2)))
  g <- (se-a)*(se>=a)

  ## EK regression
  reg    <- stats::lm(d~g, weight=1/(v))
  EKreg  <- summary(reg)$coefficient[1,]
  EKConf <- stats::confint(reg)[1,]

  ## Combining Results
  EKEstimationResults <- as.data.frame(matrix(NA, nrow=7, ncol=4))
  colnames(EKEstimationResults) <- c("method","term","variable","value")
  EKEstimationResults$method<-"EK"
  EKEstimationResults$term<-"b0"
  EKEstimationResults$variable <- c("estimate", "std.error", "statistic", "p.value","conf.low", "conf.high","DataSize")
  EKEstimationResults$value <- c(as.numeric(EKreg), as.numeric(EKConf), m)

  return(EKEstimationResults)
}
