#' @title Bom and Rachinger 2019 Data Generating Model
#'
#' @description
#' Simulates univariate regression environments to estimate the effect of
#' X1 on Y (parameter α1). Effect heterogeneity is introduced via an omitted
#' variable (X2) correlated with X1, whose coefficient (α2)
#' is randomly distributed with mean zero and variance σ2_h.
#'
#' The description and code is based on
#' \insertCite{hong2021using;textual}{PublicationBiasBenchmark}.
#' The data generating model was introduced in
#' \insertCite{bom2019kinked;textual}{PublicationBiasBenchmark}.
#'
#' @param dgm_name DGM name (automatically passed)
#' @param settings List containing \describe{
#'   \item{environment}{Type of the simulation environment. One of
#'                      \code{"LogOR"} or \code{"Cohens_d"}.}
#'   \item{mean_effect}{Mean effect}
#'   \item{effect_heterogeneity}{Mean effect heterogeneity}
#'   \item{bias}{Proportion of studies affected by publication bias}
#'   \item{n_studies}{Number of effect size estimates}
#'   \item{sample_sizes}{Sample sizes of the effect size estimates. A vector of
#'   sample sizes needs to be supplied. The sample sizes in the vector are
#'   sequentially reused until all effect size estimates are generated.}
#' }
#'
#' @details
#' This function simulates univariate regression environments, focusing on
#' estimating the effect of a variable X1 on a dependent variable Y,
#' represented by the parameter α1. The simulation introduces variation in the
#' standard errors of estimated effects by allowing sample sizes to differ
#' across primary studies. Effect heterogeneity is modeled through an omitted
#' variable (X2) that is correlated with X1, where the coefficient on the
#' omitted variable, α2, is randomly distributed across studies with mean
#' zero and variance σ2_h.
#'
#' Individual estimates of α1 are subject to bias when α2 is nonzero due to
#' the omitted variable. Across the population of all studies, this omitted
#' variable bias averages out. However, when publication selection is
#' present—where selection depends on the sign and significance of the
#' estimated effect α^1—a bias is induced in the meta-analyst's sample.
#'
#' Publication selection is modeled in two regimes: (1) no selection, and
#' (2) 50% selection. Under 50% selection, each estimate has a 50% chance of
#' being evaluated for inclusion. If selected, only positive and statistically
#' significant estimates are published; otherwise, new estimates are generated
#' until this criterion is met. This process continues until the meta-analyst’s
#' sample reaches its predetermined size.
#'
#'
#' @return Data frame with \describe{
#'   \item{yi}{effect size}
#'   \item{sei}{standard error}
#' }
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso [dgm()], [validate_dgm_settigns()]
#' @export
dgm.Bom2019 <- function(dgm_name, settings) {

  # Extract settings
  environment          <- settings[["environment"]]
  mean_effect          <- settings[["mean_effect"]]
  effect_heterogeneity <- settings[["effect_heterogeneity"]]
  bias                 <- settings[["bias"]]
  n_studies            <- settings[["n_studies"]]
  sample_sizes         <- settings[["sample_sizes"]]

  # Simulate data sets
  df <- .HongAndReed2021_Bom2019_MetaStudy(mean_effect, effect_heterogeneity, n_studies, bias, sample_sizes)

  # Create result data frame
  data <- data.frame(
    yi   = df[,"y"],
    sei  = df[,"al_se"]
  )

  return(data)
}

#' @export
validate_dgm_settigns.Bom2019 <- function(dgm_name, settings) {

  # Check that all required settings are specified
  required_params <- c("environment", "mean_effect", "effect_heterogeneity", "bias", "n_studies", "sample_sizes")
  missing_params <- setdiff(required_params, names(settings))
  if (length(missing_params) > 0)
    stop("Missing required settings: ", paste(missing_params, collapse = ", "))

  # Extract settings
  environment          <- settings[["environment"]]
  mean_effect          <- settings[["mean_effect"]]
  effect_heterogeneity <- settings[["effect_heterogeneity"]]
  bias                 <- settings[["bias"]]
  n_studies            <- settings[["n_studies"]]
  sample_sizes         <- settings[["sample_sizes"]]

  # Validate settings
  if (!length(environment) == 1 || !is.character(environment) || !environment %in% c("LogOR", "Cohens_d"))
    stop("'environment' must be a string with one of the following values: 'LogOR', 'Cohens_d'")
  if (length(mean_effect) != 1 || !is.numeric(mean_effect) || is.na(mean_effect))
    stop("'mean_effect' must be numeric")
  if (length(effect_heterogeneity) != 1 || !is.numeric(effect_heterogeneity) || is.na(effect_heterogeneity) || effect_heterogeneity < 0)
    stop("'effect_heterogeneity' must be non-negative")
  if (length(n_studies) != 1 || !is.numeric(n_studies) || is.na(n_studies) || !is.wholenumber(n_studies) || n_studies < 1)
    stop("'n_studies' must be an integer larger targer than 0")
  if (length(sample_sizes) >= 1 || !any(is.numeric(sample_sizes)) || any(is.na(sample_sizes)) || any(!is.wholenumber(sample_sizes)) || any(sample_sizes < 1))
    stop("'sample_sizes' must be an integer vector larger targer than 0")
  if (length(bias) != 1 || !is.numeric(bias) || is.na(bias) || (bias < 0 || bias > 1))
    stop("'bias' must be in [0, 1] range")

  return(invisible(TRUE))
}


### additional simulation functions ----
# Imported and slightly modified from Hong & Reed 2021
# (https://osf.io/pr4mb/)

###################################
## Primary Study Data Generation
###################################
.HongAndReed2021_Bom2019_dgp <-function(al1, sigh, obs){
  x1=runif(obs, min = 100, max = 200);
  x2=x1+rnorm(obs, mean = 0, sd = 50);
  al2=rnorm(1,mean=0,sd=sigh);
  z = 100 + al1*x1 + al2*x2 + rnorm(obs,mean=0,sd=100);
  return (as.data.frame(cbind(z, x1 ,matrix(al1+al2, nrow=obs, ncol=1))))
}
###################################
## PMeta Analysis Data Generation
###################################
.HongAndReed2021_Bom2019_MetaStudy <- function(al1, sigh, ssize, Bias, obsList){
  output =  matrix(0, nrow=ssize, ncol=6);
  colnames(output) <- c("id","y","al_se","Significant","popal","obs");

  num_publ=ssize*(Bias/100);
  for(i in 1:ssize) {
    obs<-obsList[(i-1) %% length(obsList) + 1]; # modified
    output[i,1]=i;
    output[i,6]=obs;
    if (i<=num_publ){
      while (output[i,4]==0){
        data=.HongAndReed2021_Bom2019_dgp(al1,sigh,obs);
        output[i,5]=mean(data[,3]);
        out <- lm(data[,1] ~ data[,2])
        output[i,2]=coefficients(out)[2];
        output[i,3]=sqrt(diag(vcov(out)))[2];
        output[i,4]=((summary(out)$coefficients[2,4]<=0.05)*(0<=summary(out)$coefficients[2,1]));
      }
    } else if (i>num_publ){
      data=.HongAndReed2021_Bom2019_dgp(al1,sigh,obs);
      output[i,5]=mean(data[,3]);
      out <- lm(data[,1] ~ data[,2])
      output[i,2]=coefficients(out)[2];
      output[i,3]=sqrt(diag(vcov(out)))[2];
      output[i,4]=((summary(out)$coefficients[2,4]<=0.05)*(0<=summary(out)$coefficients[2,1]));
    } else { cat("Publication Bias Error", "\n"); }
  }
  return(output)
}
###################################
