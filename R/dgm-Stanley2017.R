#' @title Stanley, Doucouliagos, and Ioannidis (2017) Data-Generating Mechanism
#'
#' @author František Bartoš \email{f.bartos96@@gmail.com} (adapted from Hong and Reed 2021)
#'
#' @description
#' Simulates two scenarios for meta-analysis studies investigating the effect
#' of a treatment in:
#' (1) Log Odds Ratio scenario, where the outcome is binary and effect
#' heterogeneity is controlled by a random component, and
#' (2) Cohen's d scenario, where the outcome is continuous and effect
#' heterogeneity is introduced through a random component.
#' Both scenarios allow for varying sample sizes and publication selection
#' regimes, affecting the inclusion of study estimates based on their
#' statistical significance and sign.
#'
#' The description and code is based on
#' \insertCite{hong2021using;textual}{PublicationBiasBenchmark}.
#' The data-generating mechanism was introduced in
#' \insertCite{stanley2017finding;textual}{PublicationBiasBenchmark}.
#'
#' @param dgm_name DGM name (automatically passed)
#' @param settings List containing \describe{
#'   \item{environment}{Type of the simulation environment. One of
#'                      \code{"logOR"} or \code{"SMD"}.}
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
#' This function simulates two meta-analysis scenarios to evaluate the effect
#' of a binary treatment variable (`treat = {0, 1}`) on study outcomes,
#' incorporating both effect heterogeneity and publication selection mechanisms.
#'
#' In the Log Odds Ratio (\code{"logOR"}) scenario, primary studies assess the
#' impact of treatment on a binary success indicator (`Y = 1`). The control
#' group has a fixed 10% probability of success, while the treatment group's
#' probability is increased by a fixed effect and a mean-zero random component,
#' whose variance (sigma2_h) controls effect heterogeneity. Each study estimates a
#' logistic regression, with the coefficient on `treat` (alpha1) as the effect of
#' interest. Study sample sizes vary, resulting in different standard errors
#' for estimated effects.
#'
#' In the Cohen's d (\code{"SMD"}) scenario, the outcome variable is
#' continuous. The treatment effect is modeled as a fixed effect (alpha1) plus a
#' random component (variance sigma2_h). Each study computes Cohen's d, the
#' standardized mean difference between treatment and control groups. Study
#' sample sizes vary, affecting the standard errors of d.
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
#'   \item{ni}{sample size}
#'   \item{es_type}{effect size type}
#' }
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso [dgm()], [validate_dgm_setting()]
#' @export
dgm.Stanley2017 <- function(dgm_name, settings) {

  # Extract settings
  environment          <- settings[["environment"]]
  mean_effect          <- settings[["mean_effect"]]
  effect_heterogeneity <- settings[["effect_heterogeneity"]]
  bias                 <- settings[["bias"]]
  n_studies            <- settings[["n_studies"]]
  sample_sizes         <- settings[["sample_sizes"]]

  # unlist effect sizes if passed as a list instead of a vector
  if (is.list(sample_sizes) && length(sample_sizes) == 1)
    sample_sizes <- sample_sizes[[1]]

  # Simulate data sets
  if (environment == "logOR"){
    df <- .HongAndReed2021_Stanley2017_MetaStudy_LogOR(mean_effect, effect_heterogeneity, n_studies, bias, sample_sizes)
  } else if (environment == "SMD"){
    df <- .HongAndReed2021_Stanley2017_MetaStudy_Cohen_d(mean_effect, effect_heterogeneity, n_studies, bias, sample_sizes)
  }

  # Create result data frame
  if (environment == "logOR"){
    data <- data.frame(
      yi      = df[,"EstimatedLogOR"],
      sei     = df[,"StdErrLogOR"],
      ni      = df[,"PrimaryStudyOBS"] * 2,
      es_type = "logOR"
    )
  } else if (environment == "SMD"){
    data <- data.frame(
      yi      = df[,"EstimatedCohend"],
      sei     = df[,"StdErrCohend"],
      ni      = df[,"PrimaryStudyOBS"] * 2,
      es_type = "SMD"
    )
  }

  return(data)
}

#' @export
validate_dgm_setting.Stanley2017 <- function(dgm_name, settings) {

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

  # unlist effect sizes if passed as a list instead of a vector
  if (is.list(sample_sizes) && length(sample_sizes) == 1)
    sample_sizes <- sample_sizes[[1]]

  # Validate settings
  if (!length(environment) == 1 || !is.character(environment) || !environment %in% c("logOR", "SMD"))
    stop("'environment' must be a string with one of the following values: 'logOR', 'SMD'")
  if (length(mean_effect) != 1 || !is.numeric(mean_effect) || is.na(mean_effect))
    stop("'mean_effect' must be numeric")
  if (length(effect_heterogeneity) != 1 || !is.numeric(effect_heterogeneity) || is.na(effect_heterogeneity) || effect_heterogeneity < 0)
    stop("'effect_heterogeneity' must be non-negative")
  if (length(n_studies) != 1 || !is.numeric(n_studies) || is.na(n_studies) || !is.wholenumber(n_studies) || n_studies < 1)
    stop("'n_studies' must be an integer larger targer than 0")
  if (length(sample_sizes) < 1 || !any(is.numeric(sample_sizes)) || any(is.na(sample_sizes)) || any(!is.wholenumber(sample_sizes)) || any(sample_sizes < 1))
    stop("'sample_sizes' must be an integer vector larger targer than 0")
  if (length(bias) != 1 || !is.numeric(bias) || is.na(bias) || (bias < 0 || bias > 1))
    stop("'bias' must be in [0, 1] range")

  return(invisible(TRUE))
}

#' @export
dgm_conditions.Stanley2017 <- function(dgm_name) {

  # Keep the same order as in Hong and Reed 2021
  simulationType  <- "SMD"
  effectSize_List <- c(0, 0.50)
  sigH_List       <- c(0, 0.0625, 0.125, 0.25, 0.50)
  PubBias_List    <- c(0, 0.5, 0.75)
  MetaStudyN_List <- c(5,10,20,40,80)
  param1 <- as.data.frame(expand.grid(effectSize=effectSize_List, sigH=sigH_List, PubBias=PubBias_List, m=MetaStudyN_List, SimType=simulationType,stringsAsFactors = FALSE))

  simulationType   <- "logOR"
  effectSize_List  <- c(0.00, 0.03, 0.06)
  sigH_List        <- c(0.006)
  PubBias_List     <- c(0.0, 0.5)
  MetaStudyN_List  <- c(5,10,20,40,80)
  param2           <- as.data.frame(expand.grid(effectSize=effectSize_List, sigH=sigH_List, PubBias=PubBias_List, m=MetaStudyN_List, SimType=simulationType,stringsAsFactors = FALSE))

  paramONE <- rbind(param1,param2)


  simulationType  <- "SMD"
  effectSize_List <- c(0, 0.50)
  sigH_List       <- c(0, 0.0625, 0.125, 0.25, 0.50)
  PubBias_List    <- c(0, 0.5, 0.75)
  MetaStudyN_List <- c(100,200,400,800)
  param3          <- as.data.frame(expand.grid(effectSize=effectSize_List, sigH=sigH_List, PubBias=PubBias_List, m=MetaStudyN_List, SimType=simulationType,stringsAsFactors = FALSE))

  simulationType  <- "logOR"
  effectSize_List <- c(0.00, 0.03, 0.06)
  sigH_List       <- c(0.006)
  PubBias_List    <- c(0.0, 0.5)
  MetaStudyN_List <- c(100,200,400,800)
  param4          <- as.data.frame(expand.grid(effectSize=effectSize_List, sigH=sigH_List, PubBias=PubBias_List, m=MetaStudyN_List, SimType=simulationType,stringsAsFactors = FALSE))

  paramTWO <- rbind(param3,param4)


  # rename parameters
  settings <- rbind(paramONE,paramTWO)
  colnames(settings)    <- c("mean_effect", "effect_heterogeneity", "bias", "n_studies", "environment")
  settings$sample_sizes <- NA

  # enlist the corresponding sample sizes
  settings$sample_sizes[settings$environment == "SMD"]   <- list(c(32,64,125,250,500))
  settings$sample_sizes[settings$environment == "logOR"] <- list(c(50,100,100,250,500))

  # attach setting id
  settings$condition_id <- 1:nrow(settings)

  return(settings)
}

### additional simulation functions ----
# Imported and slightly modified from Hong & Reed 2021
# (https://osf.io/pr4mb/)

###############################################
## Primary Study Data (logOR) Generation     ##
## Primary Study Data (logOR) Generation     ##
#################################################################
.HongAndReed2021_Stanley2017_PrimaryStudy_LogOR <- function(TreatmentEffect, sigH, bias, primaryObs){
  if(bias==0){
    P <- 0.1 + TreatmentEffect + stats::rnorm(1, 0, sigH);
  }else{
    P <- 0.1 + TreatmentEffect;
  }
  nT<-primaryObs;
  mT<-sum(stats::rbinom(nT, size=1, prob=P));
  nC<-primaryObs;
  mC<-sum(stats::rbinom(nC, size=1, prob=0.1));

  correction<-(mT*mC==0)*10^(-5);
  LogOR<-log(((mT+correction)/(nT-mT+correction))/((mC+correction)/(nC-mC+correction)));
  LogORse<-sqrt( (1/(mT+correction)) + (1/(nT-mT+correction)) + (1/(mC+correction)) + (1/(nC-mC+correction)) )
  LogORci<-LogOR+LogORse*stats::qt(c(0.025, 0.975), df=10^100)
  LogORt<-LogOR/LogORse
  return(c(TreatmentEffect,sigH, round(log(((.1+TreatmentEffect)/(.9-TreatmentEffect) )/(.1/.9)),2), LogOR, LogORse, LogORci, (LogORci[1]>0), primaryObs))
}
###################################
## Meta Analysis Data-Generation ##
#################################################################
.HongAndReed2021_Stanley2017_MetaStudy_LogOR <- function(effect, sigH, m, bias, PrimaryN){
  MetaStudyData<-matrix(0,nrow=m, ncol=10)
  colnames(MetaStudyData)<-c('StudyID','effect','sigH','TrueLogOR','EstimatedLogOR','StdErrLogOR','LowerBound','UpperBound','Pos_SigEffect', 'PrimaryStudyOBS')
  biasedStudy<-sample(c(ceiling(m*bias), floor(m*bias)),1)
  for(i in 1:m){
    primaryObs<-PrimaryN[(i-1) %% length(PrimaryN) + 1]; # modified from the original
    if(stats::runif(1, 0, 1)<bias){
      PrimaryStudy<-.HongAndReed2021_Stanley2017_PrimaryStudy_LogOR(effect, sigH, bias, primaryObs)
      while(PrimaryStudy[8]==0){PrimaryStudy<-.HongAndReed2021_Stanley2017_PrimaryStudy_LogOR(effect, sigH, bias, primaryObs)}
      MetaStudyData[i,]<-c(i,PrimaryStudy)
    }else{
      PrimaryStudy<-.HongAndReed2021_Stanley2017_PrimaryStudy_LogOR(effect, sigH, bias, primaryObs)
      MetaStudyData[i,]<-c(i,PrimaryStudy)
    }}
  return(MetaStudyData)
}
#################################################################
#################################################################



###############################################
## Primary Study Data (Cohen's d) Generation ##
## Primary Study Data (Cohen's d) Generation ##
#################################################################
.HongAndReed2021_Stanley2017_PrimaryStudy_Cohen_d <- function(TreatmentEffect, sigH, primaryObs){
  obsControl<-primaryObs;
  y_cj<-stats::rnorm(obsControl, 300, .86603) + stats::rnorm(obsControl, 0, .50)
  obsTreatment<-primaryObs;
  Te<- stats::rnorm(obsTreatment, 300, .86603) + stats::rnorm(obsTreatment, 0, .50) + (TreatmentEffect + stats::rnorm(1, 0, sigH))

  dfn1<-(length(Te)-1); dfn2<-(length(y_cj)-1);
  s1<-sum((Te-mean(Te))^2)/dfn1; s2<-sum((y_cj-mean(y_cj))^2)/dfn2;
  sp<-sqrt((dfn1*s1 + dfn2*s2)/(dfn1+dfn2))
  estimatedCohen_d<-(mean(Te)-mean(y_cj))/sp
  stdErr_Cohen_d<-sqrt(((dfn1+dfn2+2)/((dfn1+1)*(dfn1+1)))+estimatedCohen_d^2/(2*(dfn1+dfn2+2)))
  cohen_ci<-estimatedCohen_d+stats::qt(c(0.025,.975),(dfn1+dfn2))*stdErr_Cohen_d
  cohen_t<-estimatedCohen_d/stdErr_Cohen_d;
  return(c(TreatmentEffect,sigH,TreatmentEffect,estimatedCohen_d,stdErr_Cohen_d,cohen_ci,(cohen_t>1.96),primaryObs))
}
###################################
## Meta Analysis Data-Generation ##
#################################################################
.HongAndReed2021_Stanley2017_MetaStudy_Cohen_d <- function(effect, sigH, m, bias, PrimaryN){
  MetaStudyData<-matrix(0,nrow=m, ncol=10)
  colnames(MetaStudyData)<-c('StudyID','effect','sigH','TrueCohend','EstimatedCohend','StdErrCohend','LowerBound','UpperBound','Pos_SigEffect', 'PrimaryStudyOBS')
  if(bias==0.75){
    for(i in 1:m){
      primaryObs<-PrimaryN[(i-1) %% length(PrimaryN) + 1]; # modified from the original
      if(stats::runif(1, 0, 1)<bias){
        PrimaryStudy<-.HongAndReed2021_Stanley2017_PrimaryStudy_Cohen_d(effect, sigH, primaryObs)
        while(PrimaryStudy[8]==0){PrimaryStudy<-.HongAndReed2021_Stanley2017_PrimaryStudy_Cohen_d(effect, sigH, primaryObs)}
        MetaStudyData[i,]<-c(i,PrimaryStudy)
      }else{
        PrimaryStudy<-.HongAndReed2021_Stanley2017_PrimaryStudy_Cohen_d(effect, sigH, primaryObs)
        while(PrimaryStudy[4]<=0){PrimaryStudy<-.HongAndReed2021_Stanley2017_PrimaryStudy_Cohen_d(effect, sigH, primaryObs)}
        MetaStudyData[i,]<-c(i,PrimaryStudy)
      }
    }
  }else{
    for(i in 1:m){
      primaryObs<-PrimaryN[(i-1) %% length(PrimaryN) + 1] # modified from the original
      if(stats::runif(1, 0, 1)<bias){
        PrimaryStudy<-.HongAndReed2021_Stanley2017_PrimaryStudy_Cohen_d(effect, sigH, primaryObs)
        while(PrimaryStudy[8]==0){PrimaryStudy<-.HongAndReed2021_Stanley2017_PrimaryStudy_Cohen_d(effect, sigH, primaryObs)}
        MetaStudyData[i,]<-c(i,PrimaryStudy)
      }else{
        PrimaryStudy<-.HongAndReed2021_Stanley2017_PrimaryStudy_Cohen_d(effect, sigH, primaryObs)
        MetaStudyData[i,]<-c(i,PrimaryStudy)
      }
    }
  }
  return(MetaStudyData)
}
#################################################################
#################################################################


