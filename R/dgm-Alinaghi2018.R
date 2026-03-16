#' @title Alinaghi and Reed (2018) Data-Generating Mechanism
#'
#' @author František Bartoš \email{f.bartos96@@gmail.com} (adapted from Hong and Reed 2021)
#'
#' @description
#' This data-generating mechanism simulates univariate regression studies where a variable X
#' affects a continuous outcome Y. Each study estimates the coefficient of X, which consists
#' of a fixed component (alpha1) representing the overall mean effect, and a random component
#' that varies across studies but is constant within each study. In the "Random Effects"
#' environment (\code{"RE"}), each study produces one estimate, and the population effect
#' differs across studies. In the "Panel Random Effects" environment (\code{"PRE"}), each
#' study has 10 estimates, modeling the common scenario where multiple estimates per study
#' are available, with publication selection targeting the study rather than individual estimates.
#'
#' The description and code is based on
#' \insertCite{hong2021using;textual}{PublicationBiasBenchmark}.
#' The data-generating mechanism was introduced in
#' \insertCite{alinaghi2018meta;textual}{PublicationBiasBenchmark}.
#'
#' @param dgm_name DGM name (automatically passed)
#' @param settings List containing \describe{
#'   \item{environment}{Type of the simulation environment. One of \code{"FE"},
#'                      \code{"RE"}, or \code{"PRE"}.}
#'   \item{mean_effect}{Mean effect}
#'   \item{bias}{Type of publication bias. One of \code{"none"}, \code{"positive"},
#'               or \code{"significant"}.}
#' }
#'
#' @details
#' This data-generating mechanism is based on Alinaghi & Reed (2018), who study univariate
#' regression models where a variable X affects a continuous variable Y. The parameter
#' of interest is the coefficient on X. In the "Random Effects" environment (\code{"RE"}),
#' each study produces one estimate, and the population effect differs across studies.
#' The coefficient on X equals a fixed component (alpha1) plus a random component that is
#' fixed within a study but varies across studies. The overall mean effect of X on Y is
#' given by alpha1. In the "Panel Random Effects" environment (\code{"PRE"}), each study has
#' 10 estimates, modeling the common scenario where multiple estimates per study are
#' available. In this environment, effect estimates and standard errors are simulated to
#' be more similar within studies than across studies, and publication selection targets
#' the study rather than individual estimates (a study must have at least 7 out of 10
#' estimates that are significant or correctly signed.).
#'
#' A distinctive feature of Alinaghi & Reed's experiments is that the number of
#' effect size estimates  is fixed before publication selection, making the meta-analyst's
#' sample size endogenous and affected by the effect size. Large population effects
#' are subject to less publication selection, as most estimates satisfy the selection
#' criteria (statistical significance or correct sign). The sample size of all primary
#' studies is fixed at 100 observations. (Neither the number of estimates nor the sample
#' size of primary studies can be changed in the current implementation of the function.)
#'
#' Another feature is the separation of statistical significance and sign of the estimated
#' effect as criteria for selection. Significant/correctly-signed estimates are always
#' "published," while insignificant/wrong-signed estimates have only a 10% chance of
#' being published. This allows for different and sometimes conflicting consequences for
#' estimator performance.
#'
#'
#' @return Data frame with \describe{
#'   \item{yi}{effect size}
#'   \item{sei}{standard error}
#'   \item{ni}{sample size}
#'   \item{study_id}{study identifier}
#'   \item{es_type}{effect size type}
#' }
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso [dgm()], [validate_dgm_setting()]
#' @export
dgm.Alinaghi2018 <- function(dgm_name, settings) {

  # Extract settings
  environment   <- settings[["environment"]]
  mean_effect   <- settings[["mean_effect"]]
  bias          <- settings[["bias"]]

  # Simulate data sets
  df <- .HongAndReed2021_Alinaghi2018_MetaStudy(environment, mean_effect)
  df <- .HongAndReed2021_Alinaghi2018_ARBias(df, environment, bias)

  # Create result data frame
  data <- data.frame(
   yi       = df$effect,
   sei      = df$se,
   ni       = df$obs,
   study_id = df$StdID,
   es_type  = "none"
  )

  return(data)
}

#' @export
validate_dgm_setting.Alinaghi2018 <- function(dgm_name, settings) {

  # Check that all required settings are specified
  required_params <- c("environment", "mean_effect")
  missing_params <- setdiff(required_params, names(settings))
  if (length(missing_params) > 0)
    stop("Missing required settings: ", paste(missing_params, collapse = ", "))

  # Extract settings
  environment   <- settings[["environment"]]
  mean_effect   <- settings[["mean_effect"]]
  bias          <- settings[["bias"]]

  # Validate settings
  if (!length(environment) == 1 || !is.character(environment) || !environment %in% c("FE", "RE", "PRE"))
    stop("'environment' must be a string with one of the following values: 'FE', 'RE', 'PRE'")
  if (length(mean_effect) != 1 || !is.numeric(mean_effect) || is.na(mean_effect))
    stop("'mean_effect' must be numeric")
  if (!length(bias) == 1 || !is.character(bias) || !bias %in% c("none", "positive", "significant"))
    stop("'bias' must be a string with one of the following values: 'none', 'positive', 'significant'")

  return(invisible(TRUE))
}

#' @export
dgm_conditions.Alinaghi2018 <- function(dgm_name) {

  # Keep the same order as in Hong and Reed 2021
  environment <- c("RE", "PRE", "FE")
  mean_effect <- c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0)
  bias        <- c("none", "positive", "significant")

  settings <- as.data.frame(expand.grid(
    environment = environment,
    mean_effect = mean_effect,
    bias        = bias,
    stringsAsFactors = FALSE
  ))

  # attach setting id
  settings$condition_id <- 1:nrow(settings)

  return(settings)
}

### additional simulation functions ----
# Imported and slightly modified from Hong & Reed 2021
# (https://osf.io/pr4mb/)

##############################
## Primary Study Data (Effect) Generation
#######################################################
.HongAndReed2021_Alinaghi2018_PrimaryStudy <- function(StudyID, al, ali, lambdai0, type){
  if(type=='PRE'){
    sigr<-sqrt(0.25);
    m<-10;
    lambdai<-0.5+30*lambdai0;
  }else if(type=='RE'){
    sigr<-0;
    m<-1;
    lambdai<-0.5+30*lambdai0;
  }else if(type=='FE'){
    sigr<-0;
    m<-1;
    lambdai<-0.2+30*lambdai0;
  }
  obs<-100;
  PrimaryData<-as.data.frame(matrix(ncol=19, nrow=m))
  colnames(PrimaryData)<-c('StdID','EstID','al','ali','alir','lambdai','lambdair','effect','se','lowerBound','upperBound','Sig','PosSig','NegSig','nSig','nPosSig','nNegSig','nPos','obs')
  for(i in 1:m){
    alir<-stats::rnorm(1, mean = ali, sd = sigr);
    lambdair<- lambdai + stats::runif(1, 0, 1)*(sigr>0)
    x <- stats::rnorm(obs, mean = 0, sd = 1)
    y <- 1 + alir*x + lambdair*stats::rnorm(obs, mean = 0, sd = 1)
    eff_se_pval<-as.numeric(summary(stats::lm(y~x))$coefficients[2,c(1,2,4)])
    t<-abs(as.numeric(summary(stats::lm(y~x))$coefficients[2,3]))
    ci<-as.numeric(stats::confint(stats::lm(y~x), level=0.95)[2,1:2])
    PrimaryData[i,1:14]<-c(StudyID, i, al,ali, alir, lambdai, lambdair, eff_se_pval[1:2], ci, (t>=2), (ci[1]>0), (ci[2]<0))
  }
  PrimaryData$nSig<-sum(PrimaryData$Sig)/m;
  PrimaryData$nPosSig<-sum(PrimaryData$PosSig)/m;
  PrimaryData$nNegSig<-sum(PrimaryData$NegSig)/m;
  PrimaryData$nPos<-sum(PrimaryData$effect>0)/m;
  PrimaryData$obs <- obs
  return(PrimaryData)
}
#######################################################


##############################
## Meta Analysis Data-Generation
#######################################################
.HongAndReed2021_Alinaghi2018_CollectingData<- function(type, alpha){
  if(type=='PRE'){
    StudyN<-100;
    sigi<-2
  }else if(type=='RE'){
    StudyN<-1000;
    sigi<-1
  }else if(type=='FE'){
    StudyN<-1000;
    sigi<-0;
  }

  for(i in 1:StudyN){
    ali<-stats::rnorm(1, mean = alpha, sd = sigi)
    if(i==1){
      MetaStudyData<-.HongAndReed2021_Alinaghi2018_PrimaryStudy(i, alpha, ali, stats::runif(1,0,1), type)
    }else{
      MetaStudyData<-rbind(MetaStudyData, .HongAndReed2021_Alinaghi2018_PrimaryStudy(i, alpha, ali, stats::runif(1,0,1), type))
    }
  }
  return(MetaStudyData)
}
#######################################################


##############################
## Creating Publication Bias
#######################################################
.HongAndReed2021_Alinaghi2018_MetaStudy <- function(type, alpha){
  MetaData<-as.data.frame(.HongAndReed2021_Alinaghi2018_CollectingData(type, alpha))
  return(MetaData)
}
#######################################################


##############################
# Clustered Standard Errors; ARRSM
#######################################################
.HongAndReed2021_Alinaghi2018_clusteredSE_ARRSM <-function(regOLS, Study){
  M <- length(unique(Study))
  N <- length(Study)
  K <- regOLS$rank
  dfc <- (M/(M-1)) * ((N-1)/(N-K))
  u<-apply(sandwich::estfun(regOLS),2,function(x) tapply(x, Study,sum))
  vcovCL<-dfc*sandwich::sandwich(regOLS, meat=crossprod(u)/N)
  ci <-stats::coef(regOLS) + sqrt(diag(vcovCL)) %o% stats::qt(c(0.025,0.975),summary(regOLS)$df[2])
  return (list("co"=lmtest::coeftest(regOLS, vcovCL), "ci"=ci))
}
#######################################################


##############################
## Creating Publication Bias
#######################################################
.HongAndReed2021_Alinaghi2018_ARBias <- function(MetaData, type, bias){
  if(type=='PRE'){
    rnd <- matrix(1, nrow=100, ncol=1)
    for(rndi in 1:10){rnd[c((1+10*(rndi-1)):(10*rndi))] <- stats::runif(1,0,1);}
    MetaData<-as.data.frame(cbind(MetaData, rnd=rnd))
  }else{
    MetaData<-as.data.frame(cbind(MetaData, rnd=stats::runif(nrow(MetaData),0,1)))
  }
  if(bias!='none'){
    if(type=='PRE'){
      if(bias=='significant'){
        MetaData<-subset(MetaData, ((MetaData$nSig>=0.7)| (MetaData$rnd<0.1)));
      }else{
        MetaData<-subset(MetaData, ((MetaData$nPos>=0.7)| (MetaData$rnd<0.1)));
      }
    }else{
      if(bias=='significant'){
        MetaData<-subset(MetaData, ((MetaData$Sig==1)   | (MetaData$rnd<0.1)));
      }else{
        MetaData<-subset(MetaData, ((MetaData$effect>0) | (MetaData$rnd<0.1)));
      }
    }
  }
  return(MetaData)
}
#######################################################
