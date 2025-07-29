#' @title Alinaghi and Reed 2019 Data Generating Model
#'
#' @description
#' This data generating model simulates univariate regression studies where a variable X
#' affects a continuous outcome Y. Each study estimates the coefficient of X, which consists
#' of a fixed component (α1) representing the overall mean effect, and a random component
#' that varies across studies but is constant within each study. In the "Random Effects"
#' environment (\code{"RE"}), each study produces one estimate, and the population effect
#' differs across studies.
#'
#' The description and code is based on
#' \insertCite{hong2021using;textual}{PublicationBiasBenchmark}.
#' The data generating model was introduced in
#' \insertCite{alinaghi2018meta;textual}{PublicationBiasBenchmark}.
#'
#' @param dgm_name DGM name (automatically passed)
#' @param settings List containing \describe{
#'   \item{environment}{Type of the simulation environment. One of \code{"FE"},
#'                      \code{"RE"}, or \code{"PRE"}.}
#'   \item{mean_effect}{Mean effect}
#' }
#'
#' @details
#' This data generating model is based on Alinaghi & Reed (2019), who study univariate
#' regression models where a variable X affects a continuous variable Y. The parameter
#' of interest is the coefficient on X. In the "Random Effects" environment, each study
#' produces one estimate, and the population effect differs across studies. The coefficient
#' on X equals a fixed component (α1) plus a random component that is fixed within a study
#' but varies across studies. The overall mean effect of X on Y is given by α1.
#'
#' A distinctive feature of Alinaghi & Reed's experiments is that the sample size of
#' estimated effects is fixed before publication selection, making the meta-analyst's
#' sample size endogenous and affected by the effect size. Large population effects
#' are subject to less publication selection, as most estimates satisfy the selection
#' criteria (statistical significance or correct sign).
#'
#' Another feature is the separation of statistical significance and sign of the estimated
#' effect as criteria for selection. Significant/correctly-signed estimates are always
#' "published," while insignificant/wrong-signed estimates have only a 10% chance of
#' being published. This allows for different and sometimes conflicting consequences for
#' estimator performance.
#'
#' The simulations are designed to be representative of meta-analyses in economics and
#' business, which typically have several hundred estimates and substantial effect
#' heterogeneity. In addition to the "Random Effects" environment, a
#' "Panel Random Effects" environment is included, where each study has 10 estimates,
#' modeling the common scenario of multiple estimates per study. Effect estimates and
#' standard errors are simulated to be more similar within studies than across studies,
#' and publication selection targets the study rather than individual estimates. For
#' inclusion in the meta-analyst's sample, a study must have at least 7 out of 10 estimates
#' that are significant or correctly signed.
#'
#'
#' @return Data frame with \describe{
#'   \item{yi}{effect size}
#'   \item{sei}{standard error}
#'   \item{study_id}{study identifier}
#'   \item{es_id}{effect size identifier}
#' }
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso [dgm()], [validate_dgm_settigns()]
#' @export
dgm.Alinaghi2019 <- function(dgm_name, settings) {

  # Extract settings
  environment   <- settings[["environment"]]
  mean_effect   <- settings[["mean_effect"]]

  # Simulate data sets
  df <- .HongAndReed2021_Alinaghi2019_MetaStudy(environment, mean_effect)

  # Create result data frame
  data <- data.frame(
   yi       = df$effect,
   sei      = df$se,
   study_id = df$StdID,
   es_id    = df$EstID
  )

  return(data)
}

#' @export
validate_dgm_settigns.Alinaghi2019 <- function(dgm_name, settings) {

  # Check that all required settings are specified
  required_params <- c("environment", "mean_effect")
  missing_params <- setdiff(required_params, names(settings))
  if (length(missing_params) > 0)
    stop("Missing required settings: ", paste(missing_params, collapse = ", "))

  # Extract settings
  environment   <- settings[["environment"]]
  mean_effect   <- settings[["mean_effect"]]

  # Validate settings
  if (!length(environment) == 1 || !is.character(environment) || !environment %in% c("FE", "RE", "PRE"))
    stop("'environment' must be a string with one of the following values: 'FE', 'RE', 'PRE'")
  if (length(mean_effect) != 1 || !is.numeric(mean_effect) || is.na(mean_effect))
    stop("'mean_effect' must be numeric")

  return(invisible(TRUE))
}


### additional simulation functions ----
# Imported and slightly modified from Hong & Reed 2019

##############################
## Primary Study Data (Effect) Generation
#######################################################
.HongAndReed2021_Alinaghi2019_PrimaryStudy <- function(StudyID, al, ali, lambdai0, type){
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
    alir<-rnorm(1, mean = ali, sd = sigr);
    lambdair<- lambdai + runif(1, 0, 1)*(sigr>0)
    x <- rnorm(obs, mean = 0, sd = 1)
    y <- 1 + alir*x + lambdair*rnorm(obs, mean = 0, sd = 1)
    eff_se_pval<-as.numeric(summary(lm(y~x))$coefficients[2,c(1,2,4)])
    t<-abs(as.numeric(summary(lm(y~x))$coefficients[2,3]))
    ci<-as.numeric(confint(lm(y~x), level=0.95)[2,1:2])
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
## Meta Analysis Data Generation
#######################################################
.HongAndReed2021_Alinaghi2019_CollectingData<- function(type, alpha){
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
    ali<-rnorm(1, mean = alpha, sd = sigi)
    if(i==1){
      MetaStudyData<-.HongAndReed2021_Alinaghi2019_PrimaryStudy(i, alpha, ali, runif(1,0,1), type)
    }else{
      MetaStudyData<-rbind(MetaStudyData, .HongAndReed2021_Alinaghi2019_PrimaryStudy(i, alpha, ali, runif(1,0,1), type))
    }
  }
  return(MetaStudyData)
}
#######################################################


##############################
## Creating Publication Bias
#######################################################
.HongAndReed2021_Alinaghi2019_MetaStudy <- function(type, alpha){
  MetaData<-as.data.frame(.HongAndReed2021_Alinaghi2019_CollectingData(type, alpha))
  return(MetaData)
}
#######################################################


##############################
# Clustered Standard Errors; ARRSM
#######################################################
.HongAndReed2021_Alinaghi2019_clusteredSE_ARRSM <-function(regOLS, Study){
  M <- length(unique(Study))
  N <- length(Study)
  K <- regOLS$rank
  dfc <- (M/(M-1)) * ((N-1)/(N-K))
  u<-apply(estfun(regOLS),2,function(x) tapply(x, Study,sum))
  vcovCL<-dfc*sandwich(regOLS, meat=crossprod(u)/N)
  ci <-coef(regOLS) + sqrt(diag(vcovCL)) %o% qt(c(0.025,0.975),summary(regOLS)$df[2])
  return (list("co"=coeftest(regOLS, vcovCL), "ci"=ci))
}
#######################################################


##############################
## Creating Publication Bias
#######################################################
.HongAndReed2021_Alinaghi2019_ARBias <- function(MetaData, type, bias){
  if(type=='PRE'){
    rnd <- matrix(1, nrow=100, ncol=1)
    for(rndi in 1:10){rnd[c((1+10*(rndi-1)):(10*rndi))] <- runif(1,0,1);}
    MetaData<-as.data.frame(cbind(MetaData, rnd=rnd))
  }else{
    MetaData<-as.data.frame(cbind(MetaData, rnd=runif(nrow(MetaData),0,1)))
  }
  if(bias!='none'){
    if(type=='PRE'){
      if(bias=='Sig'){
        MetaData<-subset(MetaData, ((MetaData$nSig>=0.7)| (MetaData$rnd<0.1)));
      }else{
        MetaData<-subset(MetaData, ((MetaData$nPos>=0.7)| (MetaData$rnd<0.1)));
      }
    }else{
      if(bias=='Sig'){
        MetaData<-subset(MetaData, ((MetaData$Sig==1)   | (MetaData$rnd<0.1)));
      }else{
        MetaData<-subset(MetaData, ((MetaData$effect>0) | (MetaData$rnd<0.1)));
      }
    }
  }
  return(MetaData)
}
#######################################################
