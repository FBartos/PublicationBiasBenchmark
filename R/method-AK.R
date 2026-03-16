#' @title AK Method
#'
#' @author František Bartoš \email{f.bartos96@@gmail.com}
#'
#' @description
#' Implements the Andrews & Kasy (AK) method for publication bias correction in meta-analysis.
#' The AK method categorizes estimated effects into groups with different probabilities of being
#' published. AK1 uses symmetric selection grouping estimates into significant (|t| >= 1.96) and
#' insignificant (|t| < 1.96) estimates. AK2 uses asymmetric selection with four groups based on
#' both significance and sign: highly significant positive/negative effects and marginally
#' significant positive/negative effects, each with different publication probabilities. See
#' \insertCite{andrews2019identification;textual}{PublicationBiasBenchmark} for details.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes), sei (standard errors), and study_id
#' (for clustering wherever available)
#' @param settings List of method settings (see Details)
#'
#' @return Data frame with AK results
#'
#' @references
#'  \insertAllCited{}
#'
#' @details
#' The following settings are implemented \describe{
#'   \item{\code{"default"}}{Uses AK1 estimator (symmetric selection)}
#'   \item{\code{"AK1"}}{Symmetric selection model grouping estimates into significant
#'        (|t| >= 1.96) and insignificant (|t| < 1.96) categories with relative publication
#'        probabilities of 1 and p1 respectively.}
#'   \item{\code{"AK2"}}{Asymmetric selection model with four groups based on t-statistics:
#'        (a) t >= 1.96, (b) t < -1.96, (c) -1.96 <= t < 0, and (d) 0 <= t < 1.96, with
#'        relative publication probabilities of 1, p1, p2, and p3 respectively.}
#' }
#'
#' @examples
#' # Generate some example data
#' data <- data.frame(
#'   yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
#'   sei = c(0.1, 0.15, 0.08, 0.12, 0.09)
#' )
#'
#' # Apply AK method
#' result <- run_method("AK", data, "default")
#' print(result)
#'
#' @export
method.AK <- function(method_name, data, settings) {

  # Extract data
  effect_sizes    <- data$yi
  standard_errors <- data$sei

  # Check input
  if (length(effect_sizes) < 3)
    stop("At least 3 studies required for AK analysis", call. = FALSE)

  # Use clustering wherever available
  if (is.null(data[["study_id"]])) {
    study_id <- seq_along(effect_sizes)
  } else {
    study_id <- data[["study_id"]]
  }

  AKdata <- as.data.frame(cbind.data.frame(
    id       = study_id,
    effect   = effect_sizes,
    se       = standard_errors,
    constant = 1)
  )

  version <- settings[["version"]]
  if (version == "AK1") {
    res <- .method_AK_AK1.est(AKdata)
  } else if (version == "AK2") {
    res <- .method_AK_AK2.est(AKdata)
  }

  estimate     <- res$value[res$term == "b0" & res$variable == "estimate"]
  estimate_se  <- res$value[res$term == "b0" & res$variable == "std.error"]
  estimate_lci <- res$value[res$term == "b0" & res$variable == "conf.low"]
  estimate_uci <- res$value[res$term == "b0" & res$variable == "conf.high"]
  estimate_p   <- res$value[res$term == "b0" & res$variable == "p.value"]

  tau_estimate <- sqrt(res$value[res$term == "tau2" & res$variable == "estimate"])
  tau2_se      <- res$value[res$term == "tau2" & res$variable == "std.error"]

  bias_coefficient    <- list(res$value[grepl("rho", res$term) & res$variable == "estimate"])
  bias_coefficient_se <- list(res$value[grepl("rho", res$term) & res$variable == "std.error"])

  convergence <- !is.na(res$value[res$term == "b0" & res$variable == "estimate"])
  note        <- NA

  return(data.frame(
    method              = method_name,
    estimate            = estimate,
    standard_error      = estimate_se,
    ci_lower            = estimate_lci,
    ci_upper            = estimate_uci,
    p_value             = estimate_p,
    BF                  = NA,
    convergence         = convergence,
    note                = note,
    tau_estimate        = tau_estimate,
    tau2_se             = tau2_se,
    bias_coefficient    = paste0(bias_coefficient, collapse = ", "),
    bias_coefficient_se = paste0(bias_coefficient_se, collapse = ", "),
    version             = version
  ))
}

#' @export
method_settings.AK <- function(method_name) {

  settings <- list(
    "default" = list(version = "AK1"),
    "AK1"     = list(version = "AK1"),
    "AK2"     = list(version = "AK2")
  )

  return(settings)
}

#' @export
method_extra_columns.AK <- function(method_name)
  c("tau_estimate", "tau2_se", "bias_coefficient", "bias_coefficient_se", "version")


### additional computation functions ----
# Imported and slightly modified from Hong & Reed 2021
# (https://osf.io/pr4mb/)

.method_AK_AK1logLik <- function(para, mydata, z) {
  n = nrow(mydata);
  tauhat <- para[1];
  betap <- para[2];
  Coeff <- as.matrix(para[3:length(para)])
  if(ncol(mydata)>3){
    err <- mydata[,2] - as.matrix(mydata[,c(4:ncol(mydata))])%*%Coeff;
    esthat <- as.matrix(mydata[,c(4:ncol(mydata))])%*%Coeff;
  }else{
    err <- mydata[,2] - Coeff;
    esthat <- Coeff;
  }

  se <- mydata[,3];
  t <- mydata[,2]/mydata[,3]

  cutoffs <- c(-1.96, 1.96)
  phat <- (abs(t)<1.96)*betap + (abs(t)>=1.96)*1

  meanbeta=rep(0,n);
  for (i in 1:n) {
    prob_mid <- (stats::pnorm((cutoffs[2]*se[i]-esthat[i])/sqrt((tauhat) +se[i]^2))
                 -stats::pnorm((cutoffs[1]*se[i]-esthat[i])/sqrt((tauhat) +se[i]^2)));
    prob_ex <- 1-prob_mid;
    meanbeta[i] <- betap*prob_mid + 1*prob_ex;
  }

  fX = stats::dnorm(err, 0, sqrt(se^2 + tauhat));
  L <- (phat/meanbeta)*fX;
  logL <- log(L);
  LLH <- -sum(log(L));
  if(tauhat<0 | betap<0){ LLH <- 10^10; }
  if(z=="est"){return(LLH);}else{return(logL);}
}
.method_AK_AK1logLik_LLH <- function(para, AKdata) {
  return(.method_AK_AK1logLik(para, AKdata, "est"))
}

.method_AK_AK1.est <-function(AKdata) {
  subAKdata <- AKdata[, c(2, 4:ncol(AKdata))]
  #InitialValue<-as.numeric(summary(lm(effect~.-1, data=subAKdata, weights=1/AKdata$se^2))$coefficients[,1])
  InitialValue<-as.numeric(mean(AKdata$effect))
  t2 <- mean(AKdata$se^2)/4
  Result <- tryCatch(
    {
      stats::nlminb(.method_AK_AK1logLik_LLH,
                    start=c(t2,0,InitialValue),
                    lower=c(0, 0.01, rep(-Inf,length(InitialValue))),
                    upper=c(Inf, Inf, rep(Inf,length(InitialValue))),
                    hessian=TRUE,
                    AKdata=AKdata,
                    control=list(iter.max=1000, abs.tol=10^(-20), eval.max=1000))      },
    error=function(cond){
      return(list(Error = 'error',
                  convergence = 1,
                  par = c(rep(NA, ncol(AKdata)-1)),
                  EstResults=as.data.frame(matrix(NaN, nrow=5, ncol=5))))
    }
  )

  AKEstimationResults <- as.data.frame(matrix(NA, nrow=10, ncol=4))
  colnames(AKEstimationResults) <- c("method","term","variable","value")
  AKEstimationResults$method<-"AK1"
  AKEstimationResults$term<-c(rep("b0",6),"rho1","rho1","tau2","tau2")
  AKEstimationResults$variable <- c("estimate", "std.error", "statistic", "p.value","conf.low", "conf.high","estimate","std.error","estimate","std.error")

  if(Result$convergence==1){

    AKEstimationResults$value <- NA
    return(AKEstimationResults)
  }else{
    EstCoefficients <- t(as.matrix(Result$par))
    colnames(EstCoefficients) <-  c("tau2", "Betap", colnames(AKdata)[4:ncol(AKdata)])
    StdErrors <- diag(sqrt(MASS::ginv(numDeriv::hessian(.method_AK_AK1logLik_LLH, Result$par, AKdata = AKdata), tol=10^(-30))))
    PValue <- stats::dt(EstCoefficients/StdErrors, df=nrow(AKdata)-length(EstCoefficients))
    EstResults <- rbind(EstCoefficients, StdErrors, PValue)
    rownames(EstResults) <- c("Coefficients", "Std.Err", "p-value")
    crit <- stats::qt(0.975,df=(nrow(AKdata)-length(EstCoefficients)))


    AKEstimationResults$value <- c(EstResults[1,3], EstResults[2,3], (EstResults[1,3]/EstResults[2,3]), EstResults[3,3], (EstResults[1,3]-crit*EstResults[2,3]), (EstResults[1,3]+crit*EstResults[2,3]), EstResults[1,2], EstResults[2,2],EstResults[1,1], EstResults[2,1])
    return(AKEstimationResults)
  }
}
###################
###################
###################





###################
## AK2 Estimator
###################
.method_AK_AK2logLik <- function(para, mydata, z) {
  n = nrow(mydata);
  tauhat <- para[1];
  beta1 <- para[2];
  beta2 <- para[3];
  beta3 <- para[4];

  Coeff <- as.matrix(para[5:length(para)])
  if(ncol(mydata)>3){
    err <- mydata[,2] - as.matrix(mydata[,c(4:ncol(mydata))])%*%Coeff;
    esthat <- as.matrix(mydata[,c(4:ncol(mydata))])%*%Coeff;
  }else{
    err <- mydata[,2] - Coeff;
    esthat <- Coeff;
  }

  se <- mydata[,3];
  t <- mydata[,2]/mydata[,3]

  cutoffs <- c(-1.96, 0, 1.96);
  phat <- (t<=-1.96)*beta1 + (t>-1.96 & t<=0)*beta2 + (0< t &t<1.96)*beta3 + (t>=1.96)*1

  meanbeta=rep(0,n);
  for (i in 1:n) {
    prob_vlow <- stats::pnorm((cutoffs[1]*se[i]-esthat[i])/sqrt((tauhat) +se[i]^2))
    prob_low <- (stats::pnorm((cutoffs[2]*se[i]-esthat[i])/sqrt((tauhat) +se[i]^2))
                 -stats::pnorm((cutoffs[1]*se[i]-esthat[i])/sqrt((tauhat) +se[i]^2)));
    prob_upper<- (stats::pnorm((cutoffs[3]*se[i]-esthat[i])/sqrt((tauhat) +se[i]^2))
                  -stats::pnorm((cutoffs[2]*se[i]-esthat[i])/sqrt((tauhat) +se[i]^2)));
    prob_vupper<- 1-prob_vlow-prob_low-prob_upper;
    meanbeta[i] <- beta1*prob_vlow + beta2*prob_low + beta3*prob_upper + 1*prob_vupper;
  }

  fX = stats::dnorm(err, 0, sqrt(se^2 + tauhat));
  L <- (phat/meanbeta)*fX;
  logL <- log(L);
  LLH <- -sum(log(L));
  if(tauhat<0 | beta1<0 | beta2<0 | beta3<0 ){ LLH <- 10^10; }
  if(z=="est"){return(LLH);}else{return(logL);}
}
.method_AK_AK2logLik_LLH <- function(para, AKdata) {
  return(.method_AK_AK2logLik(para, AKdata, "est"))
}


.method_AK_AK2.est <-function(AKdata) {
  subAKdata <- AKdata[, c(2, 4:ncol(AKdata))]
  #InitialValue<-as.numeric(summary(lm(effect~.-1, data=subAKdata, weights=1/AKdata$se^2))$coefficients[,1])
  InitialValue<-as.numeric(mean(AKdata$effect))
  t2 <- mean(AKdata$se^2)/4
  Result <- tryCatch(
    {
      stats::nlminb(.method_AK_AK2logLik_LLH,
                    start=c(t2,0,0,0,InitialValue),
                    lower=c(0, 0.01, 0.01, 0.01, rep(-Inf,length(InitialValue))),
                    upper=c(Inf, Inf, Inf, Inf, rep(Inf,length(InitialValue))),
                    hessian=TRUE,
                    AKdata=AKdata,
                    control=list(iter.max=1000, abs.tol=10^(-20), eval.max=1000)); },
    error=function(cond){
      return(list(Error = 'error',
                  convergence = 1,
                  par = c(rep(NA, ncol(AKdata)+1)),
                  EstResults=as.data.frame(matrix(NaN, nrow=5, ncol=5))))
    }
  )

  AKEstimationResults <- as.data.frame(matrix(NA, nrow=14, ncol=4))
  colnames(AKEstimationResults) <- c("method","term","variable","value")
  AKEstimationResults$method<-"AK2"
  AKEstimationResults$term<-c(rep("b0",6),"rho1","rho1","rho2","rho2","rho3","rho3","tau2","tau2")
  AKEstimationResults$variable <- c("estimate", "std.error", "statistic", "p.value","conf.low", "conf.high","estimate","std.error","estimate","std.error","estimate","std.error","estimate","std.error")

  err<- tryCatch(
    {
      diag(sqrt(MASS::ginv(numDeriv::hessian(.method_AK_AK2logLik_LLH, Result$par, AKdata = AKdata), tol=10^(-30)))); },
    error=function(cond){
      return("error")
    })

  if(length(err) == 1 && err=="error" || Result$convergence==1){
    AKEstimationResults$value <- NA
    return(AKEstimationResults)
  }else{
    EstCoefficients <- t(as.matrix(Result$par))
    colnames(EstCoefficients) <-  c("tau2", "Beta1", "Beta2", "Beta3", colnames(AKdata)[4:ncol(AKdata)])
    StdErrors <- diag(sqrt(MASS::ginv(numDeriv::hessian(.method_AK_AK2logLik_LLH, Result$par, AKdata = AKdata), tol=10^(-30))))
    PValue <- stats::dt(EstCoefficients/StdErrors, df=nrow(AKdata)-length(EstCoefficients))

    if(sum(is.finite(PValue))==length(EstCoefficients)){
      EstResults <- rbind(EstCoefficients, StdErrors, PValue)
      rownames(EstResults) <- c("Coefficients", "Std.Err", "p-value")
      crit <- stats::qt(0.975,df=(nrow(AKdata)-length(EstCoefficients)))
      AKEstimationResults$value <- c(EstResults[1,5], EstResults[2,5], (EstResults[1,5]/EstResults[2,5]), EstResults[3,5], (EstResults[1,5]-crit*EstResults[2,5]), (EstResults[1,5]+crit*EstResults[2,5]), EstResults[1,2], EstResults[2,2], EstResults[1,3], EstResults[2,3], EstResults[1,4], EstResults[2,4],EstResults[1,1], EstResults[2,1])
      return(AKEstimationResults)
    }else{
      AKEstimationResults$value <- NA
      return(AKEstimationResults)
    }
  }
}
###################
###################
###################





###################
## Clustered Standard Errors
###################
.method_AK_ClusteredSE_AK <-function(AK1Est, my.data, type) {
  est <- AK1Est$nlmOutput$par;
  HessMatrix <- AK1Est$hessian;
  g <- matrix(0, nrow=nrow(my.data), ncol=length(est))
  for(i in 1:length(est)){
    grad1 <- rep(0, length(est));
    grad2 <- rep(0, length(est));
    stepsize <- 10^(-6)
    grad1[i] <- -stepsize
    grad2[i] <- stepsize
    if(type=='AK1'){
      g[,i] <- (.method_AK_AK1logLik(est+grad2, my.data, "gra")-.method_AK_AK1logLik(est+grad1, my.data, "gra"))/(2*stepsize)
    }else{
      g[,i] <- (.method_AK_AK2logLik(est+grad2, my.data, "gra")-.method_AK_AK2logLik(est+grad1, my.data, "gra"))/(2*stepsize)
    }
  }

  cluster_index<-my.data[,1]
  I<-order(cluster_index);
  cluster_index<-sort(cluster_index);
  g = g[I,]
  g = g - matrix(rep(apply(g,2,mean),length(I)),nrow=length(I),byrow=TRUE);
  gsum = apply(g,2,cumsum);
  index_diff <- cluster_index[-1] != cluster_index[-length(cluster_index)];
  index_diff <- c(index_diff,1);
  gsum = gsum[index_diff==1,]
  gsum=rbind(gsum[1,], diff(gsum));
  Sigma=1/(dim(g)[1]-1)*(t(gsum)%*%gsum);
  RobuClusteredStdErr <- tryCatch(
    {
      diag(sqrt(nrow(my.data)*MASS::ginv(HessMatrix, tol=10^(-30))%*%Sigma%*%MASS::ginv(HessMatrix, tol=10^(-30))))
    },
    error=function(cond){ return('error'); }
  )
  return (RobuClusteredStdErr)
}
###################
###################
###################



