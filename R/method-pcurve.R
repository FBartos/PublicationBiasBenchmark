#' @title pcurve (P-Curve) Method
#'
#' @description
#' Implements the p-Curve method which analyzes the distribution of p-values from significant studies to
#' assess whether the significant findings reflect true effects or QRP/publication bias.
#' The method also provides tests for the evidential value, lack of evidential value,
#' and p-hacking. See
#' \insertCite{simonsohn2014pcurve;textual}{PublicationBiasBenchmark} for details.
#'
#' The current implementation does not provide a test against the null hypothsis of no effect
#' and does not produce confidence intervals of the estimate.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes), sei (standard errors), and ni
#' (sample sizes wherever available, otherwise set to Inf)
#' @param settings List of method settings (see Details)
#'
#' @return Data frame with P-Curve results
#'
#' @details
#' The following settings are implemented \describe{
#'   \item{\code{"default"}}{no options}
#' }
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
#' # Apply pcurve method
#' result <- run_method("pcurve", data)
#' print(result)
#'
#' @export
method.pcurve <- function(method_name, data, settings) {

  # Extract data
  effect_sizes    <- data$yi
  standard_errors <- data$sei
  if (is.null(data[["ni"]])) {
    sample_sizes <- rep(Inf, nrow(data))
  } else {
    sample_sizes <- data$ni
  }

  # Check input
  if (length(effect_sizes) < 1)
    stop("At least 1 estimates is required for p-curve analysis", call. = FALSE)

  res <- .method_pcurve_pcurveEst(
    t  = effect_sizes / standard_errors,
    df = sample_sizes - 2,
    progress = FALSE, long = TRUE, CI = FALSE)

  if(isFALSE(res))
    stop("At least 1 statistically significant estimate is required for p-curve analysis", call. = FALSE)

  # PLACEHOLDER VALUES - REPLACE WITH ACTUAL COMPUTATIONS
  estimate     <- res$value[res$variable == "estimate" & res$term == "b0"]
  estimate_lci <- res$value[res$variable == "conf.low" & res$term == "b0"]
  estimate_uci <- res$value[res$variable == "conf.high" & res$term == "b0"]
  estimate_se  <- NA
  estimate_p   <- NA

  res_skew <- try(.method_pcurve_pc_skew(
    t.value  = effect_sizes / standard_errors,
    df       = sample_sizes - 2,
  ))
  if (inherits(res_skew, "try-error")) {
    p_value_evidence <- NA
    p_value_lack     <- NA
    p_value_hack     <- NA
  } else {
    p_value_evidence <- res_skew$p.value[res_skew$method == "pcurve.evidence"]
    p_value_lack     <- res_skew$p.value[res_skew$method == "pcurve.hack"]
    p_value_hack     <- res_skew$p.value[res_skew$method == "pcurve.lack"]
  }

  convergence <- TRUE
  note        <- NA

  return(data.frame(
    method                = method_name,
    estimate              = estimate,
    standard_error        = estimate_se,
    ci_lower              = estimate_lci,
    ci_upper              = estimate_uci,
    p_value               = estimate_p,
    BF                    = NA,
    convergence           = convergence,
    note                  = note,
    p_value_evidence      = p_value_evidence,
    p_value_lack          = p_value_lack,
    p_value_hack          = p_value_hack
  ))
}

#' @export
method_settings.pcurve <- function(method_name) {

  settings <- list(
    "default" = list()
  )

  return(settings)
}

#' @export
method_extra_columns.pcurve <- function(method_name)
  c("p_value_evidence", "p_value_lack", "p_value_hack")


### additional computation functions ----
# Imported and slightly modified from Hong & Reed 2021
# (https://osf.io/pr4mb/)
# Helper functions for p-curve analysis

#### functions for p-curve ES. Code adapted from Uri Simonsohn ####

######################################
# pcurve_loss function
# this code mirrors the functionality of the original loss function
# written by Simonsohn et al.
.method_pcurve_pcurve_loss <- function(pc_data, dobs) {
  #options(warn=-1)
  t.sig <- pc_data$t_obs
  df.sig <- pc_data$df_obs
  ncp_est <- sqrt((df.sig+2)/4)*dobs
  tc <- stats::qt(.975, df.sig)
  power_est <- 1-stats::pt(tc, df.sig, ncp_est)
  p_larger <- stats::pt(t.sig,df=df.sig,ncp=ncp_est)
  ppr <- (p_larger-(1-power_est))/power_est

  # Problem: ks.test gives an error if the number of test statistics is small and
  # bootstrapping selects a weird sample. In case of errors, return a large loss value
  KSD <- tryCatch({
    stats::ks.test(ppr, stats::punif)$statistic
  }, error = function(e) {
    return(1e10) # return a large loss function value
  })

  # print progression of loss function
  #cat(paste0("dobs=", round(dobs, 3), "; loss=", round(KSD, 3), "\n"))

  #options(warn=0)
  return(KSD)
}

##########################
# # .method_pcurve_pcurveEst is the function that should be called to provide p-curve# estimates of effect size, in addition to bootstrapped confidence intervals# (if desired). See parameters below for details.#@param t t-values
# @param df degrees of freedom
# @param CI Should the CI be computed? (Needs bootstrapping; takes long)
# @param level The coverage of the CI (default: 95%)
# @param B Number of bootstrap samples for CI
# @param progress Should a progress bar be displayed for the CI bootstrapping?
# @param long Should the results be returned in long format?
.method_pcurve_pcurveEst <- function(t, df, CI=TRUE, level=.95, B=1000, progress=TRUE, long=TRUE) {

  out <- matrix(NA, 1, 3)
  colnames(out) <- c("dPcurve","lbPcurve","ubPcurve")

  # define dmin and dmax (the range of parameter search)
  dmin <- 0
  dmax <- 4

  # .method_pcurve_pcurve_prep is called first to sort the data into a frame and verify it is compatible
  pc_data = .method_pcurve_pcurve_prep(t_obs = t, df_obs = df)
  # next we check to make sure we have more than 0 rows (at least 1 study); if not, return a null
  if(nrow(pc_data) == 0){
    return(FALSE)
  }

  # now let's get the pcurve ES estimate
  #out[1, 1] <- optim(par=0, fn=pcurve_loss, pc_data = pc_data, method="BFGS")$par

  # Update from ARawles - limits to positive results (which is intended by pcurve anyway)
  out[1,1] <- stats::optimize(.method_pcurve_pcurve_loss, interval = c(0, 4), pc_data = pc_data)$minimum

  if (CI==TRUE) {
    warning("CI not properly implemented.")
    #d.boot <- pcurve_estimate_d_CI(pc_data = pc_data, dmin=dmin, dmax=dmax, B=B, progress=progress)
    #CI.est <- quantile(d.boot, prob=c((1-level)/2, 1-(1-level)/2))
    #out[1, 2:3] <- CI.est
  }

  if (long==FALSE) {
    return(out)
  } else {
    outlong <- data.frame(method="pcurve", term="b0", variable=c("estimate", "conf.low", "conf.high"), value=out[1, ])

    # add number of significant studies that entered p-curve
    outlong <- rbind(outlong, data.frame(
      method="pcurve",
      term="kSig",
      variable="estimate",
      value=nrow(pc_data)
    ))

    rownames(outlong) <- NULL
    return(outlong)
  }
}


##########################
# .method_pcurve_pcurve_prep takes in vectors of t-values and associated degs. freedom
# packages everything into a data.frame
# strips out anything with p > .05
# also strips out any negative t-values
# all pre-processing and validation should go in here
.method_pcurve_pcurve_prep <- function(t_obs, df_obs){
  # first, calculate p-values and d-values for all studies
  d_vals = (t_obs*2)/sqrt(df_obs)
  p_vals = (1 - stats::pt(t_obs, df_obs)) * 2
  # then, shove everything into a data.frame to keep things organized
  unfiltered_data = data.frame(t_obs, df_obs, p_vals, d_vals)
  # now for the checks.
  # strip out anything that is NS, or if t < 0.
  # Note that we're not throwing any warnings out here.
  unfiltered_data = unfiltered_data[unfiltered_data$t_obs   > 0  ,,drop = FALSE]
  clean_data      = unfiltered_data[unfiltered_data$p_vals < .05 ,,drop = FALSE]
  # all done!
  return(clean_data)
}



######################################
# pcurve_estimate_d_CI obtains bootstrapped resamples of the provided dataset
# and estimates the ES of every resample using pcurve.
pcurve_estimate_d_CI <- function(pc_data, dmin, dmax, B, progress=TRUE) {
  d.boot <- c()
  # if (progress==TRUE) {
  #   require(progress)
  #   pb <- progress_bar$new(format="Bootstrapping [:bar] :percent ETA: :eta", total=B, clear=FALSE)
  # }

  for (i in 1:B) {
    # if (progress==TRUE) pb$tick()
    # get a random resample, with replacement
    # note that sample() doesn't work here, necessary to use sample_n()

    # changed from:
    # resample_data = dplyr::sample_n(pc_data, length(pc_data$t_obs), replace=TRUE)
    resample_data <- pc_data[sample(seq_len(nrow(pc_data)), nrow(pc_data), TRUE),]
    #print(resample_data)
    #
    d.boot <- c(d.boot, stats::optimize(.method_pcurve_pcurve_loss, c(dmin, dmax), pc_data = resample_data)$minimum)
  }

  return(d.boot)
}



# ---------------------------------------------------------------------
# These p-curve functions are partially copied, partially adapted from Uri Simonsohn's (uws@wharton.upenn.edu) original p-curve functions
# http://p-curve.com/Supplement/Rcode_other/R%20Code%20behind%20p-curve%20app%203.0%20-%20distributable.R


.method_pcurve_clamp <- function(x, MIN=.00001, MAX=.99999) {x[x<MIN] <- MIN; x[x>MAX] <- MAX; x}

# ---------------------------------------------------------------------
# p-curve-app 3.0 functions

# functions that find noncentrality parameter for t,f,chi distributions that gives 33% power for those d.f.

#t-test
.method_pcurve_ncp33t <- function(df, power=1/3, p.crit=.05) {
  xc = stats::qt(p=1-p.crit/2, df=df)
  #Find noncentrality parameter (ncp) that leads 33% power to obtain xc
  f = function(delta, pr, x, df) stats::pt(x, df = df, ncp = delta) - (1-power)
  out = stats::uniroot(f, c(0, 37.62), x = xc, df = df)
  return(out$root)
}


.method_pcurve_ncp33z <- function(power=1/3, p.crit=.05) {
  xc = stats::qnorm(p=1-p.crit/2)
  #Find noncentrality parameter (ncp) that leads 33% power to obtain xc
  f = function(delta, pr, x) stats::pnorm(x, mean = delta) - (1-power)
  out = stats::uniroot(f, c(0, 37.62), x = xc)
  return(out$root)
}


#F-test
.method_pcurve_ncp33f <- function(df1, df2, power=1/3, p.crit=.05) {
  xc=stats::qf(p=1-p.crit,df1=df1,df2=df2)
  f = function(delta, pr, x, df1,df2) stats::pf(x, df1 = df1, df2=df2, ncp = delta) - (1-power)
  out = stats::uniroot(f, c(0, 37.62), x = xc, df1=df1, df2=df2)
  return(out$root)
}

#chi-square
.method_pcurve_ncp33chi <- function(df, power=1/3, p.crit=.05) {
  xc=stats::qchisq(p=1-p.crit, df=df)
  #Find noncentrality parameter (ncp) that leads 33% power to obtain xc
  f = function(delta, pr, x, df) stats::pchisq(x, df = df, ncp = delta) - (1-power)
  out = stats::uniroot(f, c(0, 37.62), x = xc, df = df)
  return(out$root)
}



get_pp_values <- function(type, statistic, df, df2, p.crit=.05, power=1/3) {

  # convert r to t values
  type <- as.character(type)
  statistic[tolower(type)=="r"] <- statistic[tolower(type)=="r"] / sqrt( (1 - statistic[tolower(type)=="r"]^2) / df[tolower(type)=="r"])
  type[tolower(type)=="r"] <- "t"

  statistic <- abs(statistic)

  res <- data.frame()
  ncp <- data.frame()
  for (i in 1:length(type)) {
    switch(tolower(type[i]),
           "t" = {
             p <- 2*(1-stats::pt(abs(statistic[i]),df=df[i]))
             ppr <- p*(1/p.crit)	# pp-value for right-skew
             ppl <- 1-ppr		# pp-value for left-skew
             ncp33 <- .method_pcurve_ncp33t(df[i], power=power, p.crit=p.crit)
             pp33 <- (stats::pt(statistic[i],  df=df[i], ncp=ncp33)-(1-power))*(1/power)
           },
           "f" = {
             p <- 1-stats::pf(abs(statistic[i]), df1=df[i], df2=df2[i])
             ppr <- p*(1/p.crit)	# pp-value for right-skew
             ppl <- 1-ppr		# pp-value for left-skew
             ncp33 <- .method_pcurve_ncp33f(df1=df[i], df2=df2[i], power=power, p.crit=p.crit)
             pp33 <- (stats::pf(statistic[i], df1=df[i], df2=df2[i],  ncp=ncp33)-(1-power))*(1/power)
           },
           "z" = {
             p <- 2*(1-stats::pnorm(abs(statistic[i])))
             ppr <- p*(1/p.crit)	# pp-value for right-skew
             ppl <- 1-ppr		# pp-value for left-skew

             ncp33 <- .method_pcurve_ncp33z(power=power, p.crit=p.crit)
             pp33 <- (stats::pnorm(statistic[i], mean=ncp33, sd=1)-(1-power))*(1/power)
           },
           "p" = {
             p <- statistic[i]
             z <- stats::qnorm(p/2, lower.tail=FALSE)
             ppr <- p*(1/p.crit)	# pp-value for right-skew
             ppl <- 1-ppr		# pp-value for left-skew

             ncp33 <- .method_pcurve_ncp33z(power=power, p.crit=p.crit)
             pp33 <- (stats::pnorm(z, mean=ncp33, sd=1)-(1-power))*(1/power)
           },
           "chi2" = {
             p <- 1-stats::pchisq(abs(statistic[i]), df=df[i])
             ppr <- p*(1/p.crit)	# pp-value for right-skew
             ppl <- 1-ppr		# pp-value for left-skew
             ncp33 <- .method_pcurve_ncp33chi(df[i], power=power, p.crit=p.crit)
             pp33 <- (stats::pchisq(statistic[i],  df=df[i], ncp=ncp33)-(1-power))*(1/power)
           },
           {
             # default
             warning(paste0("Test statistic ", type[i], " not suported by p-curve."))
           }
    )
    res <- rbind(res, data.frame(p=p, ppr=ppr, ppl=ppl, pp33=pp33))
    ncp <- rbind(ncp, data.frame(type=type[i], df=df[i], df2=df2[i], ncp=ncp33))
  }

  if (nrow(res) > 0) {
    # .method_pcurve_clamp to extreme values
    res$ppr <- .method_pcurve_clamp(res$ppr, MIN=.00001, MAX=.99999)
    res$ppl <- .method_pcurve_clamp(res$ppl, MIN=.00001, MAX=.99999)
    res$pp33 <- .method_pcurve_clamp(res$pp33, MIN=.00001, MAX=.99999)

    # remove non-significant values
    res[res$p > p.crit, ] <- NA

    return(list(res=res, ncp=ncp))
  } else {
    return(NULL)
  }
}





# ---------------------------------------------------------------------
# New p-curve computation (p-curve app 3.0, http://www.p-curve.com/app3/)
.method_pcurve_p_curve_3 <- function(pps) {

  pps <- stats::na.omit(pps)

  # STOUFFER: Overall tests aggregating pp-values
  ktot <- sum(!is.na(pps$ppr))
  Z_ppr <- sum(stats::qnorm(pps$ppr))/sqrt(ktot)          # right skew
  Z_ppl <- sum(stats::qnorm(pps$ppl))/sqrt(ktot)          # left skew
  Z_pp33<- sum(stats::qnorm(pps$pp33))/sqrt(ktot)         # 33%

  p_ppr <- stats::pnorm(Z_ppr)
  p_ppl <- stats::pnorm(Z_ppl)
  p_pp33<- stats::pnorm(Z_pp33)

  return(list(
    Z_evidence = Z_ppr,
    p_evidence = p_ppr,
    Z_hack = Z_ppl,
    p_hack = p_ppl,
    Z_lack = Z_pp33,
    p_lack = p_pp33,
    inconclusive = ifelse(p_ppr>.05 & p_ppl>.05 & p_pp33>.05, TRUE, FALSE)))
}


# ---------------------------------------------------------------------
# Old p-curve computation (p-curve app 2.0, http://www.p-curve.com/app2/)
.method_pcurve_p_curve_2 <- function(pps) {

  pps <- stats::na.omit(pps)

  df <- 2*sum(nrow(pps))

  chi2_evidence <- -2*sum(log(pps$ppr), na.rm=TRUE)
  p_evidence <- stats::pchisq(chi2_evidence, df=df, lower.tail=FALSE)

  chi2_hack <- -2*sum(log(pps$ppl), na.rm=TRUE)
  p_hack <- stats::pchisq(chi2_hack, df=df, lower.tail=FALSE)

  chi2_lack <- -2*sum(log(pps$pp33), na.rm=TRUE)
  p_lack <- stats::pchisq(chi2_lack, df=df, lower.tail=FALSE)

  return(list(
    chi2_evidence = chi2_evidence,
    p_evidence = p_evidence,
    chi2_hack = chi2_hack,
    p_hack = p_hack,
    chi2_lack = chi2_lack,
    p_lack = p_lack,
    df = df,
    inconclusive = ifelse(p_evidence>.05 & p_hack>.05 & p_lack>.05, TRUE, FALSE)))
}



.method_pcurve_theoretical_power_curve <- function(power=1/3, p.max=.05, normalize=TRUE) {
  # compute arbitrary test statistics for requested power
  d <- 0.2
  n <- pwr::pwr.t.test(d=0.2, power=power)$n*2

  crit <- seq(0.01, p.max, by=.01)
  pdens <- c()
  for (cr in crit) {
    pdens <- c(pdens, pwr::pwr.t.test(d=0.2, power=NULL, n=n/2, sig.level=cr)$power)
  }
  p.dens <- diff(c(0, pdens))
  if (normalize == TRUE) p.dens <- p.dens/sum(p.dens)

  names(p.dens) <- as.character(crit)
  return(p.dens)
}


.method_pcurve_pc_skew <- function(t.value, df, long=TRUE) {

  # only select directionally consistent effects
  df <- df[t.value > 0]
  t.value <- t.value[t.value > 0]

  if (length(t.value) >= 1) {

    pp <- get_pp_values(type=rep("t", length(t.value)), statistic=t.value, df=df, df2=NA)

    pc_skew <- .method_pcurve_p_curve_3(pp$res)

    res <- data.frame(
      method    = c("pcurve.evidence", "pcurve.hack", "pcurve.lack"),
      term      = "skewtest",
      statistic = c(pc_skew$Z_evidence, pc_skew$Z_hack,  pc_skew$Z_lack),
      p.value   = c(pc_skew$p_evidence, pc_skew$p_hack,  pc_skew$p_lack)
    )

  } else {
    res <- data.frame(
      method    = c("pcurve.evidence", "pcurve.hack", "pcurve.lack"),
      term      = "skewtest",
      statistic = NA,
      p.value   = NA
    )
  }

  return(res)
}


