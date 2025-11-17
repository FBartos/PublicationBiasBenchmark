# Performance Measures and Monte Carlo Standard Errors

A comprehensive set of functions for computing performance measures and
their Monte Carlo Standard Errors (MCSE) for simulation studies. All
functions are based on definitions from Table 3 in Siepe et al. (2024) .
Winkler interval score is defined in Winkler (1972) . Positive and
negative likelihood ratios are defined in Huang and Trinquart (2023) and
Deeks and Altman (2004) . Also see Morris et al. (2019) for additional
details. Bias and relative bias were modified to account for possibly
different true values across repetitions.

## Usage

``` r
bias(theta_hat, theta)

bias_mcse(theta_hat)

relative_bias(theta_hat, theta)

relative_bias_mcse(theta_hat, theta)

mse(theta_hat, theta)

mse_mcse(theta_hat, theta)

rmse(theta_hat, theta)

rmse_mcse(theta_hat, theta)

empirical_variance(theta_hat)

empirical_variance_mcse(theta_hat)

empirical_se(theta_hat)

empirical_se_mcse(theta_hat)

coverage(ci_lower, ci_upper, theta)

coverage_mcse(ci_lower, ci_upper, theta)

power(test_rejects_h0)

power_mcse(test_rejects_h0)

mean_ci_width(ci_upper, ci_lower)

mean_ci_width_mcse(ci_upper, ci_lower)

mean_generic_statistic(G)

mean_generic_statistic_mcse(G)

positive_likelihood_ratio(tp, fp, fn, tn)

positive_likelihood_ratio_mcse(tp, fp, fn, tn)

negative_likelihood_ratio(tp, fp, fn, tn)

negative_likelihood_ratio_mcse(tp, fp, fn, tn)

interval_score(ci_lower, ci_upper, theta, alpha = 0.05)

interval_score_mcse(ci_lower, ci_upper, theta, alpha = 0.05)
```

## Arguments

- theta_hat:

  Vector of parameter estimates from simulations

- theta:

  True parameter value

- ci_lower:

  Vector of lower confidence interval bounds

- ci_upper:

  Vector of upper confidence interval bounds

- test_rejects_h0:

  Logical vector indicating whether statistical tests reject the null
  hypothesis

- G:

  Vector of generic statistics from simulations

- tp:

  Numeric with the count of true positive hypothesis tests

- fp:

  Numeric with the count of false positive hypothesis tests

- fn:

  Numeric with the count of false negative hypothesis tests

- tn:

  Numeric with the count of true negative hypothesis tests

- alpha:

  Numeric indicating the 1 - coverage level for interval_score
  calculation

## Value

Each metric function returns a numeric value representing the
performance measure. Each MCSE function returns a numeric value
representing the Monte Carlo standard error.

## Details

The package provides the following performance measures and their
corresponding MCSE functions:

- `bias(theta_hat, theta)`: Bias estimate

- `relative_bias(theta_hat, theta)`: Relative bias estimate

- `mse(theta_hat, theta)`: Mean Square Error

- `rmse(theta_hat, theta)`: Root Mean Square Error

- `empirical_variance(theta_hat)`: Empirical variance

- `empirical_se(theta_hat)`: Empirical standard error

- `coverage(ci_lower, ci_upper, theta)`: Coverage probability

- `mean_ci_width(ci_upper, ci_lower)`: Mean confidence interval width

- `interval_score(ci_lower, ci_upper, theta, alpha)`: interval_score

- `power(test_rejects_h0)`: Statistical power

- `positive_likelihood_ratio(tp, fp, fn, tn)`: Log positive likelihood
  ratio

- `negative_likelihood_ratio(tp, fp, fn, tn)`: Log negative likelihood
  ratio

- `mean_generic_statistic(G)`: Mean of any generic statistic

## References

Deeks JJ, Altman DG (2004). “Diagnostic tests 4: likelihood ratios.”
*BMJ*, **329**(7458), 168–169.
[doi:10.1136/bmj.329.7458.168](https://doi.org/10.1136/bmj.329.7458.168)
.  
  
Huang Q, Trinquart L (2023). “Relative likelihood ratios for neutral
comparisons of statistical tests in simulation studies.” *Biometrical
Journal*, **66**(1), 2200102.
[doi:10.1002/bimj.202200102](https://doi.org/10.1002/bimj.202200102) .  
  
Morris TP, White IR, Crowther MJ (2019). “Using simulation studies to
evaluate statistical methods.” *Statistics in Medicine*, **38**(11),
2074–2102. [doi:10.1002/sim.8086](https://doi.org/10.1002/sim.8086) .  
  
Siepe BS, Bartoš F, Morris TP, Boulesteix A, Heck DW, Pawel S (2024).
“Simulation studies for methodological research in psychology: A
standardized template for planning, preregistration, and reporting.”
*Psychological Methods*.
[doi:10.1037/met0000695](https://doi.org/10.1037/met0000695) .  
  
Winkler RL (1972). “A decision-theoretic approach to interval
estimation.” *Journal of the American Statistical Association*,
**67**(337), 187–191.
[doi:10.1080/01621459.1972.10481224](https://doi.org/10.1080/01621459.1972.10481224)
.

## Examples

``` r
# Generate some example data
set.seed(123)
theta_true <- 0.5
theta_estimates <- rnorm(1000, mean = theta_true, sd = 0.1)

# Compute bias and its MCSE
bias_est <- bias(theta_estimates, theta_true)
bias_se <- bias_mcse(theta_estimates)

# Compute MSE and its MCSE
mse_est <- mse(theta_estimates, theta_true)
mse_se <- mse_mcse(theta_estimates, theta_true)

# Example with coverage
ci_lower <- theta_estimates - 1.96 * 0.1
ci_upper <- theta_estimates + 1.96 * 0.1
coverage_est <- coverage(ci_lower, ci_upper, theta_true)
coverage_se <- coverage_mcse(ci_lower, ci_upper, theta_true)
```
