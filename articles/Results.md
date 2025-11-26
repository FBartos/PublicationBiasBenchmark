# Results: Overall

## Complete Results

These results are based on [Stanley
(2017)](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Stanley2017.md),
[Alinaghi
(2018)](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Alinaghi2018.md),
[Bom
(2019)](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Bom2019.md),
and [Carter
(2019)](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Carter2019.md)
data-generating mechanisms with a total of 1665 conditions.

### Average Performance

Method performance measures are aggregated across all simulated
conditions to provide an overall impression of method performance.
However, keep in mind that a method with a high overall ranking is not
necessarily the “best” method for a particular application. To select a
suitable method for your application, consider also non-aggregated
performance measures in conditions most relevant to your application.

- RMSE
- Bias
- Empirical SE
- Interval Score
- 95% CI Coverage
- 95% CI Width
- Log Positive Likelihood Ratio
- Log Negative Likelihood Ratio
- Type I Error Rate
- Power

[TABLE]

RMSE (Root Mean Square Error) is an overall summary measure of
estimation performance that combines bias and empirical SE. RMSE is the
square root of the average squared difference between the meta-analytic
estimate and the true effect across simulation runs. A lower RMSE
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the average RMSE is not possible because
the data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of RMSE
values on the corresponding outcome scale.

[TABLE]

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0. Methods are compared using condition-wise ranks. Direct
comparison using the average bias is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of bias
values on the corresponding outcome scale.

[TABLE]

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance. Methods are compared using condition-wise
ranks. Direct comparison using the average empirical standard error is
not possible because the data-generating mechanisms differ in the
outcome scale. See the DGM-specific results (or subresults) to see the
distribution of empirical standard error values on the corresponding
outcome scale.

[TABLE]

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the average Interval Score is not
possible because the data-generating mechanisms differ in the outcome
scale. See the DGM-specific results (or subresults) to see the
distribution of empirical standard error values on the corresponding
outcome scale.

[TABLE]

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

[TABLE]

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method. Methods are compared using condition-wise ranks. Direct
comparison using the average CI width is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of CI width
values on the corresponding outcome scale.

[TABLE]

The positive likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a significant test result changes the odds
of the alternative hypothesis versus the null hypothesis. A useful
method has a positive likelihood ratio greater than 1 (or a log positive
likelihood ratio greater than 0). A higher (log) positive likelihood
ratio indicates a better method.

[TABLE]

The negative likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a non-significant test result changes the
odds of the alternative hypothesis versus the null hypothesis. A useful
method has a negative likelihood ratio less than 1 (or a log negative
likelihood ratio less than 0). A lower (log) negative likelihood ratio
indicates a better method.

[TABLE]

The type I error rate is the proportion of simulation runs in which the
null hypothesis of no effect was incorrectly rejected when it was true.
Ideally, this value should be close to the nominal level of 5%.

[TABLE]

The power is the proportion of simulation runs in which the null
hypothesis of no effect was correctly rejected when the alternative
hypothesis was true. A higher power indicates a better method.

### By-Condition Performance (Conditional on Method Convergence)

The results below are conditional on method convergence. Note that the
methods might differ in convergence rate and are therefore not compared
on the same data sets.

- Convergence
- RMSE
- Bias
- Empirical SE
- Interval Score
- 95% CI Coverage
- 95% CI Width
- Log Positive Likelihood Ratio
- Log Negative Likelihood Ratio
- Type I Error Rate
- Power

![Raincloud plot showing convergence rates across different
methods](Results_files/figure-html/unnamed-chunk-56-1.png)

![Raincloud plot showing RMSE (Root Mean Square Error) across different
methods](Results_files/figure-html/unnamed-chunk-57-1.png)

RMSE (Root Mean Square Error) is an overall summary measure of
estimation performance that combines bias and empirical SE. RMSE is the
square root of the average squared difference between the meta-analytic
estimate and the true effect across simulation runs. A lower RMSE
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the average RMSE is not possible because
the data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of RMSE
values on the corresponding outcome scale.

![Raincloud plot showing bias across different
methods](Results_files/figure-html/unnamed-chunk-58-1.png)

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0. Methods are compared using condition-wise ranks. Direct
comparison using the average bias is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of bias
values on the corresponding outcome scale.

![Raincloud plot showing bias across different
methods](Results_files/figure-html/unnamed-chunk-59-1.png)

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance. Methods are compared using condition-wise
ranks. Direct comparison using the empirical standard error is not
possible because the data-generating mechanisms differ in the outcome
scale. See the DGM-specific results (or subresults) to see the
distribution of bias values on the corresponding outcome scale.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_files/figure-html/unnamed-chunk-60-1.png)

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the interval score is not possible
because the data-generating mechanisms differ in the outcome scale. See
the DGM-specific results (or subresults) to see the distribution of bias
values on the corresponding outcome scale.

![Raincloud plot showing 95% confidence interval coverage across
different methods](Results_files/figure-html/unnamed-chunk-61-1.png)

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_files/figure-html/unnamed-chunk-62-1.png)

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method. Methods are compared using condition-wise ranks. Direct
comparison using the average 95% CI width is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of 95% CI
width values on the corresponding outcome scale.

![Raincloud plot showing positive likelihood ratio across different
methods](Results_files/figure-html/unnamed-chunk-63-1.png)

The positive likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a significant test result changes the odds
of the alternative hypothesis versus the null hypothesis. A useful
method has a positive likelihood ratio greater than 1 (or a log positive
likelihood ratio greater than 0). A higher (log) positive likelihood
ratio indicates a better method.

![Raincloud plot showing negative likelihood ratio across different
methods](Results_files/figure-html/unnamed-chunk-64-1.png)

The negative likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a non-significant test result changes the
odds of the alternative hypothesis versus the null hypothesis. A useful
method has a negative likelihood ratio less than 1 (or a log negative
likelihood ratio less than 0). A lower (log) negative likelihood ratio
indicates a better method.

![Raincloud plot showing Type I Error rates across different
methods](Results_files/figure-html/unnamed-chunk-65-1.png)

The type I error rate is the proportion of simulation runs in which the
null hypothesis of no effect was incorrectly rejected when it was true.
Ideally, this value should be close to the nominal level of 5%.

![Raincloud plot showing statistical power across different
methods](Results_files/figure-html/unnamed-chunk-66-1.png)

The power is the proportion of simulation runs in which the null
hypothesis of no effect was correctly rejected when the alternative
hypothesis was true. A higher power indicates a better method.

### By-Condition Performance (Replacement in Case of Non-Convergence)

The results below incorporate method replacement to handle
non-convergence. If a method fails to converge, its results are replaced
with the results from a simpler method (e.g., random-effects
meta-analysis without publication bias adjustment). This emulates what a
data analyst may do in practice in case a method does not converge.
However, note that these results do not correspond to “pure” method
performance as they might combine multiple different methods. See
[Method Replacement
Strategy](https://fbartos.github.io/PublicationBiasBenchmark/articles/Results_Method_Replacement.md)
for details of the method replacement specification.

- Convergence
- RMSE
- Bias
- Empirical SE
- Interval Score
- 95% CI Coverage
- 95% CI Width
- Log Positive Likelihood Ratio
- Log Negative Likelihood Ratio
- Type I Error Rate
- Power

![Raincloud plot showing convergence rates across different
methods](Results_files/figure-html/unnamed-chunk-82-1.png)

![Raincloud plot showing RMSE (Root Mean Square Error) across different
methods](Results_files/figure-html/unnamed-chunk-83-1.png)

RMSE (Root Mean Square Error) is an overall summary measure of
estimation performance that combines bias and empirical SE. RMSE is the
square root of the average squared difference between the meta-analytic
estimate and the true effect across simulation runs. A lower RMSE
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the average RMSE is not possible because
the data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of RMSE
values on the corresponding outcome scale.

![Raincloud plot showing bias across different
methods](Results_files/figure-html/unnamed-chunk-84-1.png)

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0. Methods are compared using condition-wise ranks. Direct
comparison using the average bias is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of bias
values on the corresponding outcome scale.

![Raincloud plot showing bias across different
methods](Results_files/figure-html/unnamed-chunk-85-1.png)

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance. Methods are compared using condition-wise
ranks. Direct comparison using the empirical standard error is not
possible because the data-generating mechanisms differ in the outcome
scale. See the DGM-specific results (or subresults) to see the
distribution of bias values on the corresponding outcome scale.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_files/figure-html/unnamed-chunk-86-1.png)

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the interval score is not possible
because the data-generating mechanisms differ in the outcome scale. See
the DGM-specific results (or subresults) to see the distribution of bias
values on the corresponding outcome scale.

![Raincloud plot showing 95% confidence interval coverage across
different methods](Results_files/figure-html/unnamed-chunk-87-1.png)

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_files/figure-html/unnamed-chunk-88-1.png)

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method. Methods are compared using condition-wise ranks. Direct
comparison using the average 95% CI width is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of 95% CI
width values on the corresponding outcome scale.

![Raincloud plot showing positive likelihood ratio across different
methods](Results_files/figure-html/unnamed-chunk-89-1.png)

The positive likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a significant test result changes the odds
of the alternative hypothesis versus the null hypothesis. A useful
method has a positive likelihood ratio greater than 1 (or a log positive
likelihood ratio greater than 0). A higher (log) positive likelihood
ratio indicates a better method.

![Raincloud plot showing negative likelihood ratio across different
methods](Results_files/figure-html/unnamed-chunk-90-1.png)

The negative likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a non-significant test result changes the
odds of the alternative hypothesis versus the null hypothesis. A useful
method has a negative likelihood ratio less than 1 (or a log negative
likelihood ratio less than 0). A lower (log) negative likelihood ratio
indicates a better method.

![Raincloud plot showing Type I Error rates across different
methods](Results_files/figure-html/unnamed-chunk-91-1.png)

The type I error rate is the proportion of simulation runs in which the
null hypothesis of no effect was incorrectly rejected when it was true.
Ideally, this value should be close to the nominal level of 5%.

![Raincloud plot showing statistical power across different
methods](Results_files/figure-html/unnamed-chunk-92-1.png)

The power is the proportion of simulation runs in which the null
hypothesis of no effect was correctly rejected when the alternative
hypothesis was true. A higher power indicates a better method.

## Subset: Publication Bias Present

These results are based on [Stanley
(2017)](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Stanley2017.md),
[Alinaghi
(2018)](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Alinaghi2018.md),
[Bom
(2019)](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Bom2019.md),
and [Carter
(2019)](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Carter2019.md)
data-generating mechanisms with a total of 1143 conditions.

### Average Performance

Method performance measures are aggregated across all simulated
conditions to provide an overall impression of method performance.
However, keep in mind that a method with a high overall ranking is not
necessarily the “best” method for a particular application. To select a
suitable method for your application, consider also non-aggregated
performance measures in conditions most relevant to your application.

- RMSE
- Bias
- Empirical SE
- Interval Score
- 95% CI Coverage
- 95% CI Width
- Log Positive Likelihood Ratio
- Log Negative Likelihood Ratio
- Type I Error Rate
- Power

[TABLE]

RMSE (Root Mean Square Error) is an overall summary measure of
estimation performance that combines bias and empirical SE. RMSE is the
square root of the average squared difference between the meta-analytic
estimate and the true effect across simulation runs. A lower RMSE
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the average RMSE is not possible because
the data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of RMSE
values on the corresponding outcome scale.

[TABLE]

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0. Methods are compared using condition-wise ranks. Direct
comparison using the average bias is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of bias
values on the corresponding outcome scale.

[TABLE]

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance. Methods are compared using condition-wise
ranks. Direct comparison using the average empirical standard error is
not possible because the data-generating mechanisms differ in the
outcome scale. See the DGM-specific results (or subresults) to see the
distribution of empirical standard error values on the corresponding
outcome scale.

[TABLE]

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the average Interval Score is not
possible because the data-generating mechanisms differ in the outcome
scale. See the DGM-specific results (or subresults) to see the
distribution of empirical standard error values on the corresponding
outcome scale.

[TABLE]

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

[TABLE]

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method. Methods are compared using condition-wise ranks. Direct
comparison using the average CI width is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of CI width
values on the corresponding outcome scale.

[TABLE]

The positive likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a significant test result changes the odds
of the alternative hypothesis versus the null hypothesis. A useful
method has a positive likelihood ratio greater than 1 (or a log positive
likelihood ratio greater than 0). A higher (log) positive likelihood
ratio indicates a better method.

[TABLE]

The negative likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a non-significant test result changes the
odds of the alternative hypothesis versus the null hypothesis. A useful
method has a negative likelihood ratio less than 1 (or a log negative
likelihood ratio less than 0). A lower (log) negative likelihood ratio
indicates a better method.

[TABLE]

The type I error rate is the proportion of simulation runs in which the
null hypothesis of no effect was incorrectly rejected when it was true.
Ideally, this value should be close to the nominal level of 5%.

[TABLE]

The power is the proportion of simulation runs in which the null
hypothesis of no effect was correctly rejected when the alternative
hypothesis was true. A higher power indicates a better method.

### By-Condition Performance (Conditional on Method Convergence)

The results below are conditional on method convergence. Note that the
methods might differ in convergence rate and are therefore not compared
on the same data sets.

- Convergence
- RMSE
- Bias
- Empirical SE
- Interval Score
- 95% CI Coverage
- 95% CI Width
- Log Positive Likelihood Ratio
- Log Negative Likelihood Ratio
- Type I Error Rate
- Power

![Raincloud plot showing convergence rates across different
methods](Results_files/figure-html/unnamed-chunk-132-1.png)

![Raincloud plot showing RMSE (Root Mean Square Error) across different
methods](Results_files/figure-html/unnamed-chunk-133-1.png)

RMSE (Root Mean Square Error) is an overall summary measure of
estimation performance that combines bias and empirical SE. RMSE is the
square root of the average squared difference between the meta-analytic
estimate and the true effect across simulation runs. A lower RMSE
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the average RMSE is not possible because
the data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of RMSE
values on the corresponding outcome scale.

![Raincloud plot showing bias across different
methods](Results_files/figure-html/unnamed-chunk-134-1.png)

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0. Methods are compared using condition-wise ranks. Direct
comparison using the average bias is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of bias
values on the corresponding outcome scale.

![Raincloud plot showing bias across different
methods](Results_files/figure-html/unnamed-chunk-135-1.png)

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance. Methods are compared using condition-wise
ranks. Direct comparison using the empirical standard error is not
possible because the data-generating mechanisms differ in the outcome
scale. See the DGM-specific results (or subresults) to see the
distribution of bias values on the corresponding outcome scale.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_files/figure-html/unnamed-chunk-136-1.png)

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the interval score is not possible
because the data-generating mechanisms differ in the outcome scale. See
the DGM-specific results (or subresults) to see the distribution of bias
values on the corresponding outcome scale.

![Raincloud plot showing 95% confidence interval coverage across
different methods](Results_files/figure-html/unnamed-chunk-137-1.png)

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_files/figure-html/unnamed-chunk-138-1.png)

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method. Methods are compared using condition-wise ranks. Direct
comparison using the average 95% CI width is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of 95% CI
width values on the corresponding outcome scale.

![Raincloud plot showing positive likelihood ratio across different
methods](Results_files/figure-html/unnamed-chunk-139-1.png)

The positive likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a significant test result changes the odds
of the alternative hypothesis versus the null hypothesis. A useful
method has a positive likelihood ratio greater than 1 (or a log positive
likelihood ratio greater than 0). A higher (log) positive likelihood
ratio indicates a better method.

![Raincloud plot showing negative likelihood ratio across different
methods](Results_files/figure-html/unnamed-chunk-140-1.png)

The negative likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a non-significant test result changes the
odds of the alternative hypothesis versus the null hypothesis. A useful
method has a negative likelihood ratio less than 1 (or a log negative
likelihood ratio less than 0). A lower (log) negative likelihood ratio
indicates a better method.

![Raincloud plot showing Type I Error rates across different
methods](Results_files/figure-html/unnamed-chunk-141-1.png)

The type I error rate is the proportion of simulation runs in which the
null hypothesis of no effect was incorrectly rejected when it was true.
Ideally, this value should be close to the nominal level of 5%.

![Raincloud plot showing statistical power across different
methods](Results_files/figure-html/unnamed-chunk-142-1.png)

The power is the proportion of simulation runs in which the null
hypothesis of no effect was correctly rejected when the alternative
hypothesis was true. A higher power indicates a better method.

### By-Condition Performance (Replacement in Case of Non-Convergence)

The results below incorporate method replacement to handle
non-convergence. If a method fails to converge, its results are replaced
with the results from a simpler method (e.g., random-effects
meta-analysis without publication bias adjustment). This emulates what a
data analyst may do in practice in case a method does not converge.
However, note that these results do not correspond to “pure” method
performance as they might combine multiple different methods. See
[Method Replacement
Strategy](https://fbartos.github.io/PublicationBiasBenchmark/articles/Results_Method_Replacement.md)
for details of the method replacement specification.

- Convergence
- RMSE
- Bias
- Empirical SE
- Interval Score
- 95% CI Coverage
- 95% CI Width
- Log Positive Likelihood Ratio
- Log Negative Likelihood Ratio
- Type I Error Rate
- Power

![Raincloud plot showing convergence rates across different
methods](Results_files/figure-html/unnamed-chunk-158-1.png)

![Raincloud plot showing RMSE (Root Mean Square Error) across different
methods](Results_files/figure-html/unnamed-chunk-159-1.png)

RMSE (Root Mean Square Error) is an overall summary measure of
estimation performance that combines bias and empirical SE. RMSE is the
square root of the average squared difference between the meta-analytic
estimate and the true effect across simulation runs. A lower RMSE
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the average RMSE is not possible because
the data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of RMSE
values on the corresponding outcome scale.

![Raincloud plot showing bias across different
methods](Results_files/figure-html/unnamed-chunk-160-1.png)

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0. Methods are compared using condition-wise ranks. Direct
comparison using the average bias is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of bias
values on the corresponding outcome scale.

![Raincloud plot showing bias across different
methods](Results_files/figure-html/unnamed-chunk-161-1.png)

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance. Methods are compared using condition-wise
ranks. Direct comparison using the empirical standard error is not
possible because the data-generating mechanisms differ in the outcome
scale. See the DGM-specific results (or subresults) to see the
distribution of bias values on the corresponding outcome scale.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_files/figure-html/unnamed-chunk-162-1.png)

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the interval score is not possible
because the data-generating mechanisms differ in the outcome scale. See
the DGM-specific results (or subresults) to see the distribution of bias
values on the corresponding outcome scale.

![Raincloud plot showing 95% confidence interval coverage across
different methods](Results_files/figure-html/unnamed-chunk-163-1.png)

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_files/figure-html/unnamed-chunk-164-1.png)

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method. Methods are compared using condition-wise ranks. Direct
comparison using the average 95% CI width is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of 95% CI
width values on the corresponding outcome scale.

![Raincloud plot showing positive likelihood ratio across different
methods](Results_files/figure-html/unnamed-chunk-165-1.png)

The positive likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a significant test result changes the odds
of the alternative hypothesis versus the null hypothesis. A useful
method has a positive likelihood ratio greater than 1 (or a log positive
likelihood ratio greater than 0). A higher (log) positive likelihood
ratio indicates a better method.

![Raincloud plot showing negative likelihood ratio across different
methods](Results_files/figure-html/unnamed-chunk-166-1.png)

The negative likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a non-significant test result changes the
odds of the alternative hypothesis versus the null hypothesis. A useful
method has a negative likelihood ratio less than 1 (or a log negative
likelihood ratio less than 0). A lower (log) negative likelihood ratio
indicates a better method.

![Raincloud plot showing Type I Error rates across different
methods](Results_files/figure-html/unnamed-chunk-167-1.png)

The type I error rate is the proportion of simulation runs in which the
null hypothesis of no effect was incorrectly rejected when it was true.
Ideally, this value should be close to the nominal level of 5%.

![Raincloud plot showing statistical power across different
methods](Results_files/figure-html/unnamed-chunk-168-1.png)

The power is the proportion of simulation runs in which the null
hypothesis of no effect was correctly rejected when the alternative
hypothesis was true. A higher power indicates a better method.

## Subset: Publication Bias Absent

These results are based on [Stanley
(2017)](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Stanley2017.md),
[Alinaghi
(2018)](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Alinaghi2018.md),
[Bom
(2019)](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Bom2019.md),
and [Carter
(2019)](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Carter2019.md)
data-generating mechanisms with a total of 522 conditions.

### Average Performance

Method performance measures are aggregated across all simulated
conditions to provide an overall impression of method performance.
However, keep in mind that a method with a high overall ranking is not
necessarily the “best” method for a particular application. To select a
suitable method for your application, consider also non-aggregated
performance measures in conditions most relevant to your application.

- RMSE
- Bias
- Empirical SE
- Interval Score
- 95% CI Coverage
- 95% CI Width
- Log Positive Likelihood Ratio
- Log Negative Likelihood Ratio
- Type I Error Rate
- Power

[TABLE]

RMSE (Root Mean Square Error) is an overall summary measure of
estimation performance that combines bias and empirical SE. RMSE is the
square root of the average squared difference between the meta-analytic
estimate and the true effect across simulation runs. A lower RMSE
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the average RMSE is not possible because
the data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of RMSE
values on the corresponding outcome scale.

[TABLE]

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0. Methods are compared using condition-wise ranks. Direct
comparison using the average bias is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of bias
values on the corresponding outcome scale.

[TABLE]

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance. Methods are compared using condition-wise
ranks. Direct comparison using the average empirical standard error is
not possible because the data-generating mechanisms differ in the
outcome scale. See the DGM-specific results (or subresults) to see the
distribution of empirical standard error values on the corresponding
outcome scale.

[TABLE]

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the average Interval Score is not
possible because the data-generating mechanisms differ in the outcome
scale. See the DGM-specific results (or subresults) to see the
distribution of empirical standard error values on the corresponding
outcome scale.

[TABLE]

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

[TABLE]

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method. Methods are compared using condition-wise ranks. Direct
comparison using the average CI width is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of CI width
values on the corresponding outcome scale.

[TABLE]

The positive likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a significant test result changes the odds
of the alternative hypothesis versus the null hypothesis. A useful
method has a positive likelihood ratio greater than 1 (or a log positive
likelihood ratio greater than 0). A higher (log) positive likelihood
ratio indicates a better method.

[TABLE]

The negative likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a non-significant test result changes the
odds of the alternative hypothesis versus the null hypothesis. A useful
method has a negative likelihood ratio less than 1 (or a log negative
likelihood ratio less than 0). A lower (log) negative likelihood ratio
indicates a better method.

[TABLE]

The type I error rate is the proportion of simulation runs in which the
null hypothesis of no effect was incorrectly rejected when it was true.
Ideally, this value should be close to the nominal level of 5%.

[TABLE]

The power is the proportion of simulation runs in which the null
hypothesis of no effect was correctly rejected when the alternative
hypothesis was true. A higher power indicates a better method.

### By-Condition Performance (Conditional on Method Convergence)

The results below are conditional on method convergence. Note that the
methods might differ in convergence rate and are therefore not compared
on the same data sets.

- Convergence
- RMSE
- Bias
- Empirical SE
- Interval Score
- 95% CI Coverage
- 95% CI Width
- Log Positive Likelihood Ratio
- Log Negative Likelihood Ratio
- Type I Error Rate
- Power

![Raincloud plot showing convergence rates across different
methods](Results_files/figure-html/unnamed-chunk-208-1.png)

![Raincloud plot showing RMSE (Root Mean Square Error) across different
methods](Results_files/figure-html/unnamed-chunk-209-1.png)

RMSE (Root Mean Square Error) is an overall summary measure of
estimation performance that combines bias and empirical SE. RMSE is the
square root of the average squared difference between the meta-analytic
estimate and the true effect across simulation runs. A lower RMSE
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the average RMSE is not possible because
the data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of RMSE
values on the corresponding outcome scale.

![Raincloud plot showing bias across different
methods](Results_files/figure-html/unnamed-chunk-210-1.png)

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0. Methods are compared using condition-wise ranks. Direct
comparison using the average bias is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of bias
values on the corresponding outcome scale.

![Raincloud plot showing bias across different
methods](Results_files/figure-html/unnamed-chunk-211-1.png)

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance. Methods are compared using condition-wise
ranks. Direct comparison using the empirical standard error is not
possible because the data-generating mechanisms differ in the outcome
scale. See the DGM-specific results (or subresults) to see the
distribution of bias values on the corresponding outcome scale.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_files/figure-html/unnamed-chunk-212-1.png)

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the interval score is not possible
because the data-generating mechanisms differ in the outcome scale. See
the DGM-specific results (or subresults) to see the distribution of bias
values on the corresponding outcome scale.

![Raincloud plot showing 95% confidence interval coverage across
different methods](Results_files/figure-html/unnamed-chunk-213-1.png)

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_files/figure-html/unnamed-chunk-214-1.png)

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method. Methods are compared using condition-wise ranks. Direct
comparison using the average 95% CI width is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of 95% CI
width values on the corresponding outcome scale.

![Raincloud plot showing positive likelihood ratio across different
methods](Results_files/figure-html/unnamed-chunk-215-1.png)

The positive likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a significant test result changes the odds
of the alternative hypothesis versus the null hypothesis. A useful
method has a positive likelihood ratio greater than 1 (or a log positive
likelihood ratio greater than 0). A higher (log) positive likelihood
ratio indicates a better method.

![Raincloud plot showing negative likelihood ratio across different
methods](Results_files/figure-html/unnamed-chunk-216-1.png)

The negative likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a non-significant test result changes the
odds of the alternative hypothesis versus the null hypothesis. A useful
method has a negative likelihood ratio less than 1 (or a log negative
likelihood ratio less than 0). A lower (log) negative likelihood ratio
indicates a better method.

![Raincloud plot showing Type I Error rates across different
methods](Results_files/figure-html/unnamed-chunk-217-1.png)

The type I error rate is the proportion of simulation runs in which the
null hypothesis of no effect was incorrectly rejected when it was true.
Ideally, this value should be close to the nominal level of 5%.

![Raincloud plot showing statistical power across different
methods](Results_files/figure-html/unnamed-chunk-218-1.png)

The power is the proportion of simulation runs in which the null
hypothesis of no effect was correctly rejected when the alternative
hypothesis was true. A higher power indicates a better method.

### By-Condition Performance (Replacement in Case of Non-Convergence)

The results below incorporate method replacement to handle
non-convergence. If a method fails to converge, its results are replaced
with the results from a simpler method (e.g., random-effects
meta-analysis without publication bias adjustment). This emulates what a
data analyst may do in practice in case a method does not converge.
However, note that these results do not correspond to “pure” method
performance as they might combine multiple different methods. See
[Method Replacement
Strategy](https://fbartos.github.io/PublicationBiasBenchmark/articles/Results_Method_Replacement.md)
for details of the method replacement specification.

- Convergence
- RMSE
- Bias
- Empirical SE
- Interval Score
- 95% CI Coverage
- 95% CI Width
- Log Positive Likelihood Ratio
- Log Negative Likelihood Ratio
- Type I Error Rate
- Power

![Raincloud plot showing convergence rates across different
methods](Results_files/figure-html/unnamed-chunk-234-1.png)

![Raincloud plot showing RMSE (Root Mean Square Error) across different
methods](Results_files/figure-html/unnamed-chunk-235-1.png)

RMSE (Root Mean Square Error) is an overall summary measure of
estimation performance that combines bias and empirical SE. RMSE is the
square root of the average squared difference between the meta-analytic
estimate and the true effect across simulation runs. A lower RMSE
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the average RMSE is not possible because
the data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of RMSE
values on the corresponding outcome scale.

![Raincloud plot showing bias across different
methods](Results_files/figure-html/unnamed-chunk-236-1.png)

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0. Methods are compared using condition-wise ranks. Direct
comparison using the average bias is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of bias
values on the corresponding outcome scale.

![Raincloud plot showing bias across different
methods](Results_files/figure-html/unnamed-chunk-237-1.png)

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance. Methods are compared using condition-wise
ranks. Direct comparison using the empirical standard error is not
possible because the data-generating mechanisms differ in the outcome
scale. See the DGM-specific results (or subresults) to see the
distribution of bias values on the corresponding outcome scale.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_files/figure-html/unnamed-chunk-238-1.png)

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method. Methods are compared using condition-wise
ranks. Direct comparison using the interval score is not possible
because the data-generating mechanisms differ in the outcome scale. See
the DGM-specific results (or subresults) to see the distribution of bias
values on the corresponding outcome scale.

![Raincloud plot showing 95% confidence interval coverage across
different methods](Results_files/figure-html/unnamed-chunk-239-1.png)

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_files/figure-html/unnamed-chunk-240-1.png)

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method. Methods are compared using condition-wise ranks. Direct
comparison using the average 95% CI width is not possible because the
data-generating mechanisms differ in the outcome scale. See the
DGM-specific results (or subresults) to see the distribution of 95% CI
width values on the corresponding outcome scale.

![Raincloud plot showing positive likelihood ratio across different
methods](Results_files/figure-html/unnamed-chunk-241-1.png)

The positive likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a significant test result changes the odds
of the alternative hypothesis versus the null hypothesis. A useful
method has a positive likelihood ratio greater than 1 (or a log positive
likelihood ratio greater than 0). A higher (log) positive likelihood
ratio indicates a better method.

![Raincloud plot showing negative likelihood ratio across different
methods](Results_files/figure-html/unnamed-chunk-242-1.png)

The negative likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a non-significant test result changes the
odds of the alternative hypothesis versus the null hypothesis. A useful
method has a negative likelihood ratio less than 1 (or a log negative
likelihood ratio less than 0). A lower (log) negative likelihood ratio
indicates a better method.

![Raincloud plot showing Type I Error rates across different
methods](Results_files/figure-html/unnamed-chunk-243-1.png)

The type I error rate is the proportion of simulation runs in which the
null hypothesis of no effect was incorrectly rejected when it was true.
Ideally, this value should be close to the nominal level of 5%.

![Raincloud plot showing statistical power across different
methods](Results_files/figure-html/unnamed-chunk-244-1.png)

The power is the proportion of simulation runs in which the null
hypothesis of no effect was correctly rejected when the alternative
hypothesis was true. A higher power indicates a better method.

## Session Info

This report was compiled on Wed Nov 26 14:26:16 2025 (UTC) using the
following computational environment

``` r
sessionInfo()
```

    ## R version 4.5.2 (2025-10-31)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.3 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    ##  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    ##  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    ## [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] scales_1.4.0                   ggdist_3.3.3                  
    ## [3] ggplot2_4.0.1                  PublicationBiasBenchmark_0.1.2
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] generics_0.1.4       sandwich_3.1-1       sass_0.4.10         
    ##  [4] xml2_1.5.0           stringi_1.8.7        lattice_0.22-7      
    ##  [7] httpcode_0.3.0       digest_0.6.39        magrittr_2.0.4      
    ## [10] evaluate_1.0.5       grid_4.5.2           RColorBrewer_1.1-3  
    ## [13] fastmap_1.2.0        jsonlite_2.0.0       crul_1.6.0          
    ## [16] urltools_1.7.3.1     httr_1.4.7           purrr_1.2.0         
    ## [19] viridisLite_0.4.2    textshaping_1.0.4    jquerylib_0.1.4     
    ## [22] Rdpack_2.6.4         cli_3.6.5            rlang_1.1.6         
    ## [25] triebeard_0.4.1      rbibutils_2.4        withr_3.0.2         
    ## [28] cachem_1.1.0         yaml_2.3.10          tools_4.5.2         
    ## [31] memoise_2.0.1        kableExtra_1.4.0     curl_7.0.0          
    ## [34] vctrs_0.6.5          R6_2.6.1             clubSandwich_0.6.1  
    ## [37] zoo_1.8-14           lifecycle_1.0.4      stringr_1.6.0       
    ## [40] fs_1.6.6             htmlwidgets_1.6.4    ragg_1.5.0          
    ## [43] pkgconfig_2.0.3      desc_1.4.3           osfr_0.2.9          
    ## [46] pkgdown_2.2.0        bslib_0.9.0          pillar_1.11.1       
    ## [49] gtable_0.3.6         Rcpp_1.1.0           glue_1.8.0          
    ## [52] systemfonts_1.3.1    xfun_0.54            tibble_3.3.0        
    ## [55] rstudioapi_0.17.1    knitr_1.50           farver_2.1.2        
    ## [58] htmltools_0.5.8.1    labeling_0.4.3       svglite_2.2.2       
    ## [61] rmarkdown_2.30       compiler_4.5.2       S7_0.2.1            
    ## [64] distributional_0.5.0
