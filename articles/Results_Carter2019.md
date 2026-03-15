# Results: Carter (2019)

## Complete Results

These results are based on [Carter
(2019)](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Carter2019.md)
data-generating mechanism with a total of 756 conditions.

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
indicates a better method.

[TABLE]

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0.

[TABLE]

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance.

[TABLE]

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method.

[TABLE]

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

[TABLE]

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method.

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
methods](Results_Carter2019_files/figure-html/unnamed-chunk-62-1.png)

![Raincloud plot showing RMSE (Root Mean Square Error) across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-63-1.png)

RMSE (Root Mean Square Error) is an overall summary measure of
estimation performance that combines bias and empirical SE. RMSE is the
square root of the average squared difference between the meta-analytic
estimate and the true effect across simulation runs. A lower RMSE
indicates a better method. Values larger than 0.5 are visualized as 0.5.

![Raincloud plot showing bias across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-64-1.png)

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0. Values lower than -0.5 or larger than 0.5 are visualized as
-0.5 and 0.5 respectively.

![Raincloud plot showing bias across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-65-1.png)

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance. Values larger than 0.5 are visualized as
0.5.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-66-1.png)

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method. Values larger than 100 are visualized as 100.

![Raincloud plot showing 95% confidence interval coverage across
different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-67-1.png)

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-68-1.png)

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method.

![Raincloud plot showing positive likelihood ratio across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-69-1.png)

The positive likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a significant test result changes the odds
of the alternative hypothesis versus the null hypothesis. A useful
method has a positive likelihood ratio greater than 1 (or a log positive
likelihood ratio greater than 0). A higher (log) positive likelihood
ratio indicates a better method.

![Raincloud plot showing negative likelihood ratio across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-70-1.png)

The negative likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a non-significant test result changes the
odds of the alternative hypothesis versus the null hypothesis. A useful
method has a negative likelihood ratio less than 1 (or a log negative
likelihood ratio less than 0). A lower (log) negative likelihood ratio
indicates a better method.

![Raincloud plot showing Type I Error rates across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-71-1.png)

The type I error rate is the proportion of simulation runs in which the
null hypothesis of no effect was incorrectly rejected when it was true.
Ideally, this value should be close to the nominal level of 5%.

![Raincloud plot showing statistical power across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-72-1.png)

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
methods](Results_Carter2019_files/figure-html/unnamed-chunk-88-1.png)

![Raincloud plot showing RMSE (Root Mean Square Error) across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-89-1.png)

RMSE (Root Mean Square Error) is an overall summary measure of
estimation performance that combines bias and empirical SE. RMSE is the
square root of the average squared difference between the meta-analytic
estimate and the true effect across simulation runs. A lower RMSE
indicates a better method. Values larger than 0.5 are visualized as 0.5.

![Raincloud plot showing bias across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-90-1.png)

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0. Values lower than -0.5 or larger than 0.5 are visualized as
-0.5 and 0.5 respectively.

![Raincloud plot showing bias across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-91-1.png)

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance. Values larger than 0.5 are visualized as
0.5.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-92-1.png)

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method. Values larger than 100 are visualized as 100.

![Raincloud plot showing 95% confidence interval coverage across
different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-93-1.png)

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-94-1.png)

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method.

![Raincloud plot showing positive likelihood ratio across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-95-1.png)

The positive likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a significant test result changes the odds
of the alternative hypothesis versus the null hypothesis. A useful
method has a positive likelihood ratio greater than 1 (or a log positive
likelihood ratio greater than 0). A higher (log) positive likelihood
ratio indicates a better method.

![Raincloud plot showing negative likelihood ratio across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-96-1.png)

The negative likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a non-significant test result changes the
odds of the alternative hypothesis versus the null hypothesis. A useful
method has a negative likelihood ratio less than 1 (or a log negative
likelihood ratio less than 0). A lower (log) negative likelihood ratio
indicates a better method.

![Raincloud plot showing Type I Error rates across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-97-1.png)

The type I error rate is the proportion of simulation runs in which the
null hypothesis of no effect was incorrectly rejected when it was true.
Ideally, this value should be close to the nominal level of 5%.

![Raincloud plot showing statistical power across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-98-1.png)

The power is the proportion of simulation runs in which the null
hypothesis of no effect was correctly rejected when the alternative
hypothesis was true. A higher power indicates a better method.

## Subset: No Questionable Research Practices

These results are based on [Carter
(2019)](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Carter2019.md)
data-generating mechanism with a total of 252 conditions.

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
indicates a better method.

[TABLE]

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0.

[TABLE]

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance.

[TABLE]

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method.

[TABLE]

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

[TABLE]

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method.

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
methods](Results_Carter2019_files/figure-html/unnamed-chunk-138-1.png)

![Raincloud plot showing RMSE (Root Mean Square Error) across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-139-1.png)

RMSE (Root Mean Square Error) is an overall summary measure of
estimation performance that combines bias and empirical SE. RMSE is the
square root of the average squared difference between the meta-analytic
estimate and the true effect across simulation runs. A lower RMSE
indicates a better method. Values larger than 0.5 are visualized as 0.5.

![Raincloud plot showing bias across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-140-1.png)

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0. Values lower than -0.5 or larger than 0.5 are visualized as
-0.5 and 0.5 respectively.

![Raincloud plot showing bias across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-141-1.png)

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance. Values larger than 0.5 are visualized as
0.5.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-142-1.png)

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method. Values larger than 100 are visualized as 100.

![Raincloud plot showing 95% confidence interval coverage across
different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-143-1.png)

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-144-1.png)

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method.

![Raincloud plot showing positive likelihood ratio across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-145-1.png)

The positive likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a significant test result changes the odds
of the alternative hypothesis versus the null hypothesis. A useful
method has a positive likelihood ratio greater than 1 (or a log positive
likelihood ratio greater than 0). A higher (log) positive likelihood
ratio indicates a better method.

![Raincloud plot showing negative likelihood ratio across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-146-1.png)

The negative likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a non-significant test result changes the
odds of the alternative hypothesis versus the null hypothesis. A useful
method has a negative likelihood ratio less than 1 (or a log negative
likelihood ratio less than 0). A lower (log) negative likelihood ratio
indicates a better method.

![Raincloud plot showing Type I Error rates across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-147-1.png)

The type I error rate is the proportion of simulation runs in which the
null hypothesis of no effect was incorrectly rejected when it was true.
Ideally, this value should be close to the nominal level of 5%.

![Raincloud plot showing statistical power across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-148-1.png)

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
methods](Results_Carter2019_files/figure-html/unnamed-chunk-164-1.png)

![Raincloud plot showing RMSE (Root Mean Square Error) across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-165-1.png)

RMSE (Root Mean Square Error) is an overall summary measure of
estimation performance that combines bias and empirical SE. RMSE is the
square root of the average squared difference between the meta-analytic
estimate and the true effect across simulation runs. A lower RMSE
indicates a better method. Values larger than 0.5 are visualized as 0.5.

![Raincloud plot showing bias across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-166-1.png)

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0. Values lower than -0.5 or larger than 0.5 are visualized as
-0.5 and 0.5 respectively.

![Raincloud plot showing bias across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-167-1.png)

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance. Values larger than 0.5 are visualized as
0.5.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-168-1.png)

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method. Values larger than 100 are visualized as 100.

![Raincloud plot showing 95% confidence interval coverage across
different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-169-1.png)

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-170-1.png)

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method.

![Raincloud plot showing positive likelihood ratio across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-171-1.png)

The positive likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a significant test result changes the odds
of the alternative hypothesis versus the null hypothesis. A useful
method has a positive likelihood ratio greater than 1 (or a log positive
likelihood ratio greater than 0). A higher (log) positive likelihood
ratio indicates a better method.

![Raincloud plot showing negative likelihood ratio across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-172-1.png)

The negative likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a non-significant test result changes the
odds of the alternative hypothesis versus the null hypothesis. A useful
method has a negative likelihood ratio less than 1 (or a log negative
likelihood ratio less than 0). A lower (log) negative likelihood ratio
indicates a better method.

![Raincloud plot showing Type I Error rates across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-173-1.png)

The type I error rate is the proportion of simulation runs in which the
null hypothesis of no effect was incorrectly rejected when it was true.
Ideally, this value should be close to the nominal level of 5%.

![Raincloud plot showing statistical power across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-174-1.png)

The power is the proportion of simulation runs in which the null
hypothesis of no effect was correctly rejected when the alternative
hypothesis was true. A higher power indicates a better method.

## Subset: Medium Questionable Research Practices

These results are based on [Carter
(2019)](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Carter2019.md)
data-generating mechanism with a total of 252 conditions.

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
indicates a better method.

[TABLE]

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0.

[TABLE]

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance.

[TABLE]

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method.

[TABLE]

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

[TABLE]

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method.

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
methods](Results_Carter2019_files/figure-html/unnamed-chunk-214-1.png)

![Raincloud plot showing RMSE (Root Mean Square Error) across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-215-1.png)

RMSE (Root Mean Square Error) is an overall summary measure of
estimation performance that combines bias and empirical SE. RMSE is the
square root of the average squared difference between the meta-analytic
estimate and the true effect across simulation runs. A lower RMSE
indicates a better method. Values larger than 0.5 are visualized as 0.5.

![Raincloud plot showing bias across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-216-1.png)

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0. Values lower than -0.5 or larger than 0.5 are visualized as
-0.5 and 0.5 respectively.

![Raincloud plot showing bias across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-217-1.png)

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance. Values larger than 0.5 are visualized as
0.5.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-218-1.png)

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method. Values larger than 100 are visualized as 100.

![Raincloud plot showing 95% confidence interval coverage across
different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-219-1.png)

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-220-1.png)

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method.

![Raincloud plot showing positive likelihood ratio across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-221-1.png)

The positive likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a significant test result changes the odds
of the alternative hypothesis versus the null hypothesis. A useful
method has a positive likelihood ratio greater than 1 (or a log positive
likelihood ratio greater than 0). A higher (log) positive likelihood
ratio indicates a better method.

![Raincloud plot showing negative likelihood ratio across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-222-1.png)

The negative likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a non-significant test result changes the
odds of the alternative hypothesis versus the null hypothesis. A useful
method has a negative likelihood ratio less than 1 (or a log negative
likelihood ratio less than 0). A lower (log) negative likelihood ratio
indicates a better method.

![Raincloud plot showing Type I Error rates across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-223-1.png)

The type I error rate is the proportion of simulation runs in which the
null hypothesis of no effect was incorrectly rejected when it was true.
Ideally, this value should be close to the nominal level of 5%.

![Raincloud plot showing statistical power across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-224-1.png)

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
methods](Results_Carter2019_files/figure-html/unnamed-chunk-240-1.png)

![Raincloud plot showing RMSE (Root Mean Square Error) across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-241-1.png)

RMSE (Root Mean Square Error) is an overall summary measure of
estimation performance that combines bias and empirical SE. RMSE is the
square root of the average squared difference between the meta-analytic
estimate and the true effect across simulation runs. A lower RMSE
indicates a better method. Values larger than 0.5 are visualized as 0.5.

![Raincloud plot showing bias across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-242-1.png)

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0. Values lower than -0.5 or larger than 0.5 are visualized as
-0.5 and 0.5 respectively.

![Raincloud plot showing bias across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-243-1.png)

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance. Values larger than 0.5 are visualized as
0.5.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-244-1.png)

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method. Values larger than 100 are visualized as 100.

![Raincloud plot showing 95% confidence interval coverage across
different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-245-1.png)

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-246-1.png)

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method.

![Raincloud plot showing positive likelihood ratio across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-247-1.png)

The positive likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a significant test result changes the odds
of the alternative hypothesis versus the null hypothesis. A useful
method has a positive likelihood ratio greater than 1 (or a log positive
likelihood ratio greater than 0). A higher (log) positive likelihood
ratio indicates a better method.

![Raincloud plot showing negative likelihood ratio across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-248-1.png)

The negative likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a non-significant test result changes the
odds of the alternative hypothesis versus the null hypothesis. A useful
method has a negative likelihood ratio less than 1 (or a log negative
likelihood ratio less than 0). A lower (log) negative likelihood ratio
indicates a better method.

![Raincloud plot showing Type I Error rates across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-249-1.png)

The type I error rate is the proportion of simulation runs in which the
null hypothesis of no effect was incorrectly rejected when it was true.
Ideally, this value should be close to the nominal level of 5%.

![Raincloud plot showing statistical power across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-250-1.png)

The power is the proportion of simulation runs in which the null
hypothesis of no effect was correctly rejected when the alternative
hypothesis was true. A higher power indicates a better method.

## Subset: High Questionable Research Practices

These results are based on [Carter
(2019)](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Carter2019.md)
data-generating mechanism with a total of 252 conditions.

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
indicates a better method.

[TABLE]

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0.

[TABLE]

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance.

[TABLE]

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method.

[TABLE]

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

[TABLE]

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method.

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
methods](Results_Carter2019_files/figure-html/unnamed-chunk-290-1.png)

![Raincloud plot showing RMSE (Root Mean Square Error) across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-291-1.png)

RMSE (Root Mean Square Error) is an overall summary measure of
estimation performance that combines bias and empirical SE. RMSE is the
square root of the average squared difference between the meta-analytic
estimate and the true effect across simulation runs. A lower RMSE
indicates a better method. Values larger than 0.5 are visualized as 0.5.

![Raincloud plot showing bias across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-292-1.png)

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0. Values lower than -0.5 or larger than 0.5 are visualized as
-0.5 and 0.5 respectively.

![Raincloud plot showing bias across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-293-1.png)

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance. Values larger than 0.5 are visualized as
0.5.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-294-1.png)

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method. Values larger than 100 are visualized as 100.

![Raincloud plot showing 95% confidence interval coverage across
different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-295-1.png)

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-296-1.png)

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method.

![Raincloud plot showing positive likelihood ratio across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-297-1.png)

The positive likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a significant test result changes the odds
of the alternative hypothesis versus the null hypothesis. A useful
method has a positive likelihood ratio greater than 1 (or a log positive
likelihood ratio greater than 0). A higher (log) positive likelihood
ratio indicates a better method.

![Raincloud plot showing negative likelihood ratio across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-298-1.png)

The negative likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a non-significant test result changes the
odds of the alternative hypothesis versus the null hypothesis. A useful
method has a negative likelihood ratio less than 1 (or a log negative
likelihood ratio less than 0). A lower (log) negative likelihood ratio
indicates a better method.

![Raincloud plot showing Type I Error rates across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-299-1.png)

The type I error rate is the proportion of simulation runs in which the
null hypothesis of no effect was incorrectly rejected when it was true.
Ideally, this value should be close to the nominal level of 5%.

![Raincloud plot showing statistical power across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-300-1.png)

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
methods](Results_Carter2019_files/figure-html/unnamed-chunk-316-1.png)

![Raincloud plot showing RMSE (Root Mean Square Error) across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-317-1.png)

RMSE (Root Mean Square Error) is an overall summary measure of
estimation performance that combines bias and empirical SE. RMSE is the
square root of the average squared difference between the meta-analytic
estimate and the true effect across simulation runs. A lower RMSE
indicates a better method. Values larger than 0.5 are visualized as 0.5.

![Raincloud plot showing bias across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-318-1.png)

Bias is the average difference between the meta-analytic estimate and
the true effect across simulation runs. Ideally, this value should be
close to 0. Values lower than -0.5 or larger than 0.5 are visualized as
-0.5 and 0.5 respectively.

![Raincloud plot showing bias across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-319-1.png)

The empirical SE is the standard deviation of the meta-analytic estimate
across simulation runs. A lower empirical SE indicates less variability
and better method performance. Values larger than 0.5 are visualized as
0.5.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-320-1.png)

The interval score measures the accuracy of a confidence interval by
combining its width and coverage. It penalizes intervals that are too
wide or that fail to include the true value. A lower interval score
indicates a better method. Values larger than 100 are visualized as 100.

![Raincloud plot showing 95% confidence interval coverage across
different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-321-1.png)

95% CI coverage is the proportion of simulation runs in which the 95%
confidence interval contained the true effect. Ideally, this value
should be close to the nominal level of 95%.

![Raincloud plot showing 95% confidence interval width across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-322-1.png)

95% CI width is the average length of the 95% confidence interval for
the true effect. A lower average 95% CI length indicates a better
method.

![Raincloud plot showing positive likelihood ratio across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-323-1.png)

The positive likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a significant test result changes the odds
of the alternative hypothesis versus the null hypothesis. A useful
method has a positive likelihood ratio greater than 1 (or a log positive
likelihood ratio greater than 0). A higher (log) positive likelihood
ratio indicates a better method.

![Raincloud plot showing negative likelihood ratio across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-324-1.png)

The negative likelihood ratio is an overall summary measure of
hypothesis testing performance that combines power and type I error
rate. It indicates how much a non-significant test result changes the
odds of the alternative hypothesis versus the null hypothesis. A useful
method has a negative likelihood ratio less than 1 (or a log negative
likelihood ratio less than 0). A lower (log) negative likelihood ratio
indicates a better method.

![Raincloud plot showing Type I Error rates across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-325-1.png)

The type I error rate is the proportion of simulation runs in which the
null hypothesis of no effect was incorrectly rejected when it was true.
Ideally, this value should be close to the nominal level of 5%.

![Raincloud plot showing statistical power across different
methods](Results_Carter2019_files/figure-html/unnamed-chunk-326-1.png)

The power is the proportion of simulation runs in which the null
hypothesis of no effect was correctly rejected when the alternative
hypothesis was true. A higher power indicates a better method.

## Session Info

This report was compiled on Sun Mar 15 17:57:43 2026 (UTC) using the
following computational environment

``` r
sessionInfo()
```

    ## R version 4.5.3 (2026-03-11)
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
    ## [3] ggplot2_4.0.2                  PublicationBiasBenchmark_0.2.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] generics_0.1.4       sandwich_3.1-1       sass_0.4.10         
    ##  [4] xml2_1.5.2           stringi_1.8.7        lattice_0.22-9      
    ##  [7] httpcode_0.3.0       digest_0.6.39        magrittr_2.0.4      
    ## [10] evaluate_1.0.5       grid_4.5.3           RColorBrewer_1.1-3  
    ## [13] fastmap_1.2.0        jsonlite_2.0.0       crul_1.6.0          
    ## [16] urltools_1.7.3.1     httr_1.4.8           purrr_1.2.1         
    ## [19] viridisLite_0.4.3    textshaping_1.0.5    jquerylib_0.1.4     
    ## [22] Rdpack_2.6.6         cli_3.6.5            rlang_1.1.7         
    ## [25] triebeard_0.4.1      rbibutils_2.4.1      withr_3.0.2         
    ## [28] cachem_1.1.0         yaml_2.3.12          otel_0.2.0          
    ## [31] tools_4.5.3          memoise_2.0.1        kableExtra_1.4.0    
    ## [34] curl_7.0.0           vctrs_0.7.1          R6_2.6.1            
    ## [37] clubSandwich_0.6.2   zoo_1.8-15           lifecycle_1.0.5     
    ## [40] stringr_1.6.0        fs_1.6.7             htmlwidgets_1.6.4   
    ## [43] ragg_1.5.1           pkgconfig_2.0.3      desc_1.4.3          
    ## [46] osfr_0.2.9           pkgdown_2.2.0        bslib_0.10.0        
    ## [49] pillar_1.11.1        gtable_0.3.6         Rcpp_1.1.1          
    ## [52] glue_1.8.0           systemfonts_1.3.2    xfun_0.56           
    ## [55] tibble_3.3.1         rstudioapi_0.18.0    knitr_1.51          
    ## [58] farver_2.1.2         htmltools_0.5.9      labeling_0.4.3      
    ## [61] svglite_2.2.2        rmarkdown_2.30       compiler_4.5.3      
    ## [64] S7_0.2.1             distributional_0.6.0
