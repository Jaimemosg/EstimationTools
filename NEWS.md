# EstimationTools 4.3.1
- Build everything in R 4.5.1.
- Fix S3 method consistency in `maxlogLreg()`.

# EstimationTools 4.3.0
- Fix log-likelihood in `maxlogLreg()`.
- Enable further input arguments for legend in plots of residuals.

# EstimationTools 4.2.0
- Implement plots of residuals (diagnostic plots).

# EstimationTools 4.1.1

- Implementation of residuals: Cox-Snell, randomized quantile residuals and
martingale residuals.

# EstimationTools 4.1.0

- We implemented two functions to compute expected values.

# EstimationTools 4.0.1

- We fixed the hazard function routine implemented to compute hazard functions
for any distribution.
- We also implemented a routine to compute cumulative hazard functions for any
distribution.
- We fixed and updated the documentation.

# EstimationTools 4.0.0

- We changed the behaviour of `plot.HazardShape` function, its responsibility was 
splitted. Now, this function just plots the empirical TTT function, and we also 
created the `legend.HazardShape` to customize the legend options.
- We fixed some examples in the documentation.

# EstimationTools 3.0.0

- **EstimationTools** was build in R 4.2.1.
- From this version, `maxlogLreg` returns the censorship matrix. Check the [documentation](https://jaimemosg.github.io/EstimationTools/reference/maxlogLreg.html) 
for further information about the censorship matrix.
- We have implemented a wrapper (an API) of integration routines.

# EstimationTools 2.4.0 

- New!!! genetic algorithm (`GA::ga`) implemented in `maxlogL`.
- Custom optimization interface enabled (tests required). 

# EstimationTools 2.3.0 

- New `coef` method implemented.
- Link functions updated, precision problems solved.
- Link functions specified in summary (maxlogLreg) and print methods.
- Bug in answer class object in `DEoptim` fixed.

# EstimationTools 2.2.0

- `TTT_hazard_shape` was implemented as method for classes `EmpiricalTTT` and `formula` objects.
- `Hazard_Shape` function deprecated. We implemented a 'print' method for `TTT_hazard_shape` objects.
- `plot.HazardShape`: More flexibility in plot options enabled.
- Bug in `se.fit`(predict method) fixed.  

# EstimationTools 2.1.1

- Default value for data (NULL) in order to handle data inside another function (use with `apply` family) in TTT functions.
- New data sets and examples for them added. They were also referenced.

# EstimationTools 2.1.0

- Function for hazard shape estimation implemented. A plot method was also implemented.

# EstimationTools 2.0.2

- Empirical TTT with numerical (grouped) covariate.

# EstimationTools 2.0.1

- Bug fixed. Formula handle with survival objects without factor classification in TTTE_Analytic is fixed.

# EstimationTools 2.0.0

- **EstimationTools** was build in R 4.0.2.

- We have implemented non-centrality parameter detection. This parameter must be named
`ncp` in any custom distribution.

- We have implemented computation and plots of empirical total time on test (TTT).

- Standard error computation is now performed inside `maxlogL` and `maxlogLreg`.

- Standard errror value is now stored in the list case named 'fit'. It can be called typing `objectname$fit$StdE`, where 'objectname' is the object where the model is stored.

- The new method (function) `bootstrap_maxlogL` was created for standard error computation through bootstrap.

- 'summary' method only reports results. It does not perform any computation, bootstrap through 'summary' function has been deprecated.

- We have made a careful spelling check in our documentation.
