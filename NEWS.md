# EstimationTools 2.4.0 

- New!!! genetic algorithm (GA::ga) implemented in `maxlogL`.
- Custom optimization interface enabled. 

# EstimationTools 2.3.0 

- New 'coef' method implemented.
- Link functions updated, precision problems solved.
- Link functions specified in summary (maxlogLreg) and print methods.
- Bug in answer class object in `DEoptim` fixed.

# EstimationTools 2.2.0

- `TTT_hazard_shape` was implemented as method for classes `EmpiricalTTT` and `formula` objects.
- `Hazard_Shape` function deprecated. We implemented a 'print' method for `TTT_hazard_shape` objects.
- `plot.HazardShape`: More flexibility in plot options enabled.
- Bug in `se.fit`(predict method) fixed.  

# EstimationTools 2.1.1

- Default value for data (NULL) in order to handle data inside another function (use with 'apply' family) in TTT functions.
- New data sets and examples for them added. They were also referenced.

# EstimationTools 2.1.0

- Function for hazard shape estimation implemented. A plot method was also implemented.

# EstimationTools 2.0.2

- Empirical TTT with numerical (grouped) covariate.

# EstimationTools 2.0.1

- Bug fixed. Formula handle with survival objects without factor classification in TTTE_Analytic is fixed.

# EstimationTools 2.0.0

- 'EstimationTools' was build in R 4.0.2.

- We have implemented noncentrality parameter detection. This parameter must be named
`ncp` in the distribution.

- We have implemented computation and plots of empirical total time on test (TTT).

- Standard error computation is now performed inside `maxlogL` and `maxlogLreg`.

- Standard errror value is now stored in the list case named 'fit'. It can be called typing `objectname$fit$StdE`, where 'objectname' is the object where the model is stored.

- The new method (function) 'bootstrap_maxlogL' was created for standard error computation through bootstrap.

- 'summary' method only reports results. It does not perform any computation, bootstrap through 'summary' function has been deprecated.

- We have made a careful spelling check in our documentation.
