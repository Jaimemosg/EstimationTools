# EstimationTools 2.1.1

- Default value for data (NULL) in order to handle data inside another function (use with 'apply' family).

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

- Standard error computation is now performed inside 'maxlogL' and 'maxlogLreg'.

- Standard errror value is now stored in the list case named 'fit'. It can be called typing "objectname$fit$StdE", where 'objectname' is the object where the model is stored.

- The new method (function) 'bootstrap_maxlogL' was created for standard error computation through bootstrap.

- 'summary' method only reports results. It does not perform any computation, bootstrap through 'summary' function has been deprecated.

- We have made a careful spelling check in our documentation.
