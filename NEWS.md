# EstimationTools 2.0.0

- 'EstimationTools' was build in R 4.0.2.

- We have implemented computation and plots of empirical total time on test (TTT).

- Standard error computation is now performed inside 'maxlogL' and 'maxlogLreg'.

- Standard errror value is now stored in the list case named 'fit'. It can be called typing "objectname$fit$StdE", where 'objectname' is the object where the model is sotored.

- The new method (function) 'bootstrap_maxlogL' was created for standard error computation through bootstrap.

- 'summary' method only reports results. It does not perform any computation, bootstrap through 'summary' function has been deprecated.

- We have made a careful spelling check in our documentation.
