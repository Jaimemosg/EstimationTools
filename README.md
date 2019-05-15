
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EstimationTools

<!-- badges: start -->

<!-- badges: end -->

The goal of `EstimationTools` is to provide routines for parameter
estimation of probability density/mass
functions.

## Installation

<!-- You can install the released version of EstimationTools from [CRAN](https://CRAN.R-project.org) with: -->

You can install the released version of `EstimationTools` from
[github](https://github.com/Jaimemosg/EstimationTools) with:

``` r
if (!require('devtools')) install.packages('devtools')
devtools::install_github('Jaimemosg/EstimationTools', force=TRUE)
```

## Example

This is a basic example which shows you how to solve a common problem:

\begin{align*} 
X &\sim N(\mu, \sigma^2)
\mu &= 160
\sigma &= 6
\end{align*}

The solution for a data set generated with size \(n=10000\) is showed
below

``` r
library(EstimationTools)

x <- rnorm(n=10000, mean=160, sd=6)
fit <- maxlogL(x=x, link=list(over=c("sd"), fun=c("log.link")))
summary(fit)
#> ---------------------------------------------------------------
#> Optimization routine: nlminb 
#> Standard Error calculation: Hessian from optim 
#> ---------------------------------------------------------------
#>        AIC      BIC
#>   64281.61 64277.61
#> ---------------------------------------------------------------
#>      Estimate  Std. Error
#> mean  160.0125     0.0602
#> sd      6.0191     0.0426
#> -----
```
