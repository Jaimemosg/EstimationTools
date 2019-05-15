
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

  
![
\\begin{aligned} 
X &\\sim N(\\mu, \\:\\sigma^2) \\\\
\\mu &= 160 \\\\
\\sigma &= 6
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%20%0AX%20%26%5Csim%20N%28%5Cmu%2C%20%5C%3A%5Csigma%5E2%29%20%5C%5C%0A%5Cmu%20%26%3D%20160%20%5C%5C%0A%5Csigma%20%26%3D%206%0A%5Cend%7Baligned%7D%0A
"
\\begin{aligned} 
X &\\sim N(\\mu, \\:\\sigma^2) \\\\
\\mu &= 160 \\\\
\\sigma &= 6
\\end{aligned}
")  

The solution for a data set generated with size
![n=10000](https://latex.codecogs.com/png.latex?n%3D10000 "n=10000") is
showed below

``` r
library(EstimationTools)

x <- rnorm(n = 10000, mean = 160, sd = 6)
fit <- maxlogL(x = x, link=list(over=c("sd"), fun=c("log.link")))
summary(fit)
#> ---------------------------------------------------------------
#> Optimization routine: nlminb 
#> Standard Error calculation: Hessian from optim 
#> ---------------------------------------------------------------
#>        AIC      BIC
#>   63963.04 63959.04
#> ---------------------------------------------------------------
#>      Estimate  Std. Error
#> mean   159.847     0.0592
#> sd       5.924     0.0419
#> -----
```
