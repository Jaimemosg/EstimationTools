
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/Jaimemosg/EstimationTools.svg?branch=master)](https://travis-ci.org/Jaimemosg/EstimationTools)

# EstimationTools <img src="man/figure/ETLogo.png" align="right" height="200" align="right"/>

<!-- badges: start -->

<!-- badges: end -->

The goal of `EstimationTools` is to provide a routine for parameter
estimation of probability density/mass functions in
`R`.

## Installation

<!-- You can install the released version of EstimationTools from [CRAN](https://CRAN.R-project.org) with: -->

You can install the last version of `EstimationTools` from
[github](https://github.com/Jaimemosg/EstimationTools) with:

``` r
if (!require('devtools')) install.packages('devtools')
devtools::install_github('Jaimemosg/EstimationTools', force=TRUE)
```

## Example

This is a basic example which shows you how to solve a common maximum
likelihood estimation problem with `EstimationTools`:

  
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

x <- rnorm( n = 10000, mean = 160, sd = 6 )
fit <- maxlogL( x = x, link = list(over = "sd", fun = "log.link") )
summary(fit)
#> ---------------------------------------------------------------
#> Optimization routine: nlminb 
#> Standard Error calculation: Hessian from optim 
#> ---------------------------------------------------------------
#>        AIC      BIC
#>   64170.47 64166.47
#> ---------------------------------------------------------------
#>      Estimate  Std. Error
#> mean  160.0174     0.0599
#> sd      5.9858     0.0423
#> -----
```
