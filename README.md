
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/Jaimemosg/EstimationTools.svg?branch=master)](https://travis-ci.org/Jaimemosg/EstimationTools)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/Jaimemosg/EstimationTools?branch=master&svg=true)](https://ci.appveyor.com/project/Jaimemosg/EstimationTools)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/EstimationTools)](https://cran.r-project.org/package=EstimationTools) 
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

# EstimationTools <img src="man/figure/ETLogo.png" align="right" height="200" align="right"/>

<!-- badges: start -->

<!-- badges: end -->

The goal of `EstimationTools` is to provide a routine for parameter
estimation of probability density/mass functions in
`R`.

## Installation

<!-- You can install the released version of EstimationTools from [CRAN](https://CRAN.R-project.org) with: -->

You can install the last version of `EstimationTools` typing the
following command lines in `R` console:

``` r
if (!require('devtools')) install.packages('devtools')
devtools::install_github('Jaimemosg/EstimationTools', force = TRUE)
library(EstimationTools)
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
fit <- maxlogL( x = x, link = list(over = "sd", fun = "log_link") )
summary(fit)
#> ---------------------------------------------------------------
#> Optimization routine: nlminb 
#> Standard Error calculation: Hessian from optim 
#> ---------------------------------------------------------------
#>        AIC      BIC
#>   64158.35 64154.35
#> ---------------------------------------------------------------
#>      Estimate  Std. Error
#> mean  159.9093     0.0598
#> sd      5.9821     0.0423
#> -----
```
