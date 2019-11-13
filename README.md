
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/Jaimemosg/EstimationTools.svg?branch=master)](https://travis-ci.org/Jaimemosg/EstimationTools)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/Jaimemosg/EstimationTools?branch=master&svg=true)](https://ci.appveyor.com/project/Jaimemosg/EstimationTools)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version-ago/EstimationTools)](https://cran.r-project.org/package=EstimationTools)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/EstimationTools)](https://cran.r-project.org/package=EstimationTools)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!-- [![Dependencies](https://tinyverse.netlify.com/badge/EstimationTools)](https://cran.r-project.org/package=EstimationTools) -->

# EstimationTools <img src="man/figure/ETLogo.png" align="right" height="200" align="right"/>

<!-- badges: start -->

<!-- badges: end -->

The goal of `EstimationTools` is to provide a routine for parameter
estimation of probability density/mass functions in `R`.

## Installation

You can install the lastest version of `EstimationTools` typing the
following command lines in `R` console:

``` r
if (!require('devtools')) install.packages('devtools')
devtools::install_github('Jaimemosg/EstimationTools', force = TRUE)
library(EstimationTools)
```

Or you can install the released version from
[CRAN](https://cran.r-project.org/package=EstimationTools) if you
prefer. You can also type the following command lines in `R` console:

``` r
install.packages("EstimationTools")
```

You can visit the [package
website](https://Jaimemosg.github.io/EstimationTools/) to explore the
vignettes (articles) and functions reference.

# Examples

These are basic examples which shows you how to solve a common maximum
likelihood estimation problem with `EstimationTools`:

## Estimation in regression models

  
![
\\begin{aligned} 
X &\\sim N(\\mu, \\:\\sigma^2) \\\\
\\mu &= -2 + 3x \\quad (\\verb|mean|) \\\\
\\log(\\sigma) &= 1 + 0.3x \\quad (\\verb|sd|)
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%20%0AX%20%26%5Csim%20N%28%5Cmu%2C%20%5C%3A%5Csigma%5E2%29%20%5C%5C%0A%5Cmu%20%26%3D%20-2%20%2B%203x%20%5Cquad%20%28%5Cverb%7Cmean%7C%29%20%5C%5C%0A%5Clog%28%5Csigma%29%20%26%3D%201%20%2B%200.3x%20%5Cquad%20%28%5Cverb%7Csd%7C%29%0A%5Cend%7Baligned%7D%0A
"
\\begin{aligned} 
X &\\sim N(\\mu, \\:\\sigma^2) \\\\
\\mu &= -2 + 3x \\quad (\\verb|mean|) \\\\
\\log(\\sigma) &= 1 + 0.3x \\quad (\\verb|sd|)
\\end{aligned}
")  

The solution for a data set generated with size
![n=10000](https://latex.codecogs.com/png.latex?n%3D10000 "n=10000") is
printed below

``` r
library(EstimationTools)

n <- 10000
x1 <- runif(n = n, -5, 6)
y <- rnorm(n = n, mean = -2 + 3 * x1, sd = exp(1 + 0.3* x1))

formulas <- list(sd.fo = ~ x1, mean.fo = ~ x1)

norm_mod <- maxlogLreg(formulas, y_dist = y ~ dnorm,
                       link = list(over = "sd", fun = "log_link"))
summary(norm_mod)
#> _______________________________________________________________
#> Optimization routine: nlminb 
#> Standard Error calculation: Hessian from optim 
#> _______________________________________________________________
#>        AIC      BIC
#>   51621.09 51649.93
#> _______________________________________________________________
#> Fixed effects for g(mean) 
#> ---------------------------------------------------------------
#>             Estimate Std. Error Z value  Pr(>|z|)    
#> (Intercept)  -1.9701     0.0357 -55.184 < 2.2e-16 ***
#> x1            3.0054     0.0096 313.061 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> _______________________________________________________________
#> Fixed effects for g(sd) 
#> ---------------------------------------------------------------
#>             Estimate Std. Error Z value  Pr(>|z|)    
#> (Intercept)  0.99565    0.00720  138.28 < 2.2e-16 ***
#> x1           0.30629    0.00230  133.17 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> _______________________________________________________________
#> Note: p-values valid under asymptotic normality of estimators 
#> ---
```

## Estimation in distributions

  
![
\\begin{aligned} 
X &\\sim N(\\mu, \\:\\sigma^2) \\\\
\\mu &= 160 \\quad (\\verb|mean|) \\\\
\\sigma &= 6 \\quad (\\verb|sd|)
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%20%0AX%20%26%5Csim%20N%28%5Cmu%2C%20%5C%3A%5Csigma%5E2%29%20%5C%5C%0A%5Cmu%20%26%3D%20160%20%5Cquad%20%28%5Cverb%7Cmean%7C%29%20%5C%5C%0A%5Csigma%20%26%3D%206%20%5Cquad%20%28%5Cverb%7Csd%7C%29%0A%5Cend%7Baligned%7D%0A
"
\\begin{aligned} 
X &\\sim N(\\mu, \\:\\sigma^2) \\\\
\\mu &= 160 \\quad (\\verb|mean|) \\\\
\\sigma &= 6 \\quad (\\verb|sd|)
\\end{aligned}
")  

The solution for a data set generated with size
![n=10000](https://latex.codecogs.com/png.latex?n%3D10000 "n=10000") is
showed below

``` r
x <- rnorm( n = 10000, mean = 160, sd = 6 )
fit <- maxlogL( x = x, dist = "dnorm", link = list(over = "sd", fun = "log_link") )
summary(fit)
#> _______________________________________________________________
#> Optimization routine: nlminb 
#> Standard Error calculation: Hessian from optim 
#> _______________________________________________________________
#>       AIC      BIC
#>   64145.4 64159.82
#> _______________________________________________________________
#> _______________________________________________________________
#>      Estimate  Std. Error
#> mean  159.9893     0.0598
#> sd      5.9783     0.0423
#> _______________________________________________________________
```
