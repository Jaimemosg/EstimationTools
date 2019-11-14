
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
likelihood estimation problem with
`EstimationTools`:

## Estimation in regression models

<!-- The data is from [NIST](https://www.itl.nist.gov/div898/handbook/apr/section2/apr221.htm#Example). They generated  20 random Weibull failure times with a parameter `shape=1.5` and `scale=500`. The test time is 500 hours, 10 of these failure times are right censored. The observed times are, to the nearest hour: 54, 187, 216, 240, 244, 335, 361, 373, 375, and 386. -->

We generate data from an hypothetic failure test of 621.94 hours with 30
experimental units, 15 from group 1 and 15 from group 2. Lets assume a
censorship rate of 0.1, and regard that the data is right censored.
Times from 6 experimental units are shown just bellow:

    #>       t_sim status group
    #> 1  383.9410      1     1
    #> 2  194.8475      1     1
    #> 3  285.6984      1     1
    #> 28 537.0781      1     2
    #> 29 621.9377      0     2
    #> 30 445.0450      1     2

The model is as follows:

  
![
f(t|\\alpha, k) = \\frac{\\alpha}{k}
\\left(\\frac{t}{k}\\right)^{\\alpha-1}
\\exp\\left\[-\\left(\\frac{t}{k}\\right)^{\\alpha}\\right\]
](https://latex.codecogs.com/png.latex?%0Af%28t%7C%5Calpha%2C%20k%29%20%3D%20%5Cfrac%7B%5Calpha%7D%7Bk%7D%20%5Cleft%28%5Cfrac%7Bt%7D%7Bk%7D%5Cright%29%5E%7B%5Calpha-1%7D%20%5Cexp%5Cleft%5B-%5Cleft%28%5Cfrac%7Bt%7D%7Bk%7D%5Cright%29%5E%7B%5Calpha%7D%5Cright%5D%0A
"
f(t|\\alpha, k) = \\frac{\\alpha}{k} \\left(\\frac{t}{k}\\right)^{\\alpha-1} \\exp\\left[-\\left(\\frac{t}{k}\\right)^{\\alpha}\\right]
")  

  
![
\\begin{aligned}
T &\\stackrel{\\text{iid.}}{\\sim} WEI(\\alpha,\\: k), \\\\
\\alpha &= 1.5 + 0.3 \\times group \\quad (\\verb|shape|),\\\\
k &= 500 \\quad (\\verb|scale|).
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%0AT%20%26%5Cstackrel%7B%5Ctext%7Biid.%7D%7D%7B%5Csim%7D%20WEI%28%5Calpha%2C%5C%3A%20k%29%2C%20%5C%5C%0A%5Calpha%20%26%3D%201.5%20%2B%200.3%20%5Ctimes%20group%20%5Cquad%20%20%28%5Cverb%7Cshape%7C%29%2C%5C%5C%0Ak%20%26%3D%20500%20%5Cquad%20%28%5Cverb%7Cscale%7C%29.%0A%5Cend%7Baligned%7D%0A
"
\\begin{aligned}
T &\\stackrel{\\text{iid.}}{\\sim} WEI(\\alpha,\\: k), \\\\
\\alpha &= 1.5 + 0.3 \\times group \\quad  (\\verb|shape|),\\\\
k &= 500 \\quad (\\verb|scale|).
\\end{aligned}
")  

The implementation and its solution is printed below:

``` r
library(EstimationTools)

# Formulas with linear predictors
formulas <- list(scale.fo = ~ 1, shape.fo = ~ group)

# The model
fit_wei <- maxlogLreg(formulas, y_dist = Surv(t_sim, status) ~ dweibull,
                      link = list(over = c("shape", "scale"),
                                  fun = rep("log_link", 2)))
summary(fit_wei)
#> _______________________________________________________________
#> Optimization routine: nlminb 
#> Standard Error calculation: Hessian from optim 
#> _______________________________________________________________
#>        AIC      BIC
#>   356.0582 360.2618
#> _______________________________________________________________
#> Fixed effects for g(shape) 
#> ---------------------------------------------------------------
#>             Estimate Std. Error Z value  Pr(>|z|)    
#> (Intercept)  1.28937    0.23180  5.5624 2.661e-08 ***
#> group2       0.12938    0.33790  0.3829    0.7018    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> _______________________________________________________________
#> Fixed effects for g(scale) 
#> ---------------------------------------------------------------
#>             Estimate Std. Error Z value  Pr(>|z|)    
#> (Intercept)   6.2389     0.0521  119.75 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> _______________________________________________________________
#> Note: p-values valid under asymptotic normality of estimators 
#> ---
```

## Estimation in distributions

  
![
\\begin{aligned} 
X &\\sim N(\\mu, \\:\\sigma^2), \\\\
\\mu &= 160 \\quad (\\verb|mean|), \\\\
\\sigma &= 6 \\quad (\\verb|sd|).
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%20%0AX%20%26%5Csim%20N%28%5Cmu%2C%20%5C%3A%5Csigma%5E2%29%2C%20%5C%5C%0A%5Cmu%20%26%3D%20160%20%5Cquad%20%28%5Cverb%7Cmean%7C%29%2C%20%5C%5C%0A%5Csigma%20%26%3D%206%20%5Cquad%20%28%5Cverb%7Csd%7C%29.%0A%5Cend%7Baligned%7D%0A
"
\\begin{aligned} 
X &\\sim N(\\mu, \\:\\sigma^2), \\\\
\\mu &= 160 \\quad (\\verb|mean|), \\\\
\\sigma &= 6 \\quad (\\verb|sd|).
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
#>        AIC      BIC
#>   64334.94 64349.36
#> _______________________________________________________________
#> _______________________________________________________________
#>      Estimate  Std. Error
#> mean  159.9359     0.0604
#> sd      6.0352     0.0427
#> _______________________________________________________________
```
