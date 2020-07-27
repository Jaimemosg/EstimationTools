
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
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

<!-- The data is from [NIST](https://www.itl.nist.gov/div898/handbook/apr/section2/apr221.htm#Example). They generated  20 random Weibull failure times with a parameter `shape=1.5` and `scale=500`. The test time is 500 hours, 10 of these failure times are right censored. The observed times are, to the nearest hour: 54, 187, 216, 240, 244, 335, 361, 373, 375, and 386. -->

We generate data from an hypothetic failure test of approximately 641
hours with 40 experimental units, 20 from group 1 and 20 from group 2.
Lets assume a censorship rate of 10%, and regard that the data is right
censored. Times from 6 of the 20 experimental units are shown just
bellow:

``` r
library(RCurl)
library(foreign)

url_data <- "https://raw.githubusercontent.com/Jaimemosg/EstimationTools/master/extra/sim_wei.csv"
wei_data <- getURL(url_data)
data <- read.csv(textConnection(wei_data), header = TRUE)
data$group <- as.factor(data$group)
head(data)
#>   label     Time status group
#> 1     2 172.7296      1     2
#> 2     5 198.5872      1     1
#> 3    13 284.9617      1     1
#> 4    18 289.8941      1     1
#> 5    20 293.4005      1     1
#> 6    15 303.5566      1     1
```

The model is as follows:

  
![&#10;f(t|\\alpha, k) = \\frac{\\alpha}{k}
\\left(\\frac{t}{k}\\right)^{\\alpha-1}
\\exp\\left\[-\\left(\\frac{t}{k}\\right)^{\\alpha}\\right\]&#10;](https://latex.codecogs.com/png.latex?%0Af%28t%7C%5Calpha%2C%20k%29%20%3D%20%5Cfrac%7B%5Calpha%7D%7Bk%7D%20%5Cleft%28%5Cfrac%7Bt%7D%7Bk%7D%5Cright%29%5E%7B%5Calpha-1%7D%20%5Cexp%5Cleft%5B-%5Cleft%28%5Cfrac%7Bt%7D%7Bk%7D%5Cright%29%5E%7B%5Calpha%7D%5Cright%5D%0A
"
f(t|\\alpha, k) = \\frac{\\alpha}{k} \\left(\\frac{t}{k}\\right)^{\\alpha-1} \\exp\\left[-\\left(\\frac{t}{k}\\right)^{\\alpha}\\right]
")  

  
![&#10;\\begin{aligned}&#10;T &\\stackrel{\\text{iid.}}{\\sim}
WEI(\\alpha,\\: k), \\\\&#10;\\log(\\alpha) &= 1.2 + 0.1 \\times group
\\quad (\\verb|shape|),\\\\&#10;k &= 500 \\quad
(\\verb|scale|).&#10;\\end{aligned}&#10;](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%0AT%20%26%5Cstackrel%7B%5Ctext%7Biid.%7D%7D%7B%5Csim%7D%20WEI%28%5Calpha%2C%5C%3A%20k%29%2C%20%5C%5C%0A%5Clog%28%5Calpha%29%20%26%3D%201.2%20%2B%200.1%20%5Ctimes%20group%20%5Cquad%20%20%28%5Cverb%7Cshape%7C%29%2C%5C%5C%0Ak%20%26%3D%20500%20%5Cquad%20%28%5Cverb%7Cscale%7C%29.%0A%5Cend%7Baligned%7D%0A
"
\\begin{aligned}
T &\\stackrel{\\text{iid.}}{\\sim} WEI(\\alpha,\\: k), \\\\
\\log(\\alpha) &= 1.2 + 0.1 \\times group \\quad  (\\verb|shape|),\\\\
k &= 500 \\quad (\\verb|scale|).
\\end{aligned}
")  

The implementation and its solution is printed below:

``` r
library(EstimationTools)

# Formulas with linear predictors
formulas <- list(scale.fo = ~ 1, shape.fo = ~ group)

# The model
fit_wei <- maxlogLreg(formulas, data = data,
                      y_dist = Surv(Time, status) ~ dweibull,
                      link = list(over = c("shape", "scale"),
                                  fun = rep("log_link", 2)))
summary(fit_wei)
#> _______________________________________________________________
#> Optimization routine: nlminb 
#> Standard Error calculation: Hessian from optim 
#> _______________________________________________________________
#>        AIC      BIC
#>   472.4435 477.5101
#> _______________________________________________________________
#> Fixed effects for g(shape) 
#> ---------------------------------------------------------------
#>             Estimate Std. Error Z value  Pr(>|z|)    
#> (Intercept)  1.16587    0.19956  5.8422 5.152e-09 ***
#> group2       0.30861    0.28988  1.0646    0.2871    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> _______________________________________________________________
#> Fixed effects for g(scale) 
#> ---------------------------------------------------------------
#>             Estimate Std. Error Z value  Pr(>|z|)    
#> (Intercept) 6.255290   0.046286  135.14 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> _______________________________________________________________
#> Note: p-values valid under asymptotic normality of estimators 
#> ---
```

## Estimation in distributions

  
![&#10;\\begin{aligned} &#10;X &\\sim N(\\mu, \\:\\sigma^2),
\\\\&#10;\\mu &= 160 \\quad (\\verb|mean|), \\\\&#10;\\sigma &= 6 \\quad
(\\verb|sd|).&#10;\\end{aligned}&#10;](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%20%0AX%20%26%5Csim%20N%28%5Cmu%2C%20%5C%3A%5Csigma%5E2%29%2C%20%5C%5C%0A%5Cmu%20%26%3D%20160%20%5Cquad%20%28%5Cverb%7Cmean%7C%29%2C%20%5C%5C%0A%5Csigma%20%26%3D%206%20%5Cquad%20%28%5Cverb%7Csd%7C%29.%0A%5Cend%7Baligned%7D%0A
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
#>   64249.35 64263.78
#> _______________________________________________________________
#>      Estimate  Std. Error Z value Pr(>|z|)    
#> mean 159.98903    0.06009  2662.3   <2e-16 ***
#> sd     6.00943    0.04249   141.4   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> _______________________________________________________________
```
