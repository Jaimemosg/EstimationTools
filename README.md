
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

*Edit (27/07/2020)* – We have implemented new tools and we have
performed some deep modifications in our `summary` method for `maxlogL`
objects. You can find out in our
[changelog](https://jaimemosg.github.io/EstimationTools/news/index.html).

## Installation

You can install the latest version (2.0.1) of `EstimationTools` typing
the following command lines in `R` console:

``` r
if (!require('devtools')) install.packages('devtools')
devtools::install_github('Jaimemosg/EstimationTools', force = TRUE)
library(EstimationTools)
```

Or you can install the released version (2.0.0) from
[CRAN](https://cran.r-project.org/package=EstimationTools) if you
prefer. You can also type the following command lines in `R` console:

``` r
install.packages("EstimationTools")
```

You can visit the [package
website](https://Jaimemosg.github.io/EstimationTools/) to explore the
functions reference and our vignettes (articles).

## Examples

You can visit our estimation
[vignette](https://jaimemosg.github.io/EstimationTools/articles/Examples.html)
to see the examples.
