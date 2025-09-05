
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/Jaimemosg/EstimationTools/workflows/R-CMD-check/badge.svg)](https://github.com/Jaimemosg/EstimationTools/actions)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/Jaimemosg/EstimationTools?branch=master&svg=true)](https://ci.appveyor.com/project/Jaimemosg/EstimationTools)<!-- [![Codecov test coverage](https://codecov.io/gh/Jaimemosg/EstimationTools/branch/master/graph/badge.svg)](https://codecov.io/gh/Jaimemosg/EstimationTools?branch=master) -->
[![Codecov test
coverage](https://codecov.io/gh/Jaimemosg/EstimationTools/branch/master/graph/badge.svg)](https://app.codecov.io/gh/Jaimemosg/EstimationTools?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-ago/EstimationTools)](https://cran.r-project.org/package=EstimationTools)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/EstimationTools)](https://cran.r-project.org/package=EstimationTools)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

<!-- [![Travis build status](https://travis-ci.org/Jaimemosg/EstimationTools.svg?branch=master)](https://travis-ci.org/Jaimemosg/EstimationTools) -->

<!-- [![Dependencies](https://tinyverse.netlify.com/badge/EstimationTools)](https://cran.r-project.org/package=EstimationTools) -->

<!-- badges: end -->

# EstimationTools <img src="man/figures/ETLogo.png" align="right" height="200" style="float:right; height:200px;"/>

<!-- <img src="man/figures/ETLogo.png" align="right" height="200"/> -->

The goal of `EstimationTools` is to provide tools and routines for
maximum likelihood estimation of probability density/mass functions in
`R`.

<!-- _Edit (17/03/2021)_ -- We have performed a TTT plot implementation. You can find out in our [changelog](https://jaimemosg.github.io/EstimationTools/news/index.html). -->

<!-- _Edit (27/07/2020)_ -- We have implemented new tools and we have performed some deep modifications in our `summary` method for `maxlogL` objects. You can find out in our [changelog](https://jaimemosg.github.io/EstimationTools/news/index.html). -->

## Installation

You can install the latest version (4.3.1) of `EstimationTools` typing
the following command lines in `R` console:

``` r
if (!require('devtools')) install.packages('devtools')
devtools::install_github('Jaimemosg/EstimationTools', force = TRUE)
library(EstimationTools)
```

<!-- 4.3.1 -->

Or you can install the released version (4.0.0) from
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
