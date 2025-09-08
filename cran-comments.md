## v4.3.1 Submission 12 (current submission)
* Found the following (possibly) invalid URLs:
  URL: http://link.springer.com/10.1007/s10985-017-9394-3 (moved to https://link.springer.com/article/10.1007/s10985-017-9394-3)
    From: man/head_neck_cancer.Rd
    Status: 301
    Message: Moved Permanently

----------------------------------------------------------------
## v4.0.0: Submission 11 

* Multiple errors in the documentation
````
Result: NOTE 
  checkRd: (-1) integration.Rd:37-38: Lost braces in \itemize; meant \describe ?
  checkRd: (-1) integration.Rd:39: Lost braces in \itemize; meant \describe ?
  checkRd: (-1) integration.Rd:40: Lost braces in \itemize; meant \describe ?
  checkRd: (-1) integration.Rd:41-42: Lost braces in \itemize; meant \describe ?
  checkRd: (-1) loess.options.Rd:25-26: Lost braces in \itemize; meant \describe ?
  checkRd: (-1) loess.options.Rd:27-28: Lost braces in \itemize; meant \describe ?
  checkRd: (-1) maxlogLreg.Rd:138: Lost braces; missing escapes or markup?
     138 | $j^{th}$ for all the observations.
```

## v4.0.0: Submission 10

* Description updated.
* checking sizes of PDF files under 'inst/doc' ... WARNING
  'gs+qpdf' made some significant size reductions:
     compacted 'maxlogL.pdf' from 416Kb to 108Kb
  consider running tools::compactPDF(gs_quality = "ebook") on these files

fixed

## v4.0.0: Submission 9
* Found the following (possibly) invalid file URI:
  URI: /C:/Users/jaime/Documents/R/EstimationTools/docs/reference/maxlogLreg.html
    From: NEWS.md
    
  fixed

## v4.0.0: Submission 8

* Found the following (possibly) invalid URLs:
  URL: http://doi.wiley.com/10.1002/nav.3800260303
    From: man/TTTE_Analytical.Rd
    Status: 503
    Message: Service Unavailable
  URL: http://doi.wiley.com/10.2307/3314888
    From: man/reduction_cells.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://codecov.io/gh/Jaimemosg/EstimationTools?branch=master (moved to https://app.codecov.io/gh/Jaimemosg/EstimationTools?branch=master)
    From: README.md
    Status: 200
    Message: OK

fixed

## v1.2.1: Submission 6 successful (package updated on CRAN)

* Derivatives computation in maxlogL corrected.

## v1.2.0: Submission 5 successful (package updated on CRAN)

## v1.1.0: Submission 4 successful (package on CRAN)

## v1.0.2 : Issues in submission 3:

* All your examples are wrapped in \donttest{}: fixed

## v1.0.2 : Issues in submission 2:

* Please omit that field as the (correctly spelled) Maintainer field will
  be auto generated from Authors@R anyway: fixed
* Please omit the file and the reference to the file as the GPL-3 is part
  of R anyway: fixed
* Is there some reference about the method you can add in the Description 
  field in the form Authors (year) <doi:.....>?: fixed

## v1.0.1 : Issues in submission 1:

* This is way to vague: Thousands of R package do some parameter estimation: fixed
* The Description field should not start with the package name: fixed


## Test environments

* Local Windows 11 install, R 4.2.1
* win-builder (devel and release)
* OS X (GitHub Actions)
* Linux (GitHub Actions)

## R CMD check result

Status: OK

R CMD check results

0 errors | 0 warnings | 0 notes

----------------------------------------------------------------
