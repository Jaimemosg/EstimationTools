#' @title Hazard functions in \code{maxlogL} framework
#' @family maxlogL
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function takes the name of a probability density/mass function as an
#' argument and creates a hazard function.
#'
#' @param dist a length-one character vector with the name of density/mass function
#'             of interest.
#' @param support a list with the following entries:
#'                \itemize{
#'                \item \code{interval}: a two dimensional atomic vector indicating the
#'                set of possible values of a random variable having the
#'                distribution specified in \code{y_dist}.
#'                \item \code{type}: character indicating if distribution has a
#'                \code{discrete} or a \code{continous} random variable.
#'                }
#'
#' @return A function with the folling input arguments:
#' \item{x}{vector of (non-negative) quantiles.}
#' \item{...}{Arguments of the probability density/mass function.}
#'
#' @examples
#' library(EstimationTools)
#'
#' #--------------------------------------------------------------------------------
#' # Example 1: Hazard function of Weibull distribution.
#' hweibull1 <- hazard_fun('dweibull', list(interval=c(0, Inf), type='continuous'))
#' hweibull2 <- function(x){
#'   ans <- dweibull(x, shape = 2, scale = 1)/
#'     pweibull(x, shape = 2, scale = 1, lower.tail = FALSE)
#'   ans
#' }
#' hweibull1(0.2, shape = 2, scale = 1)
#' hweibull2(0.2)
#'
#'
#' #--------------------------------------------------------------------------------
#'
#' @export
hazard_fun <-  function(dist, support){
  type <- match.arg(support$type, c('continuous', 'discrete'))
  hfun <- function(x, ...){
    pdf <- do.call(what = dist,
                   args = c(list(x = x, log = TRUE), ...))
    cdf <- do.call(what = paste0('p', substring(dist, 2)),
                   args = c(list(q = x, log.p = TRUE, lower.tail = FALSE), ...))
    hf <- pdf - cdf
    hf <- exp(hf)
    hf <- 'empty'
    return(hf)
  }
  if (type == 'discrete'){
    body(hfun)[[6]] <- quote(hf <- ifelse(hf > 1, 1, hf))
  } else {
    body(hfun)[[6]] <- NULL
  }
  return(hfun)
}
