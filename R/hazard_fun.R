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
#' @param distr a length-one character vector with the name of density/mass function
#'              of interest.
#' @param support a list with the following entries:
#'                \itemize{
#'                \item \code{interval}: a two dimensional atomic vector indicating the
#'                set of possible values of a random variable having the
#'                distribution specified in \code{y_dist}.
#'                \item \code{type}: character indicating if distribution has a
#'                \code{discrete} or a \code{continous} random variable.
#'                }
#' @param method
#'
#' @return A function with the folling input arguments:
#' \item{x}{vector of (non-negative) quantiles.}
#' \item{...}{Arguments of the probability density/mass function.}
#'
#' @examples
#' library(EstimationTools)
#'
#' #--------------------------------------------------------------------------------
#' # Example 1: Hazard function of the Weibull distribution.
#' support <- list(interval=c(0, Inf), type='continuous')
#'
#' # Hazard function in the 'maxlogL' framework
#' hweibull1 <- hazard_fun('dweibull', support)
#'
#' # Hazard function from scratch
#' hweibull2 <- function(x, shape, scale){
#'   shape/scale * (x/scale)^(shape - 1)
#' }
#'
#' # Comparison
#' hweibull1(0.2, shape = 2, scale = 1)
#' hweibull2(0.2, shape = 2, scale = 1)
#'
#'
#' #--------------------------------------------------------------------------------
#'
#' @export
hazard_fun <-  function(
    distr,
    support,
    log = FALSE
){
  type <- match.arg(support$type, c("continuous", "discrete"))

  hfun <- function(x, ...){
    pdf <- do.call(what = distr,
                   args = c(list(x = x, log = log), ...))
    sf <- do.call(what = paste0('p', substring(distr, 2)),
                  args = c(list(q = x, log.p = log, lower.tail = FALSE), ...))
    hf <- if (log){ pdf - cdf } else { pdf/sf }
    hf <- 'empty'
    return(hf)
  }

  if (type == 'discrete'){
    body(hfun)[[5]] <- quote(hf <- ifelse(hf > 1, 1, hf))
  } else {
    body(hfun)[[5]] <- NULL
  }
  return(hfun)
}

#' @title Cumulative hazard functions in \code{maxlogL} framework
#' @family maxlogL
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function takes a \code{maxlogL} hazard function and computes the
#' cumulative hazard function.
#'
#' @param distr a length-one character vector with the name of density/mass function
#'              of interest.
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
#' #----------------------------------------------------------------------------
#' # Example 1: Cumulative hazard function of the Weibull distribution.
#' support <- list(interval=c(0, Inf), type='continuous')
#'
#' # Cumuative hazard function in the 'maxlogL' framework
#' Hweibull1 <- cum_hazard_fun(
#'   distr = 'dweibull',
#'   support = support,
#'   method = "integrate_hf"
#'  )
#'
#'  Hweibull2 <- cum_hazard_fun(
#'   distr = 'dweibull',
#'   support = support,
#'   method = "log_sf"
#'  )
#'
#' # Compute cumulative hazard function from scratch
#' # Recall h(x) = shape/scale * (x/scale)^(shape - 1)
#'
#' Hweibull3 <- function(x, scale, shape){
#'   (x/scale)^shape
#' }
#'
#' # Comparison
#' Hweibull1(0.2, shape = 2, scale = 1)  # using H(t) = -log(S(t))
#' Hweibull2(0.2, shape = 2, scale = 1)  # integrating h(t)
#' Hweibull3(0.2, shape = 2, scale = 1)  # raw version
#'
#'
#' #----------------------------------------------------------------------------
#'
#' @export
cum_hazard_fun <- function(
    distr,
    support = NULL,
    method = c("log_sf", "integrate_hf")
){
  if (!is.null(support)){
    type <- match.arg(support$type, c("continuous", "discrete"))
  }

  method <- match.arg(method)

  Hfun <- cumhazfun_method[[method]](distr, support)
  return(Hfun)
}
#==============================================================================
# Computation of cumulative hazard functions ----------------------------------
#==============================================================================
cumhf_log_sf <- function(distr, support){
  Hfun <- function(x, ...){
    log_sf <- do.call(
      what = paste0('p', substring(distr, 2)),
      args = c(list(q = x, log.p = TRUE, lower.tail = FALSE), ...)
    )
    Hf <- -log_sf
    return(Hf)
  }
  return(Hfun)
}

cumhf_integral <- function(distr, support){
  if (is.null(support)) stop(
    paste0(
      "Please, provide the support of the distribution as a list with the ",
      "type ('discrete' or 'continuous') and the interval limits as a two ",
      "elements vector.")
  )
  Hfun <- function(x, ...){
    integrand <- hazard_fun(distr, support)

    if (support$type == 'continuous'){
      routine <- 'integrate'
    } else if (support$type == 'discrete'){
      routine <- 'summate'
    }

    Hf <- do.call(what = 'integration',
                  args = c(list(fun = integrand, lower = support$interval[1],
                                upper = x,
                                routine = routine, ...)))
    return(Hf)
  }
  return(Hfun)
}

cumhazfun_method <- list(
  "log_sf" = cumhf_log_sf,
  "integrate_hf" = cumhf_integral
)
