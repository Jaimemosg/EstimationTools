#' @title Cumuative hazard function of a \code{maxlogLreg} model.
#' @family maxlogL
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function takes a \code{maxlogL} model and computes the cumulative
#' hazard function (CHF) using the estimated parameters.
#'
#'
#' @param object an object of \code{\link{maxlogL}} class obtained by fitting a
#'               model with \code{\link{maxlogLreg}}.
#' @param ... further arguments for \code{\link{cum_hazard_fun}}..
#'
#' @return the expected value of the fitted model corresponding to the
#' distribution specified in the \code{y_dist} argument of
#' \code{\link{maxlogLreg}}.
#'
#' @details
#' The CHF is computed by default using the following expression
#'
#' \deqn{H(x) = -\log \left( S(x|\hat{\theta})) \right),}
#'
#' where \eqn{S(x|\hat{\theta})} is the survival function using the
#' estimated parameters. This method relies on the cdf, i.e, the \code{pXXX}
#' function stored in \proglang{R} environment, where \code{xxx} is
#' the name of the distribution.
#'
#' Notice that CHF can be computed by integration
#'
#' \deqn{H(x) = \int_0^t h(s)ds}
#'
#' Just set up a \code{support} and set \code{method = "integration"}.
#'
#' @examples
#' library(EstimationTools)
#'
#' #----------------------------------------------------------------------------
#' # Example 1: cumulative hazard function of a estimated model.
#' n <- 100
#' x <- runif(n = n, -5, 6)
#' y <- rnorm(n = n, mean = -2 + 3 * x, sd = 0.3)
#' norm_data <- data.frame(y = y, x = x)
#'
#' formulas <- list(sd.fo = ~ 1, mean.fo = ~ x)
#' support <- list(interval = c(-Inf, Inf), type = "continuous")
#'
#' norm_mod_maxlogL <- maxlogLreg(
#'   formulas, y_dist = y ~ dnorm,
#'   support = support,
#'   data = norm_data,
#'   link = list(over = "sd", fun = "log_link")
#' )
#'
#' # Expected value
#' H <- cum_hazard.maxlogL(object = norm_mod_maxlogL)
#'
#'
#' #----------------------------------------------------------------------------
#'
#' @export
#==============================================================================
# Computation of cumulative for a fitted model --------------------------------
#==============================================================================
cum_hazard.maxlogL <- function(object, ...){
  distr <- object$inputs$distr

  Hfun <- cum_hazard_fun(
    distr = distr,
    method = "log_sf",
    ...
  )

  inputs_matrix <- create_inputs(
    object, add_response = TRUE, as_matrix = TRUE
  )

  Hf_i <- function(x){
    args <- sapply(X = x, FUN = function(x) x, simplify = FALSE)
    cum_haz <- do.call(
      what = Hfun,
      args = args
    )
    return(as.numeric(cum_haz))
  }

  result <- apply(
    inputs_matrix,
    MARGIN = 1,
    FUN = Hf_i
  )
  return(result)
  # integrand <- hazard_fun(distr, support)
  #
  # if ( missing(routine) ){
  #   if (support$type == 'continuous'){
  #     routine <- 'integrate'
  #   } else if (support$type == 'discrete'){
  #     routine <- 'summate'
  #   }
  # }
  # haz_computation <- function(x)
  #   do.call(what = 'integration',
  #           args = c(list(fun = integrand, lower = support$interval[1],
  #                         upper = support$interval[2],
  #                         routine = routine, ...), x))
  # result <- apply(parameters, MARGIN = 1, haz_computation)
}

#' @title Hazard functions for any distribution
#' @family distributions utilities
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
#' @param log logical; if TRUE, the natural logarithm of the hazard values are
#'            returned.
#'
#' @return A function with the folling input arguments:
#' \item{x}{vector of (non-negative) quantiles.}
#' \item{...}{Arguments of the probability density/mass function.}
#'
#' @examples
#' library(EstimationTools)
#'
#' #----------------------------------------------------------------------------
#' # Example 1: Hazard function of the Weibull distribution.
#'
#' # Hazard function in the 'maxlogL' framework
#' hweibull1 <- hazard_fun('dweibull')
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
#' #----------------------------------------------------------------------------
#'
#' @export
hazard_fun <-  function(
    distr,
    # support,
    log = FALSE
){
  hfun <- function(x, ...){
    pdf <- do.call(what = distr,
                   args = c(list(x = x, log = log), ...))
    sf <- do.call(what = paste0('p', substring(distr, 2)),
                  args = c(list(q = x, log.p = log, lower.tail = FALSE), ...))
    hf <- if (log){ pdf - sf } else { pdf/sf }
    # hf <- 'empty'
    return(hf)
  }

  # type <- match.arg(support$type, c("continuous", "discrete"))
  #
  # if (type == 'discrete'){
  #   body(hfun)[[5]] <- quote(hf <- ifelse(hf > 1, 1, hf))
  # } else {
  #   body(hfun)[[5]] <- NULL
  # }
  return(hfun)
}

#' @title Cumulative hazard functions for any distribution
#' @family distributions utilities
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
#'                \item \code{interval}: a two dimensional atomic vector
#'                indicating the set of possible values of a random variable
#'                having the distribution specified in \code{y_dist}.
#'                \item \code{type}: character indicating if distribution has a
#'                \code{discrete} or a \code{continous} random variable.
#'                }
#' @param method a character or function; if \code{"log_sf"}, the cumulative
#'               hazard function (CHF) is computed using the expression
#'               \eqn{H(t) = -\log (S(t))}; if \code{"integrate_hf"}, the CHF is
#'               computed with the integral of the hazard function.
#' @param routine a character specifying the integration routine.
#'                \code{integrate} and \code{gauss_quad} are available for
#'                continuous distributions, and \code{summate} for discrete ones.
#'                Custom routines can be defined but they must be compatible
#'                with the \code{\link{integration}} API.
#'
#' @return A function with the following input arguments:
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
#'   method = "integration"
#'  )
#'
#'  Hweibull2 <- cum_hazard_fun(
#'   distr = 'dweibull',
#'   method = "log_sf"
#'  )
#'
#' # Compute cumulative hazard function from scratch
#' # Recall h(x) = shape/scale * (x/scale)^(shape - 1), then
#' # H(x) = (x/scale)^shape
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
    method = c("log_sf", "integration"),
    routine = NULL
){
  if (!is.null(support)){
    type <- match.arg(support$type, c("continuous", "discrete"))
  }

  method <- match.arg(method)

  if (method == "log_sf"){
    Hfun <- cumhazfun_method[[method]](distr)
  }

  if (method == "integration"){
    routine <- set_custom_integration_routine(support, routine)
    Hfun <- cumhazfun_method[[method]](distr, support, routine)
  }
  return(Hfun)
}
#==============================================================================
# Computation of cumulative hazard functions ----------------------------------
#==============================================================================
cumhf_log_sf <- function(distr){
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

cumhf_integral <- function(distr, support, routine){
  if (is.null(support)) stop(
    paste0(
      "Please, provide the support of the distribution as a list with the ",
      "type ('discrete' or 'continuous') and the interval limits as a two ",
      "elements vector.")
  )
  Hfun <- function(x, ...){
    integrand <- hazard_fun(distr)

    Hf <- do.call(what = "integration",
                  args = c(list(fun = integrand,
                                lower = support$interval[1],
                                upper = x,
                                routine = routine, ...)))
    return(Hf)
  }
  return(Hfun)
}

cumhazfun_method <- list(
  "log_sf" = cumhf_log_sf,
  "integration" = cumhf_integral
)
