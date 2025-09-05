#' @title Integration
#'
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This is a wrapper routine for integration in \code{maxlogL} framework. It is
#' used in integration for compute detectability density functions and in
#' computation of mean values, but it is also a general purpose integrator.
#'
#' @param fun an \strong{R} function which should take a numeric argument x and
#'            possibly some parameters. The function returns a numerical vector
#'            value for the given argument \code{x}.
#' @param lower a numeric value for the lower limit of the integral.
#' @param upper a numeric value for the upper limit of the integral.
#' @param routine a character specifying the integration routine.
#'                \code{integrate} and \code{gauss_quad} are available, but the
#'                custom routines can be defined.
#' @param ... 	additional arguments to be passed to \code{fun} and to the
#' integration routine specified in \code{routine} argument.
#'
#' @details
#' The user can create custom integration routines through implementation
#' of a wrapper function using three arguments
#'
#' \describe{
#' \item{\code{fun}: }{a function which should take a numeric argument x and possibly
#' some parameters.}
#' \item{\code{lower}: }{a numeric value for the lower limit of the integral.}
#' \item{\code{upper}: }{a numeric value for the upper limit of the integral.}
#' \item{\code{...} }{furthermore, the user must define additional arguments to be
#' passed to \code{fun} function.}
#' }
#'
#' The output must be a numeric atomic value with the result of the integral.
#'
#' @examples
#' library(EstimationTools)
#'
#'
#' #----------------------------------------------------------------------------
#' # Example 1: Mean of X ~ N(2,1) using 'integrate'.
#' mynorm <- function(x, mu, sigma) x*dnorm(x, mean = mu, sd = sigma)
#' i1 <- integration(mynorm, lower = -Inf, upper = Inf, mu = 2, sigma = 1)
#' i1
#'
#' #----------------------------------------------------------------------------
#' # Example 2: Mean of X ~ N(2,1) using 'gauss_quad' (Gauss-Hermitie
#' #            quadrature).
#' g <- function(x, mu, sigma) sqrt(2)*sigma*x + mu
#' i2 <- integration(g, lower = -Inf, upper = Inf, routine = 'gauss_quad',
#'                   kind = 'hermite.h', normalized = FALSE,
#'                   mu = 2, sigma = 1)
#' i2 <- i2/sqrt(pi)
#' i2
#'
#'
#' #----------------------------------------------------------------------------
#' # Example 3: replicating integrate
#' i3 <- integrate(dnorm, lower=-1.96, upper=1.96)
#' i4 <- integration(dnorm, lower=-1.96, upper=1.96)
#' identical(i3$value, i4)
#'
#' #----------------------------------------------------------------------------
#'
#' @seealso \code{\link[stats]{integrate}}, \code{\link{gauss_quad}}
#'
#' @export
integration <- function(fun, lower, upper, routine = 'integrate', ...){
  # dots <- substitute(...())
  # args_list <- formals(routine)
  # routine_args <- names(args_list[-1])
  # routine_args <- routine_args[!(routine_args %in% c('lower', 'upper', '...'))]
  # routine_input_args <- args_list[routine_args[routine_args %in% dots]]
  # add_args <- dots[!(dots %in% routine_args)]
  classified_args <- extract_fun_args(fun = routine,
                                      exclude = c('lower', 'upper', '...'),
                                      ...)
  routine_input_args <- classified_args[[1]]
  add_args <- classified_args[[2]]
  if (length(routine_input_args) == 0) routine_input_args <- NULL
  if (length(add_args) == 0) add_args <- NULL

  int <- do.call(what = routine, args = c(list(fun, lower = lower,
                                               upper = upper),
                                          routine_input_args, add_args))[[1]]
  return(int)
}
