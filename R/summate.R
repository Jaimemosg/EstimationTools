#' @title Summation of One-Dimensional Functions
#'
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Discrete summation of functions of one variable over a finite or semi-infinite
#' interval.
#'
#' @param fun an \strong{R} function which should take a numeric argument x and
#'            possibly some parameters. The function returns a numerical vector
#'            value for the given argument \code{x}.
#' @param lower a numeric value for the lower limit of the integral.
#' @param upper a numeric value for the upper limit of the integral.
#' @param tol a numeric value indicating the accuracy of the result (useful in
#'        infinite summations).
#' @param ... 	additional arguments to be passed to \code{fun}.
#'
#' @details
#' Arguments after \code{...} must be matched exactly. If both limits are infinite,
#' the function fails. For semi-infinite intervals, the summation must be convergent.
#' This is accomplished in manny probability mass functions.
#'
#' @examples
#' library(EstimationTools)
#'
#'
#' #----------------------------------------------------------------------------
#' # Example 1: Poisson expected value computation, X ~ Poisson(lambda = 15)
#' Poisson_integrand <- function(x, lambda) {
#'   x * lambda^x * exp(-lambda)/factorial(x)
#' }
#'
#' summate(fun = Poisson_integrand, lower = 0, upper = Inf, lambda = 15)
#'
#'
#' #----------------------------------------------------------------------------
#'
#' @export
summate <- function(fun, lower, upper, tol = 1e-10, ...){
  dots <- substitute(...())
  if ( is.null(dots) ){
    fun_args <- formals(fun)
    if ( length(fun_args) > 1 )
      stop("Parameters for 'fun' must be defined.")
  }
  init <- 0
  ans <- 0

  x <- lower
  if (is.infinite(upper)){
    condition <- expression(TRUE)
  } else {
    condition <- expression(x <= upper)
  }

  while (eval(condition)){
    EXi <- try(do.call(what = fun, args = c(list(x, ...))), silent = TRUE)
    if ( is.nan(EXi) | inherits(EXi, "try-error") ){
      break
    } else{
      if (abs(EXi - init) < tol & x > lower) break
      ans <- ans + EXi
      x <- x + 1
      init <- ans
    }
  }
  return(ans)
}
