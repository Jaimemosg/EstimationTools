#' @title Numerical integration through Gaussian Quadrature
#' @family Integration functions
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This family of functions use quadratures for solving integrals. The user can
#' create a custom integration routine, see \emph{details} for further
#' information.
#'
#' @return The value of the integral of the function specified in \code{fun}
#' argument.
#'
#' @param fun an \strong{R} function which should take a numeric argument x and
#'            possibly some parameters. The function returns a numerical vector
#'            value for the given argument \code{x}.
#' @param lower a numeric value for the lower limit of the integral.
#' @param upper a numeric value for the upper limit of the integral.
#' @param kind character specifying the weight (polynomial) function for the
#'             quadrature.
#' @param n integer with the highest order of the polynomial of the selected
#'          rule.
#' @param normalized logical. If TRUE, rules are for orthonormal polynomials,
#'                   otherwise they are for orthogonal polynomials.
#' @param ... 	additional arguments to be passed to \code{fun} and to the
#'              quadrature routine specified in argument \code{kind}.
#'
#' @details
#' \code{gauss_quad} uses the implementation of Gaussian quadratures from
#' \strong{gaussquad} package.This is a wrapper that implements \code{rules}
#' and integration routine in the same place.
#'
#' @seealso \code{\link[gaussquad]{laguerre.quadrature}},
#' \code{\link[gaussquad]{legendre.quadrature}},
#' \code{\link[gaussquad]{chebyshev.c.quadrature}},
#' \code{\link[gaussquad]{gegenbauer.quadrature}},
#' \code{\link[gaussquad]{hermite.h.quadrature}}, etc.
#'
#' @examples
#' library(EstimationTools)
#'
#' #----------------------------------------------------------------------------
#' # Example 1: Mean of X ~ N(2,1) (Gauss-Hermitie quadrature).
#' g <- function(x, mu, sigma) sqrt(2)*sigma*x + mu
#' i2 <- gauss_quad(g, lower = -Inf, upper = Inf, kind = 'hermite.h',
#'                  normalized = FALSE, mu = 2, sigma = 1)
#' i2 <- i2/sqrt(pi)
#' i2
#'
#'
#' #----------------------------------------------------------------------------
#'
#' @import gaussquad
#' @export
gauss_quad <- function(fun, lower, upper, kind = 'legendre',
                       n = 10, normalized = FALSE, ...){
  dots <- substitute(...())
  # if ( missing(kind) ) kind <- 'legendre'
  # if ( missing(normalized) ) normalized <- FALSE
  weight_fun_name <- paste0('gaussquad::', kind, '.quadrature')
  weight_fun <- eval(parse(text = weight_fun_name))
  weight_rules <- paste0(weight_fun_name, '.rules')

  rules_fun <- eval(parse(text = weight_rules))
  rules_args <- formals(rules_fun)
  index <- match(names(rules_args), names(dots), nomatch = 0)
  rules_args <- dots[index]
  if ( length(rules_args)==0 ) rules_args <- NULL
  rules <- do.call(what = rules_fun,
                   args = c(list(n = n, normalized = normalized),
                            rules_args))

  classified_args <- extract_fun_args(fun = weight_fun,
                                      exclude = c('lower', 'upper', 'rule'),
                                      ...)
  routine_input_args <- classified_args[[1]]
  add_args_routine <- classified_args[[2]]
  if (length(routine_input_args) == 0) routine_input_args <- NULL
  if (length(add_args_routine) == 0) add_args_routine <- NULL

  integral_value <- do.call(what = weight_fun,
                            args = c(list(functn = fun, lower = lower,
                                          upper = upper, rule = rules[[n]]),
                                     routine_input_args, add_args_routine))
  return(integral_value)
}
# gauss_quad <- function(fun, lower, upper, add_args, ...){
#   if ( missing(add_args) ){
#     add_args <- list(n = 10, kind = "legendre",
#                      normalized = FALSE, weighted = TRUE)
#   }
#   weight_fun <- paste0('gaussquad::', add_args$kind, '.quadrature')
#   weight_rules <- paste0(weight_fun, '.rules')
#   n <- add_args$n
#
#   add_args['n'] <- NULL
#   add_args['kind'] <- NULL
#   rules_args <- names(formals(weight_rules))
#   index <- match(add_args, rules_args, nomatch = 0)
#   rules <- do.call(what = weight_rules,
#                    args = c(list(n = n), add_args[index]))
#
#   integral <- do.call(what = weight_fun,
#                       args = c(list(functn = fun, lower = lower,
#                                     upper = upper, rule = rules[[n]]),
#                                add_args[-index], ...))
#   return(integral)
# }
