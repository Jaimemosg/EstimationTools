#' @title Integration routines
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
#' @param add_args a list with further arguments for integration routine.
#' @param ... 	additional arguments to be passed to \code{fun}.
#'
#' @details
#' \code{integrate_stats} uses \code{\link[stats]{integrate}} function from
#' \strong{stats} package, whereas \code{gauss_quad} uses the implementation of
#' gaussian quadratures from \strong{gaussquad} package.
#'
#' The user can create custom optimization routines through implementation
#' of a wrapper function using the four arguments showed in emph{Usage} section.
#'
#' @seealso \code{\link[gaussquad]{laguerre.quadrature}},
#' \code{\link[gaussquad]{legendre.quadrature}},
#' \code{\link[gaussquad]{chebyshev.c.quadrature}},
#' \code{\link[gaussquad]{gegenbauer.quadrature}}
#'
#' @importFrom stats integrate
#' @export
integrate_stats <- function(fun, lower, upper, add_args = NULL, ...){
  int <- do.call(what = 'integrate',
                 args = c(list(f = fun, lower = lower, upper = upper),
                          add_args, ...))
  integral <- int$value
  return(integral)
}
#' @rdname integrate_stats
#' @export
gauss_quad <- function(fun, lower, upper,
                       add_args = list(n = 10, kind = "legendre",
                                       normalized = FALSE, weighted = TRUE),
                       ...){
  weight_fun <- paste0('gaussquad::', add_args$kind, '.quadrature')
  weight_rules <- paste0(weight_fun, '.rules')
  n <- add_args$n

  add_args['n'] <- NULL
  add_args['kind'] <- NULL
  rules_args <- names(formals(weight_rules))
  index <- match(add_args, rules_args, nomatch = 0)
  rules <- do.call(what = weight_rules,
                   args = c(list(n = n), add_args[index]))

  integral <- do.call(what = weight_fun,
                      args = c(list(functn = fun, lower = lower,
                                    upper = upper, rule = rules[[n]]),
                               add_args[-index], ...))
  return(integral)
}
