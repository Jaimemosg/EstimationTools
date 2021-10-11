#' @title Detection functions in \code{maxlogL} framework
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function takes the name of a key function and computes the corresponding
#' detection function.
#'
#' @param fun a detection function. It must exist in Global Environment or loaded
#'            from a package.
#' @param support a list with the following entries:
#'                \itemize{
#'                \item \code{interval}: a two dimensional atomic vector indicating the
#'                set of possible values of a random variable having the
#'                distribution specified in \code{y_dist}.
#'                \item \code{type}: character indicating if distribution has a
#'                \code{discrete} or a \code{continous} random variable.
#'                }
#' @param routine a character specifying the integration routine.
#'                \code{integrate} and \code{gauss_quad} are available, but the
#'                custom routines can be defined.
#'
#' @param ... further arguments passed to the integration routine.
#'
#' @return A function with the folling input arguments:
#' \item{x}{vector of (non-negative) quantiles.}
#' \item{...}{Arguments of the probability density/mass function.}
#'
#' @examples
#' library(EstimationTools)
#'
#' #--------------------------------------------------------------------------------
#' # Example 1: Half normal detection function without adjustment term.
#' g <- detection_fun(fun=half_norm, support=list(interval=c(0, 100),
#'                    type='continuous'))
#' curve(g(x, sigma=4.1058), to=100)
#'
#' #--------------------------------------------------------------------------------
#'
#' @export
detection_fun <- function(fun, support, routine = "integrate", ...){
  w <- support$interval[2]
  fun_args <- formals(fun)
  fun_args <- fun_args[-1]
  arg_names <- names(fun_args)
  g <- function(){
    call_fun <- match.call()
    for (name in arg_names){
      fun_args[name] <- call_fun[name]
    }
    integ <- do.call(what='integration',
                     args = c(list(fun=fun, lower = support$interval[1],
                                   upper = w, routine = routine),
                              fun_args, ...))
    density <- do.call(what = fun,
                       args = c(list(x=x), fun_args))/integ
    if (log) density <- log(density)
    return(density)
  }
  formals(g) <- c(formals(fun), list(w=w, log=FALSE))
  return(g)
}
# detection_fun <- function(fun, support, routine = "integrate", ...){
#   w <- support$interval[2]
#   fun_args <- formals(fun)
#   fun_args <- fun_args[-1]
#   arg_names <- names(fun_args)
#   g <- function(){
#     call_fun <- match.call()
#     for (name in arg_names){
#       fun_args[name] <- call_fun[name]
#     }
#     integ <- do.call(what='integration',
#                      args = c(list(fun=fun, lower = support$interval[1],
#                                    upper = w, routine = routine),
#                               fun_args, ...))
#     density <- do.call(what = fun,
#                        args = c(list(x=x), fun_args))/integ
#     if (log) density <- log(density)
#     return(density)
#   }
#   formals(g) <- c(formals(fun), list(w=w, log=FALSE))
#   return(g)
# }
