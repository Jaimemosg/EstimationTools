#' Hazard shape extracted from \code{HazardShape} objects
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function displays the estimated hazard shape given a
#' data set.
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez \email{jmosquerag@unal.edu.co}
#'
#' @param object an object of class \code{HazardShape}, generated with
#'               \code{\link{TTT_hazard_shape}}.
#'
#' @examples
#' #--------------------------------------------------------------------------------
#' # Example 1: Increasing hazard and its corresponding TTT plot with simulated data
#'
#' hweibull <- function(x, shape, scale){
#'   dweibull(x, shape, scale)/pweibull(x, shape, scale, lower.tail = FALSE)
#'   }
#' curve(hweibull(x, shape = 2.5, scale = pi), from = 0, to = 42,
#'                col = "red", ylab = "Hazard function", las = 1, lwd = 2)
#'
#' y <- rweibull(n = 50, shape = 2.5, scale = pi)
#' my_initial_guess <- TTT_hazard_shape(formula = y ~ 1)
#' Hazard_Shape(my_initial_guess)
#'
#'
#' #--------------------------------------------------------------------------------
#' @keywords internal
#' @export
Hazard_Shape <- function(object){
  lifecycle::deprecate_warn("2.2.0", "Hazard_Shape()", "print.HazardShape()")
  cat("--------------------------------------------------------------------\n")
  cat("Hazard shape: ")
  cat(object$hazard_type)
  cat('\n')
  cat("--------------------------------------------------------------------\n")
  if ( !is.null(object$warning) ){
    warning(object$warning)
  } else {cat("Successful estimate!\n")}
}
