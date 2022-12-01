#' Print method for \code{HazardShape} objects
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Displays the estimated hazard shape given a \code{HazardShape} object.
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez \email{jmosquerag@unal.edu.co}
#'
#' @param x an object of class \code{HazardShape}, generated with
#'          \code{\link{TTT_hazard_shape}}.
#' @param ... further arguments passed to or from other methods.
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
#' print(my_initial_guess)
#'
#'
#' #--------------------------------------------------------------------------------
#'
#' @method print HazardShape
#' @export
print.HazardShape <- function(x, ...){
  cat("--------------------------------------------------------------------\n")
  cat("Hazard shape: ")
  cat(x$hazard_type)
  cat('\n')
  cat("--------------------------------------------------------------------\n")
  if ( !is.null(x$warning) ){
    warning(x$warning)
  } else {cat("Successful estimate!\n")}
}

