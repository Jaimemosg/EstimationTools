#' Summary of \code{HazardShape} objects
#'
#' @description
#' This function displays the estimated hazard shape given a
#' data set.
#'
#' @author Jaime Mosquera Gutiérrez \email{jmosquerag@unal.edu.co}
#'
#' @param object an object of class \code{HazardShape}, generated with
#'               \code{\link{TTT_hazard_shape}}.
#'
#' @export
Hazard_Shape <- function(object){
  cat("--------------------------------------------------------------------\n")
  cat("Hazard shape: ")
  cat(object$hazard_type)
  cat('\n')
  cat("--------------------------------------------------------------------\n")
  if ( !is.null(object$warning) ){
    warning(object$warning)
  } else {cat("Successful estimate!")}
}

