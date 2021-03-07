#' Is return of any object of \code{EstimationTools}?
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez \email{jmosquerag@unal.edu.co}
#'
#' @description
#' Checks if an object is any of the classes implemented in \code{EstimationTools}
#' package.
#'
#' @param x Any object of \code{EstimationTools}.
#'
#==============================================================================
# maxlogL function identification ---------------------------------------------
#==============================================================================
#' @export
is.maxlogL <- function(x){
  inherits(x, c("maxlogL"))
}
#==============================================================================
# EmpiricalTTT function identification ----------------------------------------
#==============================================================================
#' @export
#' @rdname is.maxlogL
is.EmpiricalTTT <- function(x){
  inherits(x, c("EmpiricalTTT"))
}
#==============================================================================
# EmpiricalTTT function identification ----------------------------------------
#==============================================================================
#' @export
#' @rdname is.maxlogL
is.HazardShape <- function(x){
  inherits(x, c("HazardShape"))
}
