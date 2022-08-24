#==============================================================================
# Default key functions ------------------------------------------------------
#==============================================================================
#' @title Half normal key function
#' @family key functions
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function provides the half normal key function for model fitting in
#' distance sampling.
#'
#' @param x vector of perpendicular distances from the transect.
#' @param sigma scale parameter.
#'
#' @details
#' This is the half normal key function with parameter
#' \code{sigma}. Its expression is given by
#'
#' \eqn{g(x) = \exp(\frac{-x^2}{2*\sigma^2},}
#'
#' for x > 0.
#'
#' @return A numeric value corresponding to a given value of \code{x} and
#' \code{sigma}.
#'
#' @examples
#' library(EstimationTools)
#'
#' #----------------------------------------------------------------------------
#' # Example: Half normal function
#' half_norm_key(x=1, sigma=4.1058)
#' curve(half_norm_key(x, sigma=4.1058), from=0, to=20, ylab='g(x)')
#'
#' #----------------------------------------------------------------------------
#' @export
half_norm_key <- function(x, sigma){
  key <- exp(-x^2 / (2*sigma^2))
  return(key)
}
#=============================================================================#
#' @title Uniform key function
#' @family key functions
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function provides the uniform key function for model fitting in
#' distance sampling.
#'
#' @param x vector of perpendicular distances from the transect.
#' @param w half width of the strip transect.
#'
#' @details
#' This is the uniform key function with parameter
#' \code{sigma}. Its expression is given by
#'
#' \eqn{g(x) = \frac{1}{w},}
#'
#' for x > 0.
#'
#' @return A numeric value corresponding to a given value of \code{x} and
#' \code{w}.
#'
#' @examples
#' library(EstimationTools)
#'
#' #----------------------------------------------------------------------------
#' # Example: Uniform function
#' uniform_key(x=1, w=100)
#' curve(uniform_key(x, w=100), from=0, to=10, ylab='g(x)')
#'
#' #----------------------------------------------------------------------------
#' @export
uniform_key <- function(x, w){
  key <- rep(1/w, length(x))
  return(key)
}
#=============================================================================#
#' @title Hazard rate key function
#' @family key functions
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function provides the hazard rate key function for model fitting in
#' distance sampling.
#'
#' @param x vector of perpendicular distances from the transect.
#' @param sigma scale parameter.
#' @param beta shape parameter.
#'
#' @details
#' This is the hazard rate key function with parameters
#' \code{sigma} and \code{beta}. Its expression is given by
#'
#' \eqn{g(x) = 1 - \exp((\frac{-x}{\sigma})^{-\beta},}
#'
#' for x > 0.
#'
#' @return A numeric value corresponding to a given value of \code{x},
#' \code{sigma} and \code{beta}.
#'
#' @examples
#' library(EstimationTools)
#'
#' #----------------------------------------------------------------------------
#' # Example: Hazard rate function
#' hazard_rate_key(x=1, sigma=2, beta=3)
#' curve(hazard_rate_key(x, sigma=2, beta=3), from=0, to=10, ylab='g(x)')
#'
#' #----------------------------------------------------------------------------
#' @export
hazard_rate_key <- function(x, sigma, beta){
  key <- 1 - exp(-(x / sigma)^(-beta))
  return(key)
}
