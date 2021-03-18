#' Configure various aspects of interpolating function in \code{TTT_hazard_shape}
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' This function allows the user to set the parameters of any of the following
#' interpolating functions which can be used inside \code{\link{TTT_hazard_shape}}.
#'
#' @param interp.fun character. This argument defines the interpolating
#'                   function used. Default value is \code{"splinefun"}. Visit the
#'                   \strong{Details} section for further information.
#' @param length.out numeric. Number of points interpolated. Default value is 10.
#' @param ... further arguments passed to the interpolating function.
#'
#' @details
#' Each interpolating function has its particular arguments. The following
#' interpolating functions are  recommended:
#'
#' \itemize{
#' \item \code{\link[stats]{approxfun}}
#' \item \code{\link[stats]{splinefun}}
#' \item \code{\link{spline}}
#' }
#'
#' The user can also implement a custom interpolating function.
#'
#' @seealso \code{\link[stats]{approxfun}}, \code{\link[stats]{splinefun}},
#' \code{\link[stats]{smooth}}, \code{\link[stats]{smooth.spline}},
#' \code{\link[stats]{loess}}, \code{\link{TTT_hazard_shape}}
#' @export
interp.options <- function(interp.fun = "splinefun", length.out = 10, ...){
  dots <- substitute(...())
  names_dots <- names(dots)
  interp_args <- names(formals(eval(parse(text = interp.fun))))[-c(1,2)]
  dots_match <- match(names_dots, interp_args)
  if ( any(is.na(dots_match)) ){
    dots_match <- which(is.na(dots_match))
    forbidden <- names_dots[dots_match]
    sentence <- c("Argument ", " is ")
    if ( length(forbidden) > 1 ) sentence <- c("Arguments ", " are ")
    stop(paste0(sentence[1], "'", forbidden, "'", sentence[2], "not available ",
                "for", " '", interp.fun, "' function."))
  }
  list_out <- c(list(interp.fun = interp.fun, length.out = length.out), dots)
  if ( length(list_out[3:length(list_out)]) == 0 ) list_out$passing_args <- NULL
  return(list_out)
}

