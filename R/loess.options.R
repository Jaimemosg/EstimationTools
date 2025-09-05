#' Configure various aspects of LOESS in \code{TTT_hazard_shape}
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' This function allows the user to set the parameters of \code{loess} function
#' used inside \code{TTT_hazard_shape}.
#'
#' @param span 	the parameter which controls the degree of smoothing.
#' @param ... further arguments passed to \code{\link[stats]{loess}} function.
#'
#' @details
#' Please, visit \code{\link[stats]{loess}} to know further possible arguments.
#' The following arguments are not available for passing to the LOESS estimation:
#' \describe{
#' \item{data}{The only data handled inside \code{TTT_hazard_shape} is the
#' computed empirical TTT.}
#' \item{subset}{This argument is used in \code{loess} to take a subset of data.
#' In this context, it is not necessary.}
#' }
#'
#' @seealso \code{\link[stats]{loess}}, \code{\link{TTT_hazard_shape}}
#' @export
loess.options <- function(span = 2/3, ...){
  dots <- substitute(...())
  names_dots <- names(dots)
  loess_args <- names(formals(eval(parse(text = "loess"))))
  loess_args <- loess_args[which(loess_args != "data" & loess_args != "subset" &
                                   loess_args != "...")]
  dots_match <- match(names_dots, loess_args)
  if ( any(is.na(dots_match)) ){
    dots_match <- which(is.na(dots_match))
    forbidden <- names_dots[dots_match]
    sentence <- c(" Argument ", " is ")
    if ( length(forbidden) > 1 ) sentence <- c("Arguments ", " are ")
    stop(paste0(sentence[1], "'", forbidden, "'", sentence[2], "not available ", "for",
                " 'loess' function."))
  }
  return(c(list(span=span), dots))
}
