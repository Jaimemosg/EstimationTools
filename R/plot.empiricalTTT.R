#' @title Plot method for \code{EmpiricalTTT} objects
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Draws a TTT plot of an \code{EmpiricalTTT} object, one for each strata.
#'
#' TTT plots are graphed in the same order in which they appear in the list
#' element \code{strata} or in the list element \code{phi_n} of
#' the \code{EmpiricalTTT} object.
#'
#' @aliases plot.EmpiricalTTT
#'
#' @param x an object of class \code{EmpiricalTTT}.
#' @param add logical. If TRUE, \code{plot.EmpiricalTTT} add a TTT plot to an
#'            already existing plot.
#' @param grid logical. If \code{TRUE}, plot appears with grid.
#' @param type character string (length 1 vector) or vector of 1-character strings
#'             indicating the type of plot for each TTT graph. See \code{\link{plot}}.
#' @param xlab,ylab  titles for x and y axes, as in \code{\link{plot}}.
#' @param pch numeric (integer). A vector of plotting characters or symbols when
#'            \code{type = "p"}. See \code{\link[graphics]{points}}.
#' @param ... further arguments passed to \code{\link[graphics]{matplot}}. See
#'            the examples and \strong{Details} section for further information.
#'
#' @details This method is based on \code{\link[graphics]{matplot}}. Our function
#' sets some default values for graphic parameters: \code{type = "l"}, \code{pch = 1},
#' \code{xlab = "i/n"} and \code{ylab = expression(phi[n](i/n))}. This arguments
#' can be modified by the user.
#'
#' @importFrom graphics matplot lines grid
#' @export
#'
#' @examples
#' library(EstimationTools)
#'
#' #--------------------------------------------------------------------------------
#' # First example: Scaled empirical TTT from 'mgus1' data from 'survival' package.
#'
#' TTT_1 <- TTTE_Analytical(Surv(stop, event == 'pcm') ~1, method = 'cens',
#'                          data = mgus1, subset=(start == 0))
#' plot(TTT_1, type = "p")
#'
#'
#' #--------------------------------------------------------------------------------
#' # Second example: Scaled empirical TTT using a factor variable with 'aml' data
#' # from 'survival' package.
#'
#' TTT_2 <- TTTE_Analytical(Surv(time, status) ~ x, method = "cens", data = aml)
#' plot(TTT_2, type = "l", lty = c(1,1), col = c(2,4))
#' plot(TTT_2, add = TRUE, type = "p", lty = c(1,1), col = c(2,4), pch = 16)
#'
#' #--------------------------------------------------------------------------------
#' # Third example: Non-scaled empirical TTT without a factor (arbitrarily simulated
#' # data).
#'
#' y <- rweibull(n=20, shape=1, scale=pi)
#' TTT_3 <- TTTE_Analytical(y ~ 1, scaled = FALSE)
#' plot(TTT_3, type = "s", col = 3, lwd = 3)
#'
#'
#' #--------------------------------------------------------------------------------
#' # Fourth example: TTT plot for 'carbone' data from 'AdequacyModel' package
#'
#' if (!require('AdequacyModel')) install.packages('AdequacyModel')
#' library(AdequacyModel)
#' data(carbone)
#' TTT_4 <- TTTE_Analytical(response = carbone, scaled = TRUE)
#' plot(TTT_4, type = "l", col = "red", lwd = 2, grid = TRUE)
#'
#'
#' #--------------------------------------------------------------------------------
#'
#' @seealso \code{\link{TTTE_Analytical}}, \code{\link[graphics]{matplot}}
#' @method plot EmpiricalTTT
#' @export
plot.EmpiricalTTT <- function(x, add = FALSE, grid = FALSE, type = "l", pch = 1,
                              xlab = "i/n", ylab = expression(phi[n](i/n)), ...){
  matplot(x$`i/n`, x$phi_n, xlab = xlab, ylab = ylab,
          xlim = c(0,1), type = type, pch = pch, add = add, ...)
  if ( !add ){
    endpoint <- x$phi_n[nrow(x$phi_n),]
    endpoint <- max(endpoint[!is.na(endpoint)])
    lines(c(0,1), c(0, endpoint),
          lty = 4, lwd = 2)
  }
  if ( grid == TRUE )
    grid()
}
