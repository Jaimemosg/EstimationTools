#' @title Plot method for \code{EmpiricalTTT} objects
#'
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
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
#' @importFrom graphics matplot lines
#' @export
#'
#' @examples
#' library(EstimationTools)
#'
#' #--------------------------------------------------------------------------------
#' # First example: Scaled empirical TTT from 'mgus1' data from 'survival' package.
#'
#' TTT_1 <- TTT_EAnalytical(Surv(stop, event == 'pcm') ~1, method = 'cens',
#'                          data = mgus1, subset=(start == 0))
#' plot(TTT_1, type = "p")
#'
#'
#' #--------------------------------------------------------------------------------
#' # Second example: Scaled empirical TTT using a factor variable with 'aml' data
#' # from 'survival' package.
#'
#' TTT_2 <- TTT_EAnalytical(Surv(time, status) ~ x, method = "cens", data = aml)
#' plot(TTT_2, type = "l", lty = c(1,1), col = c(2,4))
#' plot(TTT_2, add = TRUE, type = "p", lty = c(1,1), col = c(2,4))
#'
#' #--------------------------------------------------------------------------------
#' # Third example: Non-scaled empirical TTT without a factor (arbitrarily simulated
#' # data).
#'
#' y <- rweibull(n=20, shape=1, scale=pi)
#' TTT_3 <- TTT_EAnalytical(y ~ 1, scaled = FALSE)
#' plot(TTT_3, type = "s", col = 3, lwd = 3)
#'
#'
#' #--------------------------------------------------------------------------------
#'
#' @seealso \code{\link{TTT_EAnalytical}}, \code{\link[graphics]{matplot}}
#' @method plot EmpiricalTTT
#'
#==============================================================================
# plot method -----------------------------------------------------------------
#==============================================================================
plot.EmpiricalTTT <- function(x, add = FALSE,  type = "l", pch = 1, xlab = "i/n",
                              ylab = expression(phi[n](i/n)), ...){
  matplot(x$`i/n`, x$phi_n, xlab = xlab, ylab = ylab,
          xlim = c(0,1), type = type, pch = pch, add = add, ...)
  if ( !add ) lines(c(0,1), c(0, max(x$phi_n[nrow(x$phi_n),])),
                    lty = 2)
}
