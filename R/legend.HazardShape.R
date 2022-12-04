#' Customize legend for \code{\link{plot.HazardShape}} outputs
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Put legend after run \code{\link{plot.HazardShape}} function.
#'
#' @inheritParams graphics::legend
#' @param curve_options a list whose names are curve graphical parameters, and whose
#' values are the corresponding graphical parameters values.
#'
#' @details
#' This function is a wrapper for the \code{\link[graphics]{legend}} function.
#' It just adds two features:
#'
#' \itemize{
#'   \item `legend` has a default value, regarding that `HazardShape` objects
#'    produces the TTT plot and its LOESS estimation.
#'   \item `curve_options` is used to set legend parameters for the LOESS
#'    curve. We encourage you to define first a list with curve parameters, and then
#'    pass it to \code{\link{plot.HazardShape}} and `legend.HazardShape` (see example 2).
#' }
#'
#' @examples
#' library(EstimationTools)
#'
#' data("reduction_cells")
#' TTT_IG <- TTTE_Analytical(Surv(days, status) ~ 1, data = reduction_cells,
#'                           method = 'censored')
#' HS_IG <- TTT_hazard_shape(TTT_IG, data = reduction_cells)
#'
#' #----------------------------------------------------------------------------
#' # Example 1: put legend to the TTT plot of the reduction cells data set.
#'
#' # Run `plot.HazardShape` method.
#' par(
#'   cex.lab=1.8,
#'   cex.axis=1.8,
#'   mar=c(4.8,5.4,1,1),
#'   las = 1,
#'   mgp=c(3.4,1,0)
#' )
#' plot(HS_IG, pch = 16, cex = 1.8)
#'
#' # Finally, put the legend
#' legend.HazardShape(
#'   x = "bottomright",
#'   box.lwd = NA,
#'   cex = 1.8,
#'   pt.cex = 1.8,
#'   bty = 'n',
#'   pch = c(16, NA)
#' )
#' #----------------------------------------------------------------------------
#' # Example 2: example 1 with custom options for the curve
#'
#' # This is the list with curve parameters
#' loess_options <- list(
#'   col = 3, lwd = 4, lty = 2
#' )
#' par(
#'   cex.lab=1.8,
#'   cex.axis=1.8,
#'   mar=c(4.8,5.4,1,1),
#'   las = 1,
#'   mgp=c(3.4,1,0)
#' )
#'
#' plot(HS_IG, pch = 16, cex = 1.8, curve_options = loess_options)
#' legend.HazardShape(
#'   x = "bottomright",
#'   box.lwd = NA,
#'   cex = 1.8,
#'   pt.cex = 1.8,
#'   bty = 'n',
#'   pch = c(16, NA),
#'   curve_options = loess_options
#' )
#'
#' @importFrom graphics legend
#' @export
legend.HazardShape <- function(x, y = NULL,
                               legend = c("Empirical TTT", "Spline curve"),
                               fill = NULL, col = 1, border = "white",
                               lty = NA, lwd = NA, pch = c(1, NA), angle = 45,
                               density = NULL, bty = "o", bg = par("bg"),
                               box.lwd = par("lwd"), box.lty = par("lty"),
                               box.col = par("fg"), pt.bg = NA, cex = 1,
                               pt.cex = cex, pt.lwd = lwd, xjust = 0, yjust = 1,
                               x.intersp = 1, y.intersp = 1, adj = c(0, 0.5),
                               text.width = NULL, text.col = par("col"),
                               text.font = NULL, merge = TRUE,
                               trace = FALSE, plot = TRUE, ncol = 1,
                               horiz = FALSE, title = NULL, inset = 0,
                               xpd = TRUE, title.col = text.col[1],
                               title.adj = 0.5, title.cex = cex[1],
                               title.font = text.font[1], seg.len = 2,
                               curve_options = list(col = 2, lwd = 2, lty = 1)){

  par_plot <- par()

  col <- c(col, curve_options$col)
  lty <- c(lty, curve_options$lty)
  lwd <- c(lwd, curve_options$lwd)

  legend(x, y, legend, fill, col, border, lty, lwd, pch, angle, density, bty,
         bg, box.lwd, box.lty, box.col, pt.bg, cex, pt.cex, pt.lwd, xjust,
         yjust, x.intersp, y.intersp, adj, text.width, text.col, text.font,
         merge, trace, plot, ncol, horiz, title, inset, xpd, title.col,
         title.adj, title.cex, title.font, seg.len)
}
