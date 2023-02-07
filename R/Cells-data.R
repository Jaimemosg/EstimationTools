#' Reduction cells
#'
#' Times to failure (in units of 1000 days) of 20 aluminum reduction cells.
#'
#' @format A data frame with 20 observations.
#' @examples
#' data(reduction_cells)
#' par(mfrow = c(1,2))
#' hist(reduction_cells$days, main="", xlab="Time (Days)")
#' plot(reduction_cells$days, xlab = "Cell (subjects)", lty = 3, type="h")
#'
#' @references
#' \insertRef{Whitmore1983}{EstimationTools}
"reduction_cells"
