#' @title Plot Residual Diagnostics for an \code{maxlogL} Object
#'
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Provides plots of Cox-Snell and martingale residuals.
#'
#' @aliases plot.maxlogL
#'
#' @param x a \code{maxlogL} object.
#' @param type a character with the type of residuals to be plotted.
#'             The default value is \code{type = "rqres"}, which is used to
#'             compute the normalized randomized quantile residuals.
#' @param which.plots a subset of numbers to specify the plots. See details
#'                    for further information.
#' @param caption title of the plots. If \code{caption = NULL}, the default
#'                 values are used.
#' @param xvar an explanatory variable to plot the residuals against.
#' @param ... further parameters for the \link{plot} method.
#'
#' @details
#' If \code{type = "rqres"}, the available subset is \code{1:4}, referring to:
#' \itemize{
#'   \item 1. Quantile residuals vs. fitted values (Tukey-Ascomb plot)
#'   \item 2. Quantile residuals vs. index
#'   \item 3. Density plot of quantile residuals
#'   \item 4. Normal Q-Q plot of the quantile residuals.
#' }
#'
#'
#' @return Returns specified plots related to the residuals of the fitted
#' \code{maxlogL} model.
#'
#' @examples
#' library(EstimationTools)
#'
#' #--------------------------------------------------------------------------------
#' # Example 1: Estimation in simulated normal distribution
#' n <- 1000
#' x <- runif(n = n, -5, 6)
#' y <- rnorm(n = n, mean = -2 + 3 * x, sd = exp(1 + 0.3* x))
#' norm_data <- data.frame(y = y, x = x)
#'
#' # It does not matter the order of distribution parameters
#' formulas <- list(sd.fo = ~ x, mean.fo = ~ x)
#' support <- list(interval = c(-Inf, Inf), type = 'continuous')
#'
#' norm_mod <- maxlogLreg(formulas, y_dist = y ~ dnorm, support = support,
#'                        data = norm_data,
#'                        link = list(over = "sd", fun = "log_link"))
#' plot(norm_mod, type = "rqres")
#'
#' @method plot maxlogL
#' @importFrom car qqPlot
#' @importFrom graphics rug
#' @importFrom stats density qnorm
#' @export
plot.maxlogL <- function(x,
                         type = c("rqres", "cox-snell", "martingale"),
                         which.plots = 1:4,
                         caption = NULL,
                         xvar = NULL,
                         ...) {
  type <- match.arg(type)

  switch(type,
    rqres = plot_rqres(x, which.plots, caption, ...),
    `cox-snell` = cat("hi"),
    martingale = cat("bye")
  )
}

plot_rqres <- function(object, which.plots, caption, ...) {
  resids <- residuals.maxlogL(object = object, type = "rqres")
  y <- object$outputs$response
  n_panels <- length(which.plots)
  if (n_panels > 4) {
    warning(
      paste0(
        "This plot method only produces at maximum four plots. ",
        "Please, write another 'plot' statement for the remaining ",
        n_panels - 4, "plots."
      )
    )
  }
  if (is.null(caption)) {
    caption <- c(
      "Residuals against fitted values",
      "Residuals against index",
      "Density estimate plot",
      "Normal Q-Q Plot"
    )
  }
  names(plots_list) <- caption
  plots_all_names <- names(plots_list[which.plots])
  par(mfrow = c(2, 2))

  for (plot_name in plots_all_names) {
    plots_list[[plot_name]](resids, y, plot_name, ...)
  }
}

rqres_vs_fitted_values <- function(resids, y, caption, ...) {
  plot(
    y,
    resids,
    main = caption,
    xlab = "Fitted values",
    ylab = "Quantile residuals"
  )
}

rqres_vs_index <- function(resids, y = NULL, caption, ...){
  plot(
    resids,
    main = caption,
    xlab = "index",
    ylab = "Quantile residuals"
  )
}

density_estimate <- function(resids, y = NULL, caption, ...){
  density_residuals <- density(resids)
  plot(
    density_residuals,
    main = caption
  )
  rug(jitter(resids))
}

norm_qqplot <- function(resids, y = NULL, caption, ...){
  qqPlot(resids, distribution = "norm",
         xlab = "Theoretical Quantiles",
         ylab = "Sample Quantiles (from quantile residuals)",
         main = caption)
}

plots_list <- list(
  rqres_vs_fitted_values,
  rqres_vs_index,
  density_estimate,
  norm_qqplot
)
