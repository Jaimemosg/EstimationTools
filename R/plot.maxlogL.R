#' @title Plot Residual Diagnostics for an \code{maxlogL} Object
#'
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Provides plots of Cox-Snell, martingale Randomized quantile residuals.
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
#' #----------------------------------------------------------------------------
#' # Example 1: Quantile residuals for a normal model
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
#' plot(norm_mod, type = "rqres", which.plots = 1:3)
#'
#'
#' #----------------------------------------------------------------------------
#' # Example 2: Cox-Snell residuals for an exponential model
#' data(ALL_colosimo)
#' formulas <- list(scale.fo = ~ lwbc)
#' support <- list(interval = c(0, Inf), type = 'continuous')
#'
#' ALL_exp_model <- maxlogLreg(
#'   formulas,
#'   fixed = list(shape = 1),
#'   y_dist = Surv(times, status) ~ dweibull,
#'   data = ALL_colosimo,
#'   support = support,
#'   link = list(over = "scale", fun = "log_link")
#' )
#'
#' summary(ALL_exp_model)
#' plot(ALL_exp_model, type = "cox-snell")
#' plot(ALL_exp_model, type = "martingale")
#' plot(ALL_exp_model, type = "censored-deviance")
#'
#'
#' @method plot maxlogL
#' @importFrom car qqPlot
#' @importFrom graphics rug curve abline
#' @importFrom survival survfit
#' @importFrom stats density qnorm pexp
#' @export
plot.maxlogL <- function(x,
                         type = c(
                           "rqres",
                           "cox-snell",
                           "martingale",
                           "censored-deviance"
                         ),
                         which.plots = NULL,
                         caption = NULL,
                         xvar = NULL,
                         ...) {
  type <- match.arg(type)
  if (is.null(which.plots)) which.plots <- available_plots[[type]]
  plot_residuals(x, type, which.plots, caption, xvar, ...)
}

plot_residuals <- function(object, type, which.plots, caption, xvar, ...) {
  resids <- residuals.maxlogL(object = object, type = type)
  y <- object$outputs$response
  n_panels <- length(which.plots)

  if (n_panels > 4) {
    warning(
      paste0(
        "This plot method only produces at maximum four plots. ",
        "Please, write another 'plot' statement choosing the remaining ",
        n_panels - 4, "plots with the argument 'which.plots'."
      )
    )
  }

  if (is.null(caption)) {
    caption <- all_captions[[type]]
  } else {
    n_captions <- length(caption)

    if (n_captions > 4){
      caption <- caption[1:4]
      warning(
        "The first four captions of the input array were selected."
      )
      n_captions <- 4
    }

    caption <- c(caption, rep("", 4 - n_captions))
  }

  if (length(which.plots) == 1) grid <- c(1, 1)
  if (length(which.plots) == 2) grid <- c(1, 2)
  if (length(which.plots) <= 4 & length(which.plots) > 2) grid <- c(2, 2)

  # valid_covariate_residuals <- c("martingale")
  #
  # if (!is.null(xvar) & any(type %in% valid_covariate_residuals)){
  #   grid <- c(1, 1)
  #
  #   caption <- paste0(
  #     stringr::str_to_title(type), " residuals against ", xvar, "."
  #   )
  # }
  #
  # if (!is.null(xvar) & !any(type %in% valid_covariate_residuals)){
  #   stop(
  #     paste0(
  #       "Residuals against covariate plot is only valid for ",
  #       paste(valid_covariate_residuals, collapse = ", "),
  #       "."
  #     )
  #   )
  # }

  plots_list <- residuals_plots_list[[type]]
  names(plots_list) <- caption
  plots_all_names <- names(plots_list[which.plots])

  par(mfrow = grid)

  for (plot_name in plots_all_names) {
    plots_list[[plot_name]](resids, y, plot_name, ...)
  }
}

#==============================================================================
# Randomized quantile residuals plot ------------------------------------------
#==============================================================================
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
  car::qqPlot(resids, distribution = "norm",
         xlab = "Theoretical normal quantiles",
         ylab = "Sample quantiles (from quantile residuals)",
         main = caption)
}

rqres_plots_list <- list(
  rqres_vs_fitted_values,
  rqres_vs_index,
  density_estimate,
  norm_qqplot
)

rqres_captions <- c(
  "Residuals against fitted values",
  "Residuals against index",
  "Density estimate plot",
  "Normal Q-Q Plot"
)
#==============================================================================
# Cox-Snell residuals plot ----------------------------------------------------
#==============================================================================
exp_qqplot <- function(resids, y = NULL, caption, ...){
  car::qqPlot(resids, distribution = "exp",
              xlab = "Theoretical exponential quantiles",
              ylab = "Sample quantiles (from Cox-Snell residuals)",
              main = caption,
              ...)
}

surv_exp_1 <- function(x) pexp(x, rate = 1, lower.tail = FALSE, log.p = FALSE)

survival_residuals_plot<- function(resids, y = NULL, caption, ...){
  KM <- survfit(Surv(resids) ~ 1)

  plot(
    KM, conf.int = FALSE,
    xlab = "Residuals",
    ylab = "Estimated S(res)",
    main = caption,
    ...
  )
  curve(surv_exp_1, add = TRUE, lty = 2, from = 0, to = max(resids))
  legend("topright", lty = c(1, 2), legend = c("Kaplan-Meier", "Exp(1)"))
}

S_exp_vs_S_KM <- function(resids, y = NULL, caption, ...){
  KM <- survfit(Surv(resids) ~ 1)

  t_0 <- min(resids)
  t_end <- max(resids)
  n_points <- KM$n - 1
  S_res_exp_1 <- surv_exp_1(resids)
  S_res_exp_1 <- sort(S_res_exp_1, decreasing = TRUE)[1:n_points]
  S_res_exp_1[n_points] <- 0

  plot(
    KM$surv,
    S_res_exp_1,
    xlab = "S(res) Kaplan-Meier",
    ylab = "S(res) Exp(1)",
    main = caption,
    ...
  )
  abline(a = 0, b = 1, lty = 2)
}

cox_snell_plots_list <- list(
  exp_qqplot,
  survival_residuals_plot,
  density_estimate,
  S_exp_vs_S_KM
)

cox_snell_captions <- c(
  "Residuals against Exp(1)",
  "Survival function of Cox-Snell residuals",
  "Density estimate plot",
  "Exp(1) survival function against KM of residuals"
)
#==============================================================================
# Martingale residuals plot ---------------------------------------------------
#==============================================================================
residuals_vs_response <- function(resids, y, caption, ...){
  plot(
    y,
    resids,
    main = caption,
    ylab = "Residuals",
    xlab = "Response",
    ...
  )
}
martingale_plots_list <- list(
  exp_qqplot,
  residuals_vs_response
)

martingale_captions <- c(
  "Residuals against Exp(1)",
  "Martingale residuals against response"
)
#==============================================================================
# Censored-deviance residuals plot --------------------------------------------
#==============================================================================
censored_deviance_plots_list <- list(
  exp_qqplot,
  residuals_vs_response
)

censored_deviance_captions <- c(
  "Residuals against Exp(1)",
  "Censored deviance residuals against response"
)

#==============================================================================
# Utility lists ---------------------------------------------------------------
#==============================================================================
residuals_plots_list <- list(
  rqres = rqres_plots_list,
  `cox-snell` = cox_snell_plots_list,
  martingale = martingale_plots_list,
  `censored-deviance` = censored_deviance_plots_list
)

all_captions <- list(
  rqres = rqres_captions,
  `cox-snell` = cox_snell_captions,
  martingale = martingale_captions,
  `censored-deviance` = censored_deviance_captions
)

available_plots <- list(
  rqres = 1:4,
  `cox-snell` = 1:4,
  martingale = 1:2,
  `censored-deviance` = 1:2
)
