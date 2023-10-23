#' @title Plot Residual Diagnostics for an \code{maxlogL} Object
#'
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Provides plots of Cox-Snell, martingale Randomized quantile residuals.
#'
# @aliases plot.maxlogL
#'
#' @param x a \code{maxlogL} object.
#' @param type a character with the type of residuals to be plotted.
#'             The default value is \code{type = "rqres"}, which is used to
#'             compute the normalized randomized quantile residuals.
#' @param parameter which parameter fitted values are required for
#'                  \code{type = "rqres"}. The default is the first one
#'                  defined in the pdf,e.g, in \code{dnorm}, the default
#'                  parameter is \code{mean}.
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
#'
#' # Quantile residuals diagnostic plot
#' plot(norm_mod, type = "rqres")
#' plot(norm_mod, type = "rqres", parameter = "sd")
#'
#' # Exclude Q-Q plot
#' plot(norm_mod, type = "rqres", which.plots = 1:3)
#'
#'
#' #----------------------------------------------------------------------------
#' # Example 2: Cox-Snell residuals for an exponential model
#' data(ALL_colosimo_table_4_1)
#' formulas <- list(scale.fo = ~ lwbc)
#' support <- list(interval = c(0, Inf), type = 'continuous')
#'
#' ALL_exp_model <- maxlogLreg(
#'   formulas,
#'   fixed = list(shape = 1),
#'   y_dist = Surv(times, status) ~ dweibull,
#'   data = ALL_colosimo_table_4_1,
#'   support = support,
#'   link = list(over = "scale", fun = "log_link")
#' )
#'
#' summary(ALL_exp_model)
#' plot(ALL_exp_model, type = "cox-snell")
#' plot(ALL_exp_model, type = "right-censored-deviance")
#'
#' plot(ALL_exp_model, type = "martingale", xvar = NULL)
#' plot(ALL_exp_model, type = "martingale", xvar = "lwbc")
#'
#'
#' #----------------------------------------------------------------------------
#'
#' @method plot maxlogL
#' @importFrom car qqPlot
#' @importFrom graphics rug curve abline panel.smooth
#' @importFrom survival survfit
#' @importFrom stats density qnorm pexp
#' @export
plot.maxlogL <- function(x,
                         type = c(
                           "rqres",
                           "cox-snell",
                           "martingale",
                           "right-censored-deviance"
                         ),
                         parameter = NULL,
                         which.plots = NULL,
                         caption = NULL,
                         xvar = NULL,
                         ...) {
  type <- match.arg(type)
  resid_vs_covaraiate <- as.character(type %in% xvar_valid_types)

  plot_function <- switch (resid_vs_covaraiate,
    `TRUE` = plot_selector,
    `FALSE`= plot_residuals_generic
  )
  plot_function(x, type, parameter, which.plots, xvar, caption, ...)
}

xvar_valid_types <- c("martingale", "right-censored-deviance")

plot_selector <- function(x, type, parameter, which.plots, xvar, caption, ...){
  if (type %in% xvar_valid_types){
    if (!is.null(xvar)){

      if (!is.null(which.plots)) warning(
        "'which.plots' argument was ignored beacase 'xvar' was defined."
      )
      covariates_vector <- xvar

      if (xvar == "all"){
        parameters_names <- x$outputs$par_names
        design_matrixes_list <- x$outputs$design_matrix[parameters_names]
        covariates_list <- lapply(design_matrixes_list, function(x) colnames(x))
        covariates_vector <- unique(unlist(covariates_list))
        covariates_vector <- covariates_vector[covariates_vector != "(Intercept)"]
        which.plots <- 1:length(covariates_vector)
      }

      for (xvar in covariates_vector){
        plot_residuals_vs_xvar(x, xvar, type, caption, ...)
      }

    } else {

      plot_residuals_generic(
        x, type, parameter, which.plots, xvar, caption, ...
      )

    }
  }
}

plot_residuals_generic <- function(
    object, type, parameter, which.plots, xvar, caption, ...
) {

  if (is.null(which.plots)) which.plots <- available_plots[[type]]
  resids <- residuals.maxlogL(object = object, type = type)

  if (is.null(parameter)) parameter <- object$outputs$par_names[1]
  fitted_values <- object$outputs$fitted.values[[parameter]]
  if (type == "right-censored-deviance") fitted_values <- object$outputs$response

  n_panels <- length(which.plots)

  if (n_panels > 4) {
    warning(
      paste0(
        "This plot method only produces at maximum four plots. ",
        "Please, write another 'plot' statement choosing the remaining ",
        n_panels - 4, " plots with the argument 'which.plots'."
      )
    )
  }

  if (is.null(caption)) {
    caption <- all_captions[[type]]
  } else {
    n_captions <- length(caption)
    max_num_captions <- length(all_captions[[type]])

    if (n_captions > max_num_captions){
      caption <- caption[1:max_num_captions]
      warning(
        paste0(
          "The first ",
          max_num_captions,
          " captions of the input array were selected."
        )
      )
      n_captions <- max_num_captions
    }

    caption <- c(caption, rep("", max_num_captions - n_captions))
  }

  if (length(which.plots) == 1) par(mfrow = c(1, 1))
  if (length(which.plots) == 2) par(mfrow = c(1, 2))
  if (length(which.plots) <= 4 & length(which.plots) > 2) par(mfrow = c(2, 2))

  plots_list <- residuals_plots_list[[type]]

  names_list <- caption
  for(i in 1:length(names_list)){
    if (names_list[i] == "") names_list[i] <- as.character(i)
  }

  names(plots_list) <- names_list
  plots_all_names <- names(plots_list[which.plots])
  caption <- caption[which.plots]

  for (i in 1:length(plots_all_names)) {
    plots_list[[ plots_all_names[i] ]](
      resids, fitted_values, caption[i], parameter, ...
    )
  }
}

#==============================================================================
# Randomized quantile residuals plot ------------------------------------------
#==============================================================================
rqres_vs_fitted_values <- function(resids, y, caption, parameter, ...) {
  plot(
    y,
    resids,
    main = caption,
    xlab = bquote(italic( .(parameter) ) * " parameter fitted values"),
    ylab = "Quantile residuals",
    ...
  )
  panel.smooth(
    y,
    resids,
    main = caption,
    lwd = 2,
    ...
  )
}

rqres_vs_index <- function(resids, y = NULL, caption, parameter = NULL, ...){
  plot(
    resids,
    main = caption,
    xlab = "index",
    ylab = "Quantile residuals",
    ...
  )
  panel.smooth(
    1:length(resids),
    resids,
    main = caption,
    lwd = 2,
    ...
  )
}

density_estimate <- function(resids, y = NULL, caption, parameter = NULL, ...){
  density_residuals <- density(resids)
  plot(
    density_residuals,
    main = caption
  )
  rug(jitter(resids))
}

norm_qqplot <- function(resids, y = NULL, caption, parameter = NULL, ...){
  car::qqPlot(resids, distribution = "norm",
         xlab = "Theoretical normal quantiles",
         ylab = "Sample quantiles (quantile residuals)",
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
exp_qqplot <- function(resids, y = NULL, caption, parameter, ...){
  car::qqPlot(resids, distribution = "exp",
              xlab = "Theoretical exponential quantiles",
              ylab = "Sample quantiles (Cox-Snell residuals)",
              main = caption,
              ...)
}

surv_exp_1 <- function(x) pexp(x, rate = 1, lower.tail = FALSE, log.p = FALSE)

survival_residuals_plot<- function(
    resids, y = NULL, caption, paramter = NULL, ...
){
  KM <- survfit(Surv(resids) ~ 1)

  plot(
    KM, conf.int = FALSE,
    xlab = "Residuals",
    ylab = "Estimated S(res)",
    main = caption,
    ...
  )

  curve(surv_exp_1, add = TRUE, lty = 2, from = 0, to = max(resids))

  dots <- list(...)
  any_invalid_param <- any(!(names(formals(legend)) %in% names(dots)))
  if (any_invalid_param){
    legend_args <- dots[names(dots)[names(dots) %in% names(formals(legend))]]
  }

  do.call(
    what = "legend",
    args = c(
      list(x = "topright", lty = c(1, 2), legend = c("Kaplan-Meier", "Exp(1)")),
      legend_args
    )
  )
  # legend("topright", lty = c(1, 2), legend = c("Kaplan-Meier", "Exp(1)"), ...)
}

S_exp_vs_S_KM <- function(resids, y = NULL, caption, parameter = NULL, ...){
  S_res_exp_1 <- surv_exp_1(resids)
  S_res_exp_1 <- sort(S_res_exp_1, decreasing = TRUE)

  KM <- survfit(Surv(resids) ~ 1)

  t_0 <- min(resids)
  t_end <- max(resids)
  n_points <- KM$n
  surv_func <- KM$surv
  length_surv_func <- length(surv_func)
  length_resids <- length(resids)

  if (length_surv_func < length_resids){
    # n_points <- n_points - 1
    # S_res_exp_1[n_points] <- 0
    surv_func[(length_surv_func + 1):length_resids] <- 0
  }

  # S_res_exp_1 <- S_res_exp_1[1:n_points]

  plot(
    surv_func,
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
  "Exp(1) against KM of residuals"
)
#==============================================================================
# Martingale residuals plot ---------------------------------------------------
#==============================================================================
residuals_vs_response <- function(resids, y, caption, parameter = NULL, ...){
  plot(
    y,
    resids,
    main = caption,
    ylab = "Residuals",
    xlab = "Response",
    ...
  )
  panel.smooth(
    y,
    resids,
    lwd = 2,
    ...
  )
}
martingale_plots_list <- list(
  residuals_vs_response
)

martingale_captions <- c(
  "Martingale residuals against response"

)

plot_residuals_vs_xvar <- function(x, xvar, type, caption, ...) {

  if (length(xvar) > 1){
    warning(
      paste0(
        "This plot method only produces at maximum one plots. ",
        "Please, write another 'plot' statement writing a covariate ",
        "name from your dataset in the argument 'xvar'."
      )
    )
  }
  resids <- residuals.maxlogL(object = x, type = type)
  y <- x$outputs$response

  data <- x$inputs$data
  x <- data[, xvar]

  caption <- paste0(stringr::str_to_title(type), " residuals against ", xvar)

  plot(
    x,
    resids,
    main = caption,
    ylab = "Residuals",
    xlab = xvar,
    ...
  )
  panel.smooth(
    x,
    resids,
    lwd = 2,
    ...
  )
}
#==============================================================================
# Right censored- deviance residuals plot -------------------------------------
#==============================================================================
# censored_deviance_plots_list <- list(
#   residuals_vs_response
# )
#
# censored_deviance_captions <- c(
#   "Right censored deviance residuals against response"
# )
norm_qqplot_deviance <- function(resids, y = NULL, caption, parameter = NULL, ...){
  car::qqPlot(resids, distribution = "norm",
              xlab = "Theoretical normal quantiles",
              ylab = "Sample quantiles (deviance residuals)",
              main = caption)
}

censored_deviance_plots_list <- list(
  norm_qqplot_deviance
)

censored_deviance_captions <- c(
  "Normal Q-Q Plot"
)

#==============================================================================
# Utility lists ---------------------------------------------------------------
#==============================================================================
residuals_plots_list <- list(
  rqres = rqres_plots_list,
  `cox-snell` = cox_snell_plots_list,
  martingale = martingale_plots_list,
  `right-censored-deviance` = censored_deviance_plots_list
)

all_captions <- list(
  rqres = rqres_captions,
  `cox-snell` = cox_snell_captions,
  martingale = martingale_captions,
  `right-censored-deviance` = censored_deviance_captions
)

available_plots <- list(
  rqres = 1:4,
  `cox-snell` = 1:4,
  martingale = 1,
  `right-censored-deviance` = 1
)
