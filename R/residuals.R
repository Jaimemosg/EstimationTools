#' @title Extract Residuals from \code{maxlogL} model.
#'
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' \code{residuals.maxlogL} is the \code{maxlogLreg} specific method for the
#' generic function residuals which extracts the residuals from a fitted model.
#'
#' @aliases residuals.maxlogL
#'
#' @param object an object of \code{\link{maxlogL}} class obtained by fitting a
#'               model with \code{\link{maxlogLreg}}.
#' @param parameter a character which specifies residuals for a specific parameter.
#' @param type a character with the type of residuals to be computed.
#'             The default value is \code{type = "rqres"}, which is used to
#'             compute the normalized randomized quantile residuals.
#' @param routine a character specifying the integration routine.
#'                \code{integrate} and \code{gauss_quad} are available. Custom
#'                routines can be defined but they must be compatible with the
#'                \code{\link{integration}} API.
#' @param ... further arguments for the integration routine.
#'
#' @details
#' For \code{type = "deviance"}, the residuals are computed using the following
#' expression
#'
#' \deqn{r^D_i = \mbox{sign}(y_i - \hat{\mu}_i) d_i^{1/2},}
#'
#' where \eqn{d_i} is the residual deviance of each data point. In this context,
#' \eqn{\hat{\mu}} is the estimated mean, computed as the expected value using
#' the estimated parameters of a fitted \code{maxlogLreg} model.
#'
#' On the other hand, for \code{type = "response"} the computation is simpler
#'
#' \deqn{r^R_i = (y_i - \hat{\mu}_i).}
#'
#' @return a vector with the specified residuals of a \code{maxlogLreg} model.
#'
#' @examples
#' library(EstimationTools)
#'
#' #----------------------------------------------------------------------------
#' # Example 1: Test deviance residuals
#' set.seed(123)
#' n <- 500
#' x <- runif(n = n, min = 0, max = 1)
#' y <- rweibull(n = n, shape = 1, scale = exp(4.5 + 0.5*x))
#' status <- rep(1, n) # sample(0:1, size = n, replace = TRUE)
#'
#' distribution <- Surv(y, status) ~ dweibull
#'
#' formulas <- list(
#'   scale.fo = ~ x
#' )
#'
#' fixed <- list(shape = 1)
#'
#' links <- list(
#'   over = "scale",
#'   fun = "log_link"
#' )
#'
#' model <- maxlogLreg(
#'   formulas = formulas,
#'   y_dist = distribution,
#'   fixed = fixed,
#'   link = links
#' )
#'
#' # Using `residuals` method
#' cox_snell_residuals_test <- residuals(model, type = "cox-snell")
#' martingale_residuals_test <- residuals(model, type = "martingale")
#' deviance_residuals_test <- residuals(model, type = "right-censored-deviance")
#'
#' # From scratch
#' cox_snell_residuals_ref <- -pweibull(
#'   q = y,
#'   shape = 1,
#'   scale = exp(cbind(rep(1, n), x) %*% cbind(coef(model))),
#'   lower.tail = FALSE,
#'   log.p = TRUE
#' )
#' martingale_residuals_ref <- status - cox_snell_residuals_ref
#' deviance_residuals_ref <- sign(martingale_residuals_ref) * (
#'   -2 * (martingale_residuals_ref + status*log(status - martingale_residuals_ref))
#' )^ 0.5
#'
#'
#' plot(cox_snell_residuals_test, cox_snell_residuals_ref)
#' plot(martingale_residuals_test, martingale_residuals_ref)
#' plot(deviance_residuals_test, deviance_residuals_ref)
#'
#'
#' #----------------------------------------------------------------------------
#'
#' @method residuals maxlogL
#' @export
residuals.maxlogL <- function(
    object,
    parameter = NULL,
    type = "rqres",
    routine,
    ...
){
  available_residuals <- c(
    "rqres",
    "response",
    "cox-snell",
    "martingale",
    "right-censored-deviance"
  )

  type <- match.arg(type, choices = available_residuals)

  if ( is.null(parameter) ) parameter <- object$outputs$par_names[1]
  parameter <- match.arg(parameter, choices = object$outputs$par_names)

  y <- object$outputs$response
  support <- object$inputs$support
  dist <- deparse(object$inputs$y_dist[[3]])
  cens <- object$inputs$cens

  if (
    is.null(support) &
    type %in% available_residuals &
    !missing(routine)
  ){
    stop(paste0(type, " residuals cannot be computed if a support is",
                " not defined. Please, refit your 'maxlogLreg' model",
                "specifying the 'support' argument."))
  }

  right_censorship <- check_right_censorship(
    cens_matrix = cens, type = type
  )

  if (right_censorship){
    # if ( is.Surv(object$inputs$y_dist) ){
    cumHaz <- cum_hazard.maxlogL(object)
    delta <- cens[, 3] + 1
    delta <- ifelse(delta == 2, 0, 1)

    # Martingale for right censored data
    mres <- delta - cumHaz

    if (type == "cox-snell") resid <- cumHaz

    if (type == 'martingale') resid <- mres

    if (type == 'right-censored-deviance'){
      deviance_i <- -2 * ( mres + delta * log(delta - mres) )
      resid <- sign(mres) * sqrt(deviance_i)
    }
    # }
  }

  if (type == "response"){
    if (missing(routine)) routine <- "monte-carlo"
    mean <- expected_value.maxlogL(
      object = object,
      routine = routine,
      n = 1e6
    )
    resid <- y - mean
  }

  if (type == "rqres"){
    distr <- object$inputs$distr
    cum_fun <- paste0('p', substring(distr, 2))

    fitted_parameters <- create_inputs(
      object, add_response = TRUE, as_matrix = FALSE
    )# object$outputs$fitted.values

    names(fitted_parameters)[names(fitted_parameters) == "x"] <- "q"

    Fyi <- do.call(
      what = cum_fun,
      # args = c(list(q = y, lower.tail = TRUE, log.p = FALSE), fitted_parameters)
      args = c(list(lower.tail = TRUE, log.p = FALSE), fitted_parameters)
    )
    resid <- qnorm(
      p = as.numeric(Fyi),
      mean = 0,
      sd = 1,
      lower.tail = TRUE,
      log.p = FALSE
    )
  }

  names(resid) <- 1:length(y)
  return(resid)
}

check_right_censorship <- function(
    cens_matrix,
    type
){
  choices <- c(
    "cox-snell",
    "martingale",
    "right-censored-deviance"
  )

  right_censored_residual <- type %in% choices

  if ( sum(cens_matrix[, 2]) > 0 & right_censored_residual ){
    stop(
      paste0(
        "'", type, "'",
        " residuals are not available for left censored data. Please, ",
        "compute randomized quantile residuals or raw residuals, which are ",
        "available for any censorship type. Just set residuals = 'rqres' or ",
        "residuals = 'response' respectively.")
    )
  }

  right_censored_data <- sum(cens_matrix[, 3]) > 0 | sum(cens_matrix[, 1]) > 0

  if ( right_censored_data & right_censored_residual ){
    output <- TRUE
  } else {
    output <- FALSE
  }
  return(output)
}
#==============================================================================
# Deviance for each data point ------------------------------------------------
#==============================================================================
# Deviance of each data point for \code{maxlogLreg} outputs
# This function computes the deviance for each data point from the
# response variable given a fitted model.
#
# param object an object of \code{maxlogL} class generated by
#       \code{\link{maxlogLreg}} function.
#
# details
# The function requires a fitted model with \code{maxlogLreg}.
#
# return
# This function returns a list with the following elements:
# \enumerate{
#    \item \code{deviance_i}: the contribution of each value of the
#    response variable in the data set to the deviance.
#    \item \code{proposed_deviance_i}: log-likelihood of data given the fitted
#    model in \code{object} argument.
#    \item \code{saturated_deviance_i}. log-likelihood of data given the
#    saturated model.
# }
dev.resids <- function(object){
  if (object$outputs$type != "maxlogLreg")
    stop(paste("'dev.resids()' method only useful for models with ",
               "covariates. \n Use 'maxlogLreg' in order to ",
               "take advantage of this method."))
  # Details of Proposed model
  distr <- object$inputs$distr
  fitted.values0 <- object$outputs$fitted.values
  y <- object$outputs$response
  loglik0_i <- do.call(what = distr,
                       args = c(list(x = y, log = TRUE), fitted.values0))

  # Saturated model
  saturated_model <- saturated_maxlogL(object)
  fitted.valuesS <- saturated_model$outputs$fitted.values
  loglikS_i <- do.call(what = distr,
                       args = c(list(x = y, log = TRUE), fitted.valuesS))
  output <- list(deviance_i = 2 * (loglikS_i - loglik0_i),
                 proposed_deviance_i = loglik0_i,
                 saturated_deviance_i = loglikS_i)
  return(output)
}

