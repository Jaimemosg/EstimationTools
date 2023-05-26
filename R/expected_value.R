#' @title Expected value of a \code{maxlogLreg} model.
#' @family maxlogL
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function takes a \code{maxlogL} model and computes the expected value
#' using the estimated parameters. The expected value is computed using the
#' following expression
#'
#' \deqn{\hat{E[g(X)]} = \int_{-\infty}^{\infty} x f(x|\hat{\theta}) dx,}
#'
#' where \eqn{f(x|\hat{\theta})} is a probability density function using the
#' estimated parameters.
#'
#'
#' @param object an object of \code{\link{maxlogL}} class obtained by fitting a
#'               model with \code{\link{maxlogLreg}}.
#' @param g a given function \eqn{g(x)}.
#' @param routine a character specifying the integration routine.
#'                \code{integrate} and \code{gauss_quad} are available for
#'                continuous distributions, and \code{summate} for discrete ones.
#'                Custom routines can be defined but they must be compatible
#'                with the \code{\link{integration}} API.
#' @param ... further arguments for the integration routine.
#'
#' @return the expected value of the fitted model corresponding to the
#' distribution specified in the \code{y_dist} argument of
#' \code{\link{maxlogLreg}}.
#'
#' @examples
#' library(EstimationTools)
#'
#' #----------------------------------------------------------------------------
#' # Example 1: mean value of a estimated model.
#' n <- 100
#' x <- runif(n = n, -5, 6)
#' y <- rnorm(n = n, mean = -2 + 3 * x, sd = 0.3)
#' norm_data <- data.frame(y = y, x = x)
#'
#' formulas <- list(sd.fo = ~ 1, mean.fo = ~ x)
#' support <- list(interval = c(-Inf, Inf), type = "continuous")
#'
#' norm_mod_maxlogL <- maxlogLreg(
#'   formulas, y_dist = y ~ dnorm,
#'   support = support,
#'   data = norm_data,
#'   link = list(over = "sd", fun = "log_link")
#' )
#'
#' # Actual y values
#' y <- norm_mod_maxlogL$outputs$response
#'
#' # Expected value
#' Ey <- expected_value.maxlogL(
#'   object = norm_mod_maxlogL,
#'   routine = "monte-carlo"
#' )
#'
#' # Compare
#' plot(y, Ey)
#'
#'
#' #----------------------------------------------------------------------------
#'
#' @export
#==============================================================================
# Computation of expected value for a fitted model ----------------------------
#==============================================================================
expected_value.maxlogL <- function(
    object,
    g = identity,
    routine,
    ...
){
  # parameters <- object$outputs$fitted.values
  # par_names <- names(parameters)
  # parameters <- matrix(unlist(parameters), nrow = object$outputs$n)
  # colnames(parameters) <- par_names
  parameters <- create_inputs(
    object, add_response = FALSE, as_matrix = TRUE
  )

  distr <- object$inputs$distr
  support <- object$inputs$support

  # routine <- set_custom_integration_routine(support, routine)
  #
  # if ( is.character(routine) ){
  #   if (routine == "monte-carlo"){
  #     mean_computation <- function(x){
  #       do.call(
  #         what = "expected_value_montecarlo",
  #         args = c(list(f = distr, g = identity, is_distr = TRUE, ...), x)
  #       )
  #     }
  #   } else {
  #     integrand <- function(distr){
  #       nm_distr <- as.name(distr)
  #       pars <- formals(args(distr))
  #       log_par <- pars[names(pars) == 'log']
  #       pars <- sapply(object$outputs$par_names, as.name)
  #       distr_call <- as.call(c(nm_distr, as.name('x'), pars, log_par))
  #
  #       if (is.character(g)){
  #         body_fun <- str2expression(paste('g(x) *', deparse(distr_call)))
  #       } else {
  #         body_fun <- expression(g(x) * distr_call)
  #       }
  #
  #       func <- function() 'body'
  #       formals(func) <- formals(args(distr))
  #       formals(func)[object$outputs$par_names] <-
  #         sapply(object$outputs$par_names, function(x) x <- bquote())
  #       body(func) <- body_fun
  #       return(func)
  #     }
  #
  #     EX <- integrand(distr)
  #
  #     mean_computation <- function(x){
  #       do.call(what = 'integration',
  #               args = c(
  #                 list(
  #                   fun = EX,
  #                   lower = support$interval[1],
  #                   upper = support$interval[2],
  #                   routine = routine, ...
  #                 ),
  #                 x
  #               )
  #       )
  #     }
  #   }
  #   result <- apply(parameters, MARGIN = 1, mean_computation)
  # }
  mean_computation <- function(x){
    do.call(
      what = "expected_value",
      args = list(
        f = distr,
        parameters = as.list(x),
        support = support,
        g = identity,
        routine = routine
      )
    )
  }
  result <- apply(parameters, MARGIN = 1, mean_computation)
  return(result)
}

#' @title Expected value of a given function for any distribution
#' @family distributions utilities
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function takes the name of a probability density/mass function as an
#' argument and creates a function to compute the expected value.
#'
#'
#' @param f a character with the probability density/mass function name. The
#'          function must be availble in the \code{R} environment using the
#'          usual nomenclature (\code{d} prefix before the name).
#' @param parameters a list with the input parameters for the distribution.
#' @param support a list with the following entries:
#'                \itemize{
#'                \item \code{interval}: a two dimensional atomic vector
#'                indicating the set of possible values of a random variable
#'                having the distribution specified in \code{y_dist}.
#'                \item \code{type}: character indicating if distribution has a
#'                \code{discrete} or a \code{continous} random variable.
#'                }
#' @param g a given function \eqn{g(x)}. If \code{g = identity}, then
#'          \eqn{g(x) = x} and this is actually the mean of the distribution.
#' @param routine a character specifying the integration routine.
#'                \code{integrate} and \code{gauss_quad} are available for
#'                continuous distributions, and \code{summate} for discrete ones.
#'                Custom routines can be defined but they must be compatible
#'                with the \code{\link{integration}} API.
#' @param ... further arguments for the integration routine.
#'
#' @return the expected value of the specified distribution.
#'
#' @examples
#' library(EstimationTools)
#'
#' #----------------------------------------------------------------------------
#' # Example 1: mean of X ~ N(2, 1) using 'integrate' under the hood.
#' support <- list(interval=c(-Inf, Inf), type = "continuous")
#'
#' expected_value(
#'   f = "dnorm",
#'   parameters = list(mean = 2, sd = 1),
#'   support = support
#' )
#'
#' # Equivalent to
#' expected_value(
#'   f = "dnorm",
#'   parameters = list(mean = 2, sd = 1),
#'   support = support,
#'   g = identity,
#'   routine = "integrate"
#' )
#'
#' # Example 1: mean of X ~ N(22, 1)
#'
#' # 'integrate' fails because the mean is 22.
#' expected_value(
#'   f = "dnorm",
#'   parameters = list(mean = 22, sd = 1),
#'   support = support
#' )
#'
#' # Let's compute with Monte Carlo integration
#' expected_value(
#'   f = "dnorm",
#'   parameters = list(mean = 22, sd = 1),
#'   support = support,
#'   routine = "monte-carlo"
#' )
#'
#' # Compute Monte Carlo integration with more samples
#' \donttest{
#' expected_value(
#'   f = "dnorm",
#'   parameters = list(mean = 22, sd = 1),
#'   support = support,
#'   routine = "monte-carlo",
#'   n = 1e8
#' )
#' }
#'
#' #----------------------------------------------------------------------------
#'
#' @export
#==============================================================================
# Computation of expected value for any distribution --------------------------
#==============================================================================
expected_value <- function(
    f,
    parameters,
    support,
    g = identity,
    routine = NULL,
    ...
){
  type <- match.arg(support$type, c("continuous", "discrete"))
  routine <- set_custom_integration_routine(support, routine)

  if (is.character(f)){
    if ( is.character(routine) ){
      if (routine == "monte-carlo"){
        result <- do.call(
            what = "expected_value_montecarlo",
            args = c(list(f = f, g = identity), parameters, ...)
          )
      } else {
        integrand <- function(distr){
          nm_distr <- as.name(distr)
          pars <- formals(distr)
          log_par <- pars[names(pars) == 'log']
          log_par$log <- FALSE

          par_names <- names(pars)
          par_names <- sapply(
            par_names[par_names != 'log' & par_names != 'x'], as.name
          )
          distr_call <- as.call(c(nm_distr, as.name('x'), par_names, log_par))

          body_fun <- str2expression(paste('g(x) *', deparse(distr_call)))

          func <- function() 'body'
          formals(func) <- formals(distr)
          formals(func)[names(par_names)] <-
            sapply(par_names, function(x) x <- bquote())
          body(func) <- body_fun
          return(func)
        }

        EX <- integrand(f)

        result <- do.call(
          what = 'integration',
          args = c(
            list(
              fun = EX,
              lower = support$interval[1],
              upper = support$interval[2],
              routine = routine,
              ...
            ),
            parameters
          )
        )
      }
    }
  }
  return(result)
}

expected_value_montecarlo <- function(
    f,
    g = identity,
    ...
){
  what <- paste0('r', substring(f, 2))
  add_args <- c(list(n = 1e6), substitute(...()))
  n_indexes <- which(names(add_args) == "n")
  if (length(n_indexes) > 1) add_args[[n_indexes[1]]] <- NULL

  sample_f <- do.call(
    what = what,
    args = add_args
  )
  integral <- mean(g(sample_f))
  return(integral)
}
