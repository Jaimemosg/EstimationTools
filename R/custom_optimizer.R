#' Set custom optimizer in \code{maxlogLreg}
#'
#' @description
#' This function allows the selection of a custom S3 optimizer and additional
#' arguments to be passed to it.
#'
#' @param optimizer_name a one-length character specifying the name of the optimizer to be used.
#' @param objective_name a one-length character with the name of the optimizer.
#' @param start_name  a one-length character with the name of the start/initial
#'                    values input argument.
#' @param lower_name a one-length character with the name of the lower bounds
#'                   input argument.
#' @param upper_name a one-length character with the name of the upper bounds
#'                   input argument.
#' @param optim_vals_name a one-length character with the name of the optimum
#'                        values output argument.
#' @param objective_vals_name a one-length character with the name of the objective
#'                        function name output argument.
#' @param ... Further arguments to be passed to the selected optimizer. Do not
#'            include lower bounds, upper bounds and start/initial values. It must
#'            be included in the \code{maxlogLreg} function definition
#'
#' @return a list containing the selected optimizer name and the additional,
#' the names of the start values argument, upper and lower bounds, optimum output
#' values, objective function output value and the arguments passed to it.
#'
#' @examples
#' library(EstimationTools)
#'
#' #--------------------------------------------------------------------------------
#' # Example 1: Estimation in simulated normal distribution (alternative
#' # implementation using `nlminb` as a custom optimizer)
#' n <- 1000
#' x <- runif(n = n, -5, 6)
#' y <- rnorm(n = n, mean = -2 + 3 * x, sd = exp(1 + 0.3* x))
#' norm_data <- data.frame(y = y, x = x)
#'
#' formulas <- list(sd.fo = ~ x, mean.fo = ~ x)
#' support <- list(interval = c(-Inf, Inf), type = 'continuous')
#'
#' # Default optimizer
#' norm_mod1 <- maxlogLreg(formulas, y_dist = y ~ dnorm, support = support,
#'                         data = norm_data,
#'                         link = list(over = "sd", fun = "log_link"))
#' summary(norm_mod1)
#'
#' # Use the default optimizer as a custom one
#' optimizer <- set_optimizer(
#'   optimizer_name = "nlminb",
#'   objective_name = "objective",
#'   start_name = "start",
#'   lower_name = "lower",
#'   upper_name = "upper",
#'   optim_vals_name = "par",
#'   objective_vals_name = "objective"
#' )
#'
#' norm_mod2 <- maxlogLreg(formulas, y_dist = y ~ dnorm, support = support,
#'                         data = norm_data,
#'                         link = list(over = "sd", fun = "log_link"),
#'                         optimizer = optimizer, start= NULL, lower = NULL,
#'                         upper = NULL)
#' summary(norm_mod2)
#'
#' @export
set_optimizer <- function(
    optimizer_name,
    objective_name,
    start_name,
    lower_name,
    upper_name,
    optim_vals_name,
    objective_vals_name,
    ...
){
  further_args <- list(...)
  further_args <- if(length(further_args) > 1) further_args[[1]] else NULL

  output <- list(
    optimizer_name = optimizer_name,
    objective_name = objective_name,
    bounds_arguments = list(
      lower_name = lower_name, upper_name = upper_name, start_name = start_name
    ),
    outputs_names = list(
      optim_vals = optim_vals_name, objective_vals = objective_vals_name
    ),
    further_args = further_args
  )

  class(output) <- "optimizer.config"
  return(output)
}
