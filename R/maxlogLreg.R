#' @title Maximum Likelihood Estimation for parametric linear regression models
#' @family maxlogL
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Function to compute maximum likelihood estimators (MLE) of regression parameters
#' of any distribution implemented in \code{R} with covariates (linear predictors).
#'
#' @param formulas a list of formula objects. Each element must have an \code{~},
#' with the terms on the right separated by \code{+} operators. The response
#' variable on the left side is optional. Linear predictor of each parameter must
#' be specified with the name of the parameter followed by the suffix \code{'.fo'}.
#' See the examples below for further illustration.
#' @param y_dist a formula object that specifies the distribution of the response
#'               variable. On the left side of \code{~} must be the response,
#'               and in the right side must be the name o de probability
#'               density/mass function. See the section \strong{Details} and the
#'               examples below for further illustration.
#' @param support a list with the following entries:
#'                \itemize{
#'                \item \code{interval}: a two dimensional atomic vector indicating
#'                      the set of possible values of a random variable having the
#'                      distribution specified in \code{y_dist}.
#'                \item \code{type}: character indicating if distribution has a
#'                \code{discrete} or a \code{continous} random variable.
#'                }
#' @param data an optional data frame containing the variables in the model.
#'             If data is not specified, the variables are taken from the
#'             environment from which \code{maxlogLreg} is called.
#' @param subset an optional vector specifying a subset of observations to be used
#'               in the fitting process.
#' @param fixed a list with fixed/known parameters of distribution of interest.
#'              Fixed parameters must be passed with its name and its value (known).
#' @param link a list with names of parameters to be linked, and names of the
#'             link function object. For names of parameters, please visit
#'             documentation of density/mass function. There are three link
#'             functions available: \code{\link{log_link}}, \code{\link{logit_link}}
#'             and \code{\link{NegInv_link}}. Take into account: the order
#'             used in argument \code{over} corresponds to the order in argument
#'             \code{link}.
#' @param optimizer a length-one character vector with the name of optimization
#'                  routine. \code{\link[stats:nlminb]{nlminb}}, \code{\link[stats:optim]{optim}} and
#'                  \code{\link[DEoptim:DEoptim]{DEoptim}} are available; \code{\link[stats:nlminb]{nlminb}}
#'                  is the default routine.
#' @param start a numeric vector with initial values for the parameters to be
#'              estimated. Zero is the default value.
#' @param lower a numeric vector with lower bounds, with the same lenght of
#'              argument `start` (for box-constrained optimization). \code{-Inf}
#'              is the default value.
#' @param upper a numeric vector with upper bounds, with the same lenght of
#'              argument `start` (for box-constrained optimization). \code{Inf}
#'              is the default value.
#' @param inequalities a character vector with the inequality constrains for
#'                     the distribution parameters.
#' @param control control parameters of the optimization routine. Please, visit
#'                documentation of selected optimizer for further information.
#' @param StdE_method a length-one character vector with the routine for Hessian
#'                    matrix computation. The This is needed for standard error
#'                    estimation. The options available are \code{"optim"} and
#'                    \code{"numDeriv"}. For further information, visit
#'                    \code{\link[stats]{optim}} or \code{\link[numDeriv]{hessian}}.
#' @param silent  logical. If TRUE, warnings of \code{maxlogL} are suppressed.
#' @param ... Further arguments to be supplied to the optimization routine.
#'
#' @return A list with class \code{maxlogL} containing the following
#'  lists:
#' \item{fit}{A list with output information about estimation and method used.}
#' \item{inputs}{A list with all input arguments.}
#' \item{outputs}{A list with additional information. The most important outputs
#'       are:
#'       \itemize{
#'          \item \code{npar}: number of parameters.
#'          \item \code{n}: sample size
#'          \item \code{Stde_method}: standard error computation method.
#'          \item \code{b_lenght}: a list with the number of regression parameters.
#'          \item \code{design_matrix}: a list with the \eqn{\mathbf{X}} matrix
#'                for each parameter, the response values (called \code{y}) and
#'                the censorship matrix (called \code{status}). See the Details
#'                section for further information.
#'        }
#' }
#'
#' @details \code{maxlogLreg} computes programmatically the log-likelihood
#' (log L) function corresponding for the following model:
#'
#' \deqn{
#'   y_i \stackrel{iid.}{\sim} \mathcal{D}(\theta_{i1},\theta_{i2},\dots,
#'   \theta_{ij}, \dots, \theta_{ik})
#' }
#' \deqn{
#'   g(\boldsymbol{\theta}_{j}) = \boldsymbol{\eta}_{j} = \mathbf{X}_j^\top
#'   \boldsymbol{\beta}_j,
#' }
#'
#' where,
#'
#' \itemize{
#'   \item \eqn{g_k(\cdot)} is the \eqn{k}-th link function.
#'   \item \eqn{\boldsymbol{\eta}_{j}} is the value of the linear predictor for the
#'         \eqn{j^{th}} for all the observations.
#'   \item \eqn{\boldsymbol{\beta}_j = (\beta_{0j}, \beta_{1j},\dots,
#'         \beta_{(p_j-1)j})^\top} are the fixed effects vector, where \eqn{p_j}
#'         is the number of parameters in linear predictor \eqn{j} and
#'         \eqn{\mathbf{X}_j} is a known design matrix of order \eqn{n\times p_j}.
#'         These terms are specified in \code{formulas} argument.
#'   \item \eqn{\mathcal{D}} is the distribution specified in argument
#'         \code{y_dist}.
#' }
#'
#' Then, \code{maxlogLreg} maximizes the log L through
#' \code{\link[stats:optim]{optim}}, \code{\link[stats:nlminb]{nlminb}} or
#' \code{\link[DEoptim:DEoptim]{DEoptim}}. \code{maxlogLreg} generates an S3 obj.
#'
#' Estimation with censorship can be handled with \code{Surv} objects
#' (see example 2). The output object stores the corresponding censorship matrix,
#' defined as \eqn{r_{il} = 1} if sample unit \eqn{i} has status \eqn{l}, or
#' \eqn{r_{il} = 0} in other case. \eqn{i=1,2,\dots,n} and \eqn{l=1,2,3}
#' (\eqn{l=1}: observation status, \eqn{l=2}: right censorship status,
#' \eqn{l=3}: left censorship status).
#'
#' @note
#' \itemize{
#'   \item The following generic functions can be used with a \code{maxlogL}
#'   object: \code{summary, print, logLik, AIC}.
#'   \item Noncentrality parameters must be named as \code{ncp} in the
#'   distribution.
#' }
#'
#' @importFrom stats nlminb optim pnorm model.frame as.formula na.omit
#' @importFrom survival Surv
#' @importFrom DEoptim DEoptim
#' @importFrom BBmisc is.error
#' @importFrom numDeriv hessian
#' @importFrom stringr str_extract
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
#' summary(norm_mod)
#'
#'
#' #--------------------------------------------------------------------------------
#' # Example 2: Fitting with censorship
#' # (data from https://www.itl.nist.gov/div898/handbook/apr/section4/apr413.htm)
#'
#' failures <- c(55, 187, 216, 240, 244, 335, 361, 373, 375, 386)
#' fails <- c(failures, rep(500, 10))
#' status <- c(rep(1, length(failures)), rep(0, 10))
#' Wei_data <- data.frame(fails = fails, status = status)
#'
#' # Formulas with linear predictors
#' formulas <- list(scale.fo=~1, shape.fo=~1)
#' support <- list(interval = c(0, Inf), type = 'continuous')
#'
#' # Bounds for optimization. Upper bound set with default values (Inf)
#' start <- list(
#'   scale = list(Intercept = 100),
#'   shape = list(Intercept = 10)
#' )
#' lower <- list(
#'   scale = list(Intercept = 0),
#'   shape = list(Intercept = 0)
#' )
#'
#' mod_weibull <- maxlogLreg(formulas, y_dist = Surv(fails, status) ~ dweibull,
#'                           support = c(0, Inf), start = start,
#'                           lower = lower, data = Wei_data)
#' summary(mod_weibull)
#'
#'
#' #--------------------------------------------------------------------------------
#'
#' @references
#' \insertRef{Nelder1965}{EstimationTools}
#'
#' \insertRef{Fox1978}{EstimationTools}
#'
#' \insertRef{Nash1979}{EstimationTools}
#'
#' \insertRef{Dennis1981}{EstimationTools}
#'
#' @importFrom Rdpack reprompt
#'
#' @seealso 
#'   \code{\link{summary.maxlogL}}, 
#'   \code{\link[stats:optim]{optim}}, 
#'   \code{\link[stats:nlminb]{nlminb}}, 
#'   \code{\link[DEoptim:DEoptim]{DEoptim}}, 
#'   \code{\link[DEoptim:DEoptim.control]{DEoptim.control}}, 
#'   \code{\link{maxlogL}}, 
#'   \code{\link{bootstrap_maxlogL}}
#'
#==============================================================================
# Maximization routine for regression -----------------------------------------
#==============================================================================
#' @export
maxlogLreg <- function(formulas, y_dist, support = NULL, data = NULL,
                       subset = NULL, fixed = NULL, link = NULL,
                       optimizer = 'nlminb', start = NULL,
                       lower = NULL, upper = NULL, inequalities = NULL,
                       control = NULL, silent = FALSE,
                       StdE_method = c('optim', 'numDeriv'), ...){

  if (silent) options(warn = -1)
  call <- match.call()

  # Distribution of response variables
  distr <- as.character(y_dist)[3]

  # Subset of data
  if ( !is.null(subset) ) data <- subset(data, eval(parse(text = subset)))

  # List of arguments of density function
  arguments <- formals(distr)

  # Common errors
  if ( !is.list(control) ){
    if ( !is.null(control) ){
      stop("'control' argument must be a list \n \n")
    }
  }

  if ( is.null(y_dist) ) stop(paste0("'y_dist' argument is NULL: ",
                                     "Distribution for response variable ",
                                     "must be specified \n \n"))

  if ( !inherits(y_dist, "formula") ) stop(paste0("'y_dist' argument must be ",
                                                  "a formula specifying ",
                                                  "distribution of response ",
                                                  "the variable \n \n"))

  if ( !is.null(link) ){
    if (length(match(link$over, names(arguments)) ) == 0)
      stop(paste0("Name(s) of linked parameter(s) do not agree with ",
                  "arguments of ", distr, ". \n Please, change name(s) ",
                  "specified in the entry 'over' of 'link' argument in \n",
                  " function maxlogLreg\n"))
    if ( is.null(link$over) & !is.null(link$fun) ){
      warn <- paste0("You do not specify parameters to map, ",
                     "however, you specify a link function \n ",
                     "(the entry 'over' in 'link' argument is NULL ",
                     "but the entry 'fun' is not NULL)\n")
      warning(warn)
    }
    if ( !is.null(link$over) & is.null(link$fun) )
      stop(paste0("You specify parameters to map, ",
                  "however, you do not specify a link function \n",
                  "(the entry 'fun' in 'link' argument is NULL ",
                  "but the entry 'over' is not NULL).\n "))
  }
  if ( !is.null(fixed) ){
    if( length(match(names(fixed),names(arguments))) == 0 )
      stop(paste0("Name(s) of fixed (known) parameter(s) do not agree with ",
                  "arguments of ", distr, ". \n Please, change names ",
                  "specified in argument 'fixed' in function ",
                  "maxlogLreg", "\n"))
  }
  if ( is.null(names(formulas)) ) stop(paste0("Please, specify parameters ",
                                              "formulas with the correct ",
                                              "notation"))

  # Exclusion of fixed or default (ncp) parameters from objective variables
  # class_arguments <- sapply(arguments, class)
  names_arguments <- names(arguments)
  pos_ncp <- sapply(names_arguments, function(y) grep('^ncp*.', y)[1])
  pos_ncp <- which(!is.na(pos_ncp))

  if ( length(pos_ncp) > 0 ){
    class_arguments <- sapply(arguments, class)
    num_ncp <- which((class_arguments[pos_ncp] == "numeric" |
                        class_arguments[pos_ncp] == "symbol"))
    if ( length(num_ncp) > 0 ){
      fixed[[names_arguments[pos_ncp]]] <- arguments[[pos_ncp]]
    }
  }
  names_fixed <- names(fixed)

  # Exclusion of fixed parameters from objective variables
  pos.deletion <- match(names(fixed), names(arguments))
  if ( length(pos.deletion) > 0 ) arguments <- arguments[-pos.deletion]

  # Parameters counting
  nnum <- sapply(1:length(arguments),
                 FUN = function(x) is.numeric(arguments[[x]]))
  nsym <- sapply(1:length(arguments),
                 FUN = function(x) is.symbol(arguments[[x]]))

  ## x is a symbol in distribution, must be substracted
  npar <- length(nnum[nnum == TRUE]) + length(nsym[nsym == TRUE]) - 1


  ## Names of parameters in implemented function
  num_names <- names(arguments[c(which(nsym == TRUE),
                                 which(nnum == TRUE))])
  par_names <- num_names[num_names != "x"]

  # Matching between parameters and formulas
  fo_names <- gsub(".fo", "", names(formulas))
  par_order <- match(par_names, fo_names)
  formulas <- formulas[par_order]

  ## Design matrixes
  dsgn_mat <- model_matrix_maxlogL(formulas = formulas, data = data,
                                 y_dist = y_dist, npar = npar,
                                 par_names = par_names)
  levels <- dsgn_mat$levels
  cens <- dsgn_mat$status

  ## Number of regression parameters
  n_betas <- sum(as.numeric(unlist(sapply(dsgn_mat[1:npar], ncol))))
  par_matrix <- matrix(1:npar, nrow = npar)
  b_names <- apply(par_matrix, MARGIN = 1,
                   FUN = function(x) colnames(dsgn_mat[[x]]))

  # Patch to build a list when npar == 1
  if (nrow(par_matrix) == 1){
    b_names <- list(b_names[1]) # list(b_names[,1])
  }

  names(b_names) <- par_names
  b_length <- sapply(dsgn_mat[1:npar], ncol)

  #  Feasible region
  # This implementation is useful in 'maxlogL' in future revisions
  # lower <- set_values(input = lower, n_betas = n_betas, par_names = par_names)
  # upper <- set_values(input = upper, n_betas = n_betas, par_names = par_names)
  # start <- set_values(input = start, n_betas = n_betas, par_names = par_names)
  lower <- set_values(input = lower, n_betas = n_betas, par_names = par_names,
                      b_names = b_names, npar = npar, b_length = b_length)
  upper <- set_values(input = upper, n_betas = n_betas, par_names = par_names,
                      b_names = b_names, npar = npar, b_length = b_length)
  start <- set_values(input = start, n_betas = n_betas, par_names = par_names,
                      b_names = b_names, npar = npar, b_length = b_length)

  # Optimizers
  if (is.character(optimizer)){

    solvers <- c('nlminb', 'optim', 'DEoptim')
    if ( !optimizer %in% solvers ){
      stop(c("Select optimizers from the following list: \n \n",
             "  --> ",paste0(solvers, collapse=", ")))
    }

    if ( optimizer == 'nlminb' ) {
      nlminbcontrol <- control
      fit <- nlminb(start = start, objective = minus_lL_LinReg,
                    lower = lower, upper = upper, control = nlminbcontrol, ...,
                    mat = dsgn_mat, distr = distr, dist_args = arguments,
                    over = link$over, link = link$fun, npar = npar, fixed = fixed,
                    par_names = par_names, b_length = b_length, ineqs = inequalities)
      fit$objective <- -fit$objective
    }

    if ( optimizer == 'optim' ) {
      optimcontrol <- control
      if (npar<2) fit <- optim(par = start, fn = minus_lL_LinReg, lower = lower,
                               upper=upper,mat = dsgn_mat, distr = distr,
                               dist_args = arguments, over = link$over,
                               link = link$fun, npar = npar, fixed = fixed,
                               par_names = par_names, b_length = b_length)
      fit <- optim(par = start,fn = minus_lL_LinReg, control = optimcontrol, ...,
                   mat = dsgn_mat, distr = distr, dist_args = arguments,
                   over = link$over, link = link$fun, npar = npar, fixed = fixed,
                   par_names = par_names, b_length = b_length)
      fit$objective <- -fit$value
    }

    if ( optimizer == 'DEoptim' ) {
      if (is.null(lower) | is.null(upper)) stop("'lower' and 'upper'
                                                 limits must be defined
                                                 for 'DEoptim' optimizer", "\n\n")
      DEoptimcontrol <- c(trace = FALSE, control)
      trace_arg <- which(names(DEoptimcontrol) == "trace")
      if (length(trace_arg) > 1){
        if (length(trace_arg) == 2){
          DEoptimcontrol$trace <- NULL
        } else {
          warn <-"Argument 'trace' in 'DEoptim.control' has multiple definitions \n"
          warning(warn)
        }
      }
      fit <- DEoptim(fn = minus_lL_LinReg, lower = lower, upper = upper,
                     control = DEoptimcontrol, ..., mat = dsgn_mat,
                     distr = distr, dist_args = arguments, over = link$over,
                     link = link$fun, npar = npar, fixed = fixed,
                     par_names = par_names, b_length = b_length)
      fit$par <- as.numeric(fit$optim$bestmem)
      fit$objective <- -fit$optim$bestval
    }
  }

  if (is.optimizer.config(optimizer)){
    objective_fun <- list(minus_lL_LinReg)
    names(objective_fun) <- optimizer$objective_name

    bounds_arguments <- list(lower, upper, start)
    names(bounds_arguments) <- c(
      optimizer$bounds_arguments$lower_name,
      optimizer$bounds_arguments$upper_name,
      optimizer$bounds_arguments$start_name
    )
    args_LL <- list(
      mat = dsgn_mat, distr = distr, dist_args = arguments,
      over = link$over, link = link$fun, npar = npar, fixed = fixed,
      par_names = par_names, b_length = b_length
    )

    fit <- do.call(
      what = optimizer$optimizer_name,
      args = c(
        objective_fun,
        optimizer$further_args,
        bounds_arguments,
        args_LL
      )
    )
    fit$par <- fit[[optimizer$outputs_names$optim_vals]]
    fit$objective <- fit[[optimizer$outputs_names$objective_vals]]
  }

  ## Regression parameters names
  # regpar_names <- matrix(sapply(dsgn_mat[1:npar], function(x) colnames(x)),
  #                        nrow = 1)
  regpar_names <- as.character(unlist(sapply(dsgn_mat[1:npar],
                                             function(x) colnames(x))))
  names(fit$par) <- regpar_names

  # Hessian computation
  StdE_method <- match.arg(StdE_method, c('optim', 'numDeriv'))
  if ( StdE_method == 'optim' ){
    fit$hessian <- try(optim(par = fit$par, fn = minus_lL_LinReg,
                             method = 'L-BFGS-B',
                             lower = fit$par - 0.5*fit$par,
                             upper = fit$par + 0.5*fit$par,
                             hessian = TRUE, mat = dsgn_mat, distr = distr,
                             dist_args = arguments, over = link$over,
                             link = link$fun, npar = npar, fixed = fixed,
                             par_names = par_names, b_length = b_length)$hessian,
                       silent = TRUE)
    StdE_computation <- "Hessian from optim"
  }
  if ( (any(is.na(fit$hessian)) | is.error(fit$hessian)) |
       any(is.character(fit$hessian)) | StdE_method == 'numDeriv' ){
    fit$hessian <- try(numDeriv::hessian(minus_lL_LinReg, fit$par,
                                         mat = dsgn_mat, distr = distr,
                                         dist_args = arguments,
                                         over = link$over, link = link$fun,
                                         npar = npar, fixed = fixed,
                                         par_names = par_names,
                                         b_length = b_length), silent = TRUE)
    StdE_computation <- "numDeriv::hessian"
  }
  if ( (any(is.na(fit$hessian)) | is.error(fit$hessian)) |
       any(is.character(fit$hessian)) ){
    StdE_computation <- paste0("'", StdE_method, "' failed")
    fit$hessian <- NA
    fit$StdE <- NA
  } else {
    fit$StdE <- sqrt(diag(solve(fit$hessian)))
  }

  # Linear predictors computation
  betas_list <- all_betas(b_length = b_length, npar = npar,
                          param = fit$par)
  linear.predictors <- lapply(X = 1:npar, FUN = LinPred,
                              betas = betas_list, mat = dsgn_mat[1:npar])
  names(linear.predictors) <- par_names

  # Fitted values computation
  fitted.values <- link_apply(values = linear.predictors, over = link$over,
                              dist_args = arguments, npar = npar,
                              link_fun = link$fun)
  names(fitted.values) <- par_names
  fitted.values <- lapply(fitted.values, function(x) as.numeric(x))

  # Coefficients of predictors
  A <- param_index(b_length, npar)
  coefficients <- lapply(1:length(par_names), function(i) fit$par[A[i,1]:A[i,2]])
  names(coefficients) <- par_names

  # fit stores the following information:
  # fit <- list(par, objective, hessian, StdE)
  inputs <- list(call = call, distr = distr, y_dist = y_dist, support = support,
                 formulas = formulas, fixed = fixed, link = link, cens = cens,
                 start = start, lower = lower, upper = upper,
                 optimizer = optimizer, data = dsgn_mat$data)
  outputs <- list(npar = npar, n = length(dsgn_mat$y),
                  StdE_Method = StdE_computation, type = "maxlogLreg",
                  b_length = b_length, levels = levels,
                  par_names = par_names, response = dsgn_mat$y,
                  design_matrix = dsgn_mat,
                  linear.predictors = linear.predictors,
                  fitted.values = fitted.values, coef = coefficients)
  result <- list(fit = fit, inputs = inputs, outputs = outputs)
  class(result) <- "maxlogL"
  if (silent) options(warn = 0)
  return(result)
}

# set_values <- function(input, n_betas, par_names, par_order){
#   call <- match.call()
#   limit <- call[[2]]
#   if( is.null(input) ){
#     default_bounds <- c(lower = -Inf, upper = Inf, start = 0)
#     output <- rep(x = as.numeric(default_bounds[as.character(limit)]),
#                   times = n_betas)
#   } else {
#     if ( is.null(names(input)) ) stop(paste0("Please, specify '",
#                                                    as.character(limit),
#                                                    "' values with the name",
#                                                    " of parameters"))
#     input_order <- match(par_names, names(input))
#     output <- as.numeric(input)[input_order]
#   }
#   return(output)
# }
set_values <- function(input, n_betas, par_names, par_order,
                       b_names, npar, b_length){
  call <- match.call()
  limit <- call[[2]]
  default_bounds <- c(lower = -Inf, upper = Inf, start = 0)
  if( is.null(input) ){
    output <- rep(x = as.numeric(default_bounds[as.character(limit)]),
                  times = n_betas)
  } else {
    input_names <- lapply(input, names)
    b_names <- lapply(b_names, change)

    # Check for names for all regression parameters in input
    input_names_length <- sapply(input_names, length)
    input_length <- sapply(input, length)
    if ( any(input_names_length < input_length) )
      stop(paste0("There are more names than values in'", as.character(limit),
                  "' argument. Please, specify the name of all regression parameters"))
    if ( any(input_names_length > input_length) )
      stop(paste0("There are more values than names in'", as.character(limit),
                  "' argument. Please, specify the value of all regression parameters"))

    # Reorder the parameters test[[1]] <- input[[1]][c(3,1,2)]
    input_order <- match(par_names, names(input))
    input <- input[input_order]
    reg_order <- apply(
      matrix(par_names, nrow = npar), MARGIN = 1,
      FUN = function(x) match(b_names[[x]], input_names[input_order][[x]])
    )
    na_pos <- lapply(lapply(reg_order, is.na), which)
    reg_order <- lapply(lapply(reg_order, na.omit), as.numeric)
    input <- apply(matrix(1:npar, nrow = npar), MARGIN = 1,
                   FUN = function(x) input[[x]] <- input[[x]][reg_order[[x]]])
    names(input) <- par_names
    output <- as.numeric(unlist(input))

    # Adding default values where it is needed
    if( any(unlist(sapply(na_pos, length)) == 0) ){
      A <- param_index(b_length, npar)
      A <- A - sum(sapply(na_pos, length))
      A[A==0] <- 1
      for (i in 1:npar){
        newpos <- A[i,1] + na_pos[[i]] - 2
        if ( length(newpos) == 0 ) next
        output <- append(output,
                         as.numeric(default_bounds[as.character(limit)]),
                         after = newpos)
      }
    }
  }
  return(output)
}
change <- function(x){
  x[x == "(Intercept)"] <- "Intercept"
  return(x)
}
#==============================================================================
# Design matrix composition ---------------------------------------------------
#==============================================================================
#' @export
#' @method model.matrix maxlogL
#' @importFrom stats model.matrix
model.matrix.maxlogL <- function(object, ...) {
  # Expect object to contain what the helper needs:
  # object$formulas, object$data, object$y_dist, object$npar, object$par_names
  model_matrix_maxlogL(
    formulas  = object$formulas,
    data      = object$data,
    y_dist    = object$y_dist,
    npar      = object$npar,
    par_names = object$par_names
  )
}
#' @keywords internal
#' @noRd
model_matrix_maxlogL <- function(formulas, data, y_dist, npar, par_names){
  if ( !any(lapply(formulas, class) == "formula") ){
    stop("All elements in argument 'formulas' must be of class formula")
  }

  # Number of formulas (one formula for each parameter)
  nfos <- length(formulas)

  if (nfos != npar) stop(paste0("Distribution defined for response ",
                                "variable has ", npar, " parameters. ",
                                "Each parameter must have its own formula"))

  # Response variable
  if ( !inherits(y_dist, "formula") ) stop(paste0("Expression in 'y_dist' ",
                                                  "must be of class 'formula"))
  if ( length(y_dist) != 3 ) stop(paste0("Expression in 'y_dist' ",
                                         "must be a formula of the form ",
                                         "'response ~ distribution' or ",
                                         "'Surv(response, status) ~ distribution'"))

  Y <- all.vars(y_dist)[1] #Surv_transform(y_dist = y_dist)

  # Extract the right side of formulas
  formulas_corrector <- stringr::str_extract(as.character(formulas), "~.+")
  formulas_tmp <- as.list(formulas_corrector)
  names(formulas_tmp) <- par_names

  # Variables
  fos_mat_char <- lapply(formulas_tmp, fos_bind, response = Y)
  fos_mat <- lapply(fos_mat_char, as.formula)
  list_mfs <- lapply(fos_mat, model.frame, data = data)
  if ( is.null(data) ){
    data_temp <- as.data.frame(list_mfs)
    var_names <- as.character(unlist(sapply(list_mfs, names)))
    names(data_temp) <- var_names
    col_names <- unique(var_names)
    data_temp <- as.data.frame(data_temp[, col_names])
    names(data_temp) <- col_names
    data <- data_temp
  }

  levels <- NULL
  original_data <- data
  data_reg <- data
  response <- model.frame(fos_mat[[1]], data = data)[, 1]
  if ( is.character(response) | is.factor(response) ){
    eval(substitute(data$res <- as.integer(data$res) - 1, list(res = Y)))
    levels <- levels(response)
    data_reg <- data
  }

  # Censorship status
  cens <- Surv_transform(y_dist = y_dist, data = data)

  # Formulas for 'model.frame'
  mtrxs <- lapply(X = 1:nfos, FUN = matrixes, formulas = fos_mat,
                  model_frames = list_mfs)

  names(mtrxs) <- names(fos_mat)
  mtrxs$y <- response
  mtrxs$status <- cens[,2:ncol(cens)]
  mtrxs$data_reg <- data_reg
  mtrxs$data <- original_data
  mtrxs$levels <- levels
  return(mtrxs)
}
fos_bind <- function(formula, response){
  paste(response, paste(formula, collapse = " "))
}
matrixes <- function(j, formulas, model_frames){
  do.call(what = "model.matrix",
          args = list(object = as.formula(formulas[[j]]),
                      data = model_frames[[j]]))
}
#==============================================================================
# Response variable evaluation ------------------------------------------------
#==============================================================================
Surv_transform <- function(y_dist, data){
  SurvObject <- with(data, eval(y_dist[[2]]))
  if ( inherits(SurvObject, "Surv") ){
    if ( ncol(SurvObject) == 3 ){
      stop("Estimation for interval censored data no available \n\n")
    }
    if ( ncol(SurvObject) == 2 ){
      y <- SurvObject[,1]
      obs   <- ifelse(SurvObject[,2] == 1, 1, 0)
      left  <- ifelse(SurvObject[,2] == 2, 1, 0)
      right <- ifelse(SurvObject[,2] == 0, 1, 0)
      # yvar <- all.vars(y_dist)[1]
    }
  } else if ( is.numeric(SurvObject) ){
    y <- SurvObject
    obs <- rep(1, length(y))
    left <- right <- rep(0, length(y))
    # yvar <- all.vars(y_dist)[1]
  } else {
    stop("Response variable must be of class, 'numeric', 'factor' or a Surv' object")
  }
  status <- c(obs, left, right)
  cens_data <- matrix(c(y, status), nrow = length(y))
  return(cens = cens_data)
}
#==============================================================================
# log-likelihood function computation -----------------------------------------
#==============================================================================
minus_lL_LinReg <- function(param, mat, distr, dist_args, over, link, npar,
                            fixed, par_names, b_length, ineqs = NULL){
  # Linear predictor
  betas_list <- all_betas(b_length = b_length, npar = npar,
                          param = param)
  param <- lapply(X = 1:npar, FUN = LinPred,
                  betas = betas_list, mat = mat[1:npar])
  names(param) <- par_names
  original_scale_param <- param

  if( !is.null(link) & !is.null(over) ){
    linked_params <- link_list(over = over, dist_args = dist_args,
                               npar = npar)
    if ( !is.null(linked_params) ){
      link_eval <- vector( mode = "list", length = length(linked_params) )
      link <- paste0(link, "()")
      link_eval <- lapply( 1:length(linked_params),
                           FUN=function(x) eval(parse(text = link[x])) )
      for (i in 1:length(linked_params)){
        g_inv <- paste0("link_eval[[", i, "]]$g_inv")
        g_inv <- eval(parse(text=g_inv))
        param[[linked_params[i]]] <- do.call(
          what = "g_inv",
          args = list(eta = param[[linked_params[i]]])
        )
      }
    }
  }
  cdf  <- paste0('p', substring(distr, 2))
  y <- mat$y
  delta <- mat$status

  logf <- do.call( what = distr, args = c(list(x = y), param,
                                          log = TRUE, fixed) )

  ll <- logf*delta[, 1]

  if ( any(delta[, 2] > 0) ){
    logF <- do.call(
      what = cdf,
      args = c(
        list(q = y),
        param,
        lower.tail = TRUE,
        log.p = TRUE,
        fixed = fixed
      )
    )
    ll <- ll + logF*delta[, 2]
  }

  if ( any(delta[, 3] > 0) ) {
    logS <- do.call(
      what = cdf,
      args = c(
        list(q = y),
        param,
        lower.tail = FALSE,
        log.p = TRUE, fixed
      )
    )
    ll <- ll + logS*delta[, 3]
  }

  ll <- -sum(ll)

  if ( !is.null(ineqs) ){
    # Non-linear constrain body
    ineqs <- lapply(ineqs, BBmisc::asQuoted)
    g_ineqs <- eval( bquote( function(){do.call( "rbind", .(ineqs) )} ) )

    # Non-linear constrain parameters
    ineqs_params <- vector( mode = "list", length = length(par_names) )
    names(ineqs_params) <- par_names
    formals(g_ineqs) <- ineqs_params

    eval_g_ineqs <- do.call(
      what = "g_ineqs",
      args = original_scale_param
    )

    if ( any(!eval_g_ineqs) ){
      ll <- ll + 1e10
    }
  }

  # Negative of log-Likelihood function
  return(as.numeric(ll)) # Useful when using Rmpfr (LuA)
}
param_index <- function(b_length, npar){
  b_length_plus <- c(0, as.numeric(b_length))
  A <- matrix(0, ncol = 2, nrow = length(b_length_plus)-1)
  for ( i in 1:(length(b_length_plus)-1) ){
    j <- i + 1
    a <- sum(b_length_plus[1:i]) + 1
    b <- sum(b_length_plus[1:j])
    A[i,] <- c(a, b)
  }
  return(A)
}
all_betas <- function(b_length, npar, param){
  A <- param_index(b_length, npar)
  betas <- vector(mode = "list", length = npar)
  b_length_plus <- c(0, as.numeric(b_length))
  for ( i in 1:(length(b_length_plus)-1) ){
    betas[[i]] <- matrix(param[A[i,1]:A[i,2]], ncol = 1)
  }
  return(betas)
}
# all_betas <- function(b_length, npar, param){
# betas <- vector(mode = "list", length = npar)
# b_length_plus <- c(0, as.numeric(b_length))
# for ( i in 1:(length(b_length_plus)-1) ){
#   j <- i + 1
#   a <- sum(b_length_plus[1:i]) + 1
#   b <- sum(b_length_plus[1:j])
#   betas[[i]] <- matrix(param[a:b], ncol = 1)
# }
#   return(betas)
# }
LinPred <- function(j, betas, mat){
  mat[[j]] %*% betas[[j]]
}
# link_list <- function(over, dist_args, npar){
#   if ( is.null(over) ){
#     return(linked_params = NULL)
#   } else {
#     if ( length(over) > npar ) stop(paste0("Number of mapped parameters is ",
#                                            "greater than the number of parameters ",
#                                            "of the distribution.\n Remember, ",
#                                            "npar >= over"))
#     numeric_list <- vector(mode = "list", length = npar + 1)
#     names_numeric <- rep("", times = npar + 1)
#     j <- 1
#     for ( i in 1:length(dist_args) ){
#       if (is.numeric(dist_args[[i]]) | is.symbol(dist_args[[i]])){
#         numeric_list[[j]]<- dist_args[[i]]
#         names_numeric[j] <- names(dist_args[i])
#         j <- j + 1
#       }
#     }
#     numeric_list[which(names_numeric == "x")] <- NULL
#     names_numeric <- names_numeric[-which(names_numeric == "x")]
#     names(numeric_list) <- names_numeric
#
#     args_names <- names(dist_args)
#     mapped_param <- match(over, args_names)
#
#     linked_args <- vector(mode = "list", length = length(over))
#     names_linked <- rep("", times = length(over))
#     for ( i in 1:length(mapped_param) ){
#       linked_args[i] <- dist_args[mapped_param[i]]
#       names_linked[i] <- names(dist_args[mapped_param[i]])
#     }
#     names(linked_args) <- names_linked
#
#     linked_params <- match(names_linked, names_numeric)
#     return(linked_params)
#   }
# }
