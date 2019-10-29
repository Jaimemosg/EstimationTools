#' @title Maximum Likelihood Estimation for parametric linear regression models
#' @family maxlogL
#'
#' @description
#' Function to compute maximum likelihood estimators (MLE) of regression parameters
#' of any distribution implemented in \code{R} with covariates (linear predictors).
#'
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @param formulas a list of formula objects. Each element must have an \code{~}, with the terms
#' on the right separated by \code{+} operators. The response variable on the left side is optional.
#' Linear predictor of each parameter must be specified with the name of the parameter folowed by
#' the sufix \code{'.fo'}. See 'Details' and the examples below for further
#' illustration.
#' @param y_dist a formula object tha specifies the distribution of the response variable.
#'               On the left side of \code{~} must be the response, and in the right side
#'               must be the name o de probability density/mass function. See the section
#'               'Details' and the examples below for further illustration.
#' @param data an optional data frame containing the variables in the model. If data is not
#'             specified, the variables are taken from the environment from which
#'             \code{maxlogLreg} is called.
#' @param subset an optional vector specifying a subset of observations to be used
#'               in the fitting process.
#' @param fixed a list with fixed/known parameters of distribution of interest. Fixed parameters
#'              must be passed with its name and its value (known).
#' @param link a list with names of parameters to be linked, and names of the link object.
#'             For names of parameters, please visit documentation of density/mass function.
#'             There are three link functions available: \code{\link{log_link}},
#'             \code{\link{logit_link}} and \code{\link{NegInv_link}}.
#' @param start a numeric vector with initial values for the parameters to be estimated.
#' @param lower a numeric vector with lower bounds, with the same lenght of
#'              argument `start` (for box-constrained optimization).
#' @param upper a numeric vector with upper bounds, with the same lenght of
#'              argument `start` (for box-constrained optimization).
#' @param optimizer a lenght-one character vector with the name of optimization routine.
#'                  \code{\link{nlminb}}, \code{\link{optim}} and
#'                  \code{\link[DEoptim]{DEoptim}} are available; \code{\link{nlminb}}
#'                  is the default routine.
#' @param control control parameters of the optimization routine. Please, visit documentation of selected
#'                optimizer for further information.
#' @param ... Further arguments to be supplied to the optimizer.
#'
#' @return A list with class \code{"maxlogL"} containing the following
#'  lists:
#' \item{fit}{A list with output information about estimation and method used.}
#' \item{inputs}{A list with all input arguments.}
#' \item{outputs}{A list with additional information:
#'       \itemize{
#'          \item Number of parameters.
#'          \item Sample size
#'          \item Standard error computation method.
#'          \item Number of regression parameters.
#'        }
#' }
#'
#' @details \code{maxlogLreg} calculates computationally the log-likelihood (log L) function
#' corresponding to the distribution specified in argument \code{y_dist} with linear
#' predictors specified in argument \code{formulas}. Then, it maximizes the log L through
#' \code{\link{optim}}, \code{\link{nlminb}} or \code{\link{DEoptim}}. \code{maxlogLreg}
#' generates an S3 object of class \code{maxlogLreg}.
#'
#' @note The following generic functions can be used with a \code{maxlogL} object:
#' \code{summary, print}.
#'
#' @importFrom stats nlminb optim pnorm model.frame as.formula
#' @importFrom survival Surv
#' @importFrom DEoptim DEoptim
#' @importFrom BBmisc is.error
#' @importFrom numDeriv hessian
#' @importFrom stringr str_extract
#'
#' @examples
#' #--------------------------------------------------------------------------------
#' # Estimation in simulated normal distribution
#' n <- 1000
#' x1 <- runif(n = n, -5, 6)
#' y <- rnorm(n = n, mean = -2 + 3 * x1, sd = exp(1 + 0.3* x1))
#'
#' # It does not matter the order of the paramters
#' formulas <- list(sd.fo = ~ x1, mean.fo = ~ x1)
#'
#' library(EstimationTools)
#' norm_mod <- maxlogLreg(formulas, y_dist = y ~ dnorm,
#'                        link = list(over = "sd", fun = "log_link"))
#' summary(norm_mod)
#' #--------------------------------------------------------------------------------
#' # Fitting with censorship
#' # (data from https://www.itl.nist.gov/div898/handbook/apr/section4/apr413.htm)
#'
#' failures = c(55, 187, 216, 240, 244, 335, 361, 373, 375, 386)
#' fails <- c(failures, rep(500, 10))
#' status <- c(rep(1, length(failures)), rep(0, 10))
#' formulas <- list(scale.fo=~1, shape.fo=~1)
#'
#' mod_weibull <- maxlogLreg(formulas, y_dist = Surv(fails, status) ~ dweibull,
#'                           start = c(scale=100, shape=10),
#'                           lower = c(scale=0,shape=0))
#' summary(mod_weibull)
#' #--------------------------------------------------------------------------------
#==============================================================================
# Maximization routine for regression -----------------------------------------
#==============================================================================
#' @export
maxlogLreg <- function(formulas,
                       y_dist,
                       data = sys.parent(),
                       subset = NULL,
                       fixed = NULL,
                       link = NULL,
                       start = NULL,
                       lower = NULL,
                       upper = NULL,
                       optimizer = 'nlminb',
                       control = NULL,
                       ...){

  call <- match.call()

  # Distribution of response varaibles
  dist <- as.character(y_dist)[3]

  # Subset of data
  if ( !is.null(subset) ) data <- subset(data, eval(parse(text = subset)))

  # List of arguments of density function
  arguments <- as.list(args(dist))

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

  solvers <- c('nlminb', 'optim', 'DEoptim')
  if ( !optimizer %in% solvers ){
    stop(c("Select optimizers from the following list: \n \n",
           "  --> ",paste0(solvers, collapse=", ")))
  }

  if ( !is.null(link) ){
    if (length(match(link$over, names(arguments)) ) == 0)
      stop(paste0("Name(s) of linked parameter(s) do not agree with ",
                  "arguments of ", dist, ". \n Please, change name(s) ",
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
                  "arguments of ", dist, ". \n Please, change names ",
                  "specified in argument 'fixed' in function ",
                  "maxlogLreg", "\n"))
  }
  if ( is.null(names(formulas)) ) stop(paste0("Please, specify parameters ",
                                              "formulas with the correct ",
                                              "notation"))

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
  dsgn_mat <- model.matrix.MLreg(formulas = formulas, data = data,
                                 y_dist = y_dist, npar = npar,
                                 par_names = par_names)

  ## Number of regression parameters
  n_betas <- sum(as.numeric(unlist(sapply(dsgn_mat[1:npar], ncol))))
  b_length <- sapply(dsgn_mat[1:npar], ncol)

  #  Feasible region
  lower <- set_values(input = lower, n_betas = n_betas, par_names = par_names)
  upper <- set_values(input = upper, n_betas = n_betas, par_names = par_names)
  start <- set_values(input = start, n_betas = n_betas, par_names = par_names)

  # Optimizers
  if ( optimizer == 'nlminb' ) {
    nlminbcontrol <- control
    fit <- nlminb(start = start, objective = minus_lL_LinReg,
                  lower = lower, upper = upper, control = nlminbcontrol, ...,
                  mat = dsgn_mat, dist = dist, dist_args = arguments,
                  over = link$over, link = link$fun, npar = npar, fixed = fixed,
                  par_names = par_names, b_length = b_length)
    fit$objective <- -fit$objective
  }

  if ( optimizer == 'optim' ) {
    optimcontrol <- control
    if (npar<2) fit <- optim(par = start, fn = minus_lL_LinReg, lower = lower,
                             upper=upper,mat = dsgn_mat, dist = dist,
                             dist_args = arguments, over = link$over,
                             link = link$fun, npar = npar, fixed = fixed,
                             par_names = par_names, b_length = b_length)
    fit <- optim(par = start,fn = minus_lL_LinReg, control = optimcontrol, ...,
                 mat = dsgn_mat, dist = dist, dist_args = arguments,
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
                   dist = dist, dist_args = arguments, over = link$over,
                   link = link$fun, npar = npar, fixed = fixed,
                   par_names = par_names, b_length = b_length)
    fit$par <- fit$optim$bestmem
    fit$objective <- -fit$optim$bestval
  }

  ## Regression parameters names
  # regpar_names <- matrix(sapply(dsgn_mat[1:npar], function(x) colnames(x)),
  #                        nrow = 1)
  regpar_names <- as.character(unlist(sapply(dsgn_mat[1:npar],
                                             function(x) colnames(x))))
  names(fit$par) <- regpar_names

  # Hessian computation
  fit$hessian <- try(optim(par = fit$par, fn = minus_lL_LinReg,
                           method = 'L-BFGS-B',
                           lower = fit$par - 0.5*fit$par,
                           upper = fit$par + 0.5*fit$par,
                           hessian = TRUE, mat = dsgn_mat, dist = dist,
                           dist_args = arguments, over = link$over,
                           link = link$fun, npar = npar, fixed = fixed,
                           par_names = par_names, b_length = b_length)$hessian,
                     silent = TRUE,)

  StdE_Method <- "Hessian from optim"
  if ( (any(is.na(fit$hessian)) | is.error(fit$hessian)) |
       any(is.character(fit$hessian)) ){
    fit$hessian <- try(numDeriv::hessian(minus_lL_LinReg, fit$par,
                                         mat = dsgn_mat, dist = dist,
                                         dist_args = arguments,
                                         over = link$over, link = link$fun,
                                         npar = npar, fixed = fixed,
                                         par_names = par_names,
                                         b_length = b_length), silent = TRUE)
    StdE_Method <- "numDeriv::hessian"
  }
  if ( (any(is.na(fit$hessian)) | is.error(fit$hessian)) |
       any(is.character(fit$hessian)) ) fit$hessian <- NA

  inputs <- list(call = call, dist = dist, y_dist = y_dist,
                 formulas = formulas, fixed = fixed, link = link,
                 start = start, lower = lower, upper = upper,
                 optimizer = optimizer, data = dsgn_mat$data_reg)
  outputs <- list(npar = npar - length(fixed), n = length(dsgn_mat$y),
                  StdE_Method = StdE_Method, type = "maxlogLreg",
                  StdE = "Not computed yet", b_length = b_length,
                  par_names = par_names)
  result <- list(fit = fit, inputs = inputs, outputs = outputs)
  class(result) <- "maxlogL"
  return(result)
}

set_values <- function(input, n_betas, par_names, par_order){
  call <- match.call()
  limit <- call[[2]]
  if( is.null(input) ){
    default_bounds <- c(lower = -Inf, upper = Inf, start = 0)
    output <- rep(x = as.numeric(default_bounds[as.character(limit)]),
                  times = n_betas)
  } else {
    if ( is.null(names(input)) ) stop(paste0("Please, specify '",
                                                   as.character(limit),
                                                   "' values with the name",
                                                   " of parameters"))
    input_order <- match(par_names, names(input))
    output <- as.numeric(input)[input_order]
  }
  return(output)
}
#==============================================================================
# Design matrix composition ---------------------------------------------------
#==============================================================================
model.matrix.MLreg <- function(formulas, data, y_dist, npar, par_names){
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
                                         "'y ~ dist' or ",
                                         "'Surv(time, status) ~ dist'"))
  Y <- Surv_transform(y_dist = y_dist)

  # Extract the right side of formulas
  formulas_corrector <- stringr::str_extract(as.character(formulas), "~.+")
  formulas_tmp <- as.list(formulas_corrector)
  names(formulas_tmp) <- par_names

  # Variables
  fos_mat <- lapply(formulas_tmp, fos_bind, response = Y$resp)
  mf <- lapply(fos_mat, model.frame)
  data_reg <- as.data.frame(mf)
  var_names <- as.character(sapply(mf, names))
  names(data_reg) <- var_names
  data_reg <- as.data.frame(data_reg[,unique(var_names)])
  names(data_reg) <- unique(var_names)
  response <- model.frame(fos_mat[[1]], data = data)[, 1]
  # response <- data_reg[, 1]

  # Formulas for 'model.frame'
  list_mfs <- lapply(fos_mat, model.frame, data = data)
  mtrxs <- lapply(X = 1:nfos, FUN = matrixes,
                  formulas = fos_mat, model_frames = list_mfs)

  names(mtrxs) <- names(fos_mat)
  mtrxs$y <- response
  mtrxs$status <- Y$cens[,2:ncol(Y$cens)]
  mtrxs$data_reg <- data_reg
  return(mtrxs)
}
fos_bind <- function(formula, response){
  paste(response, paste(formula, collapse = " "))
}
matrixes <- function(j, formulas, model_frames){
  do.call(what = "model.matrix",
          args = list(as.formula(formulas[[j]]), model_frames[j]))
}
#==============================================================================
# Response variable evaluation ------------------------------------------------
#==============================================================================
Surv_transform <- function(y_dist){
  SurvObject <- eval(y_dist[[2]])
  if ( inherits(SurvObject, "Surv") ){
    if ( ncol(SurvObject) == 3 ){
      stop("Estimation for interval censored data no available \n\n")
    }
    if ( ncol(SurvObject) == 2 ){
      y <- SurvObject[,1]
      obs   <- ifelse(SurvObject[,2] == 1, 1, 0)
      left  <- ifelse(SurvObject[,2] == 2, 1, 0)
      right <- ifelse(SurvObject[,2] == 0, 1, 0)
      yvar <- all.vars(y_dist)[1]
    }
  } else if ( class(SurvObject) == "numeric" ){
    y <- SurvObject
    obs <- rep(1, length(y))
    left <- right <- rep(0, length(y))
    yvar <- all.vars(y_dist)[1]
  } else {
    stop("Response variable must be of class 'numeric' or a 'Surv' object")
  }
  status <- c(obs, left, right)
  cens_data <- matrix(c(y,status), nrow = length(y))
  return(list(cens = cens_data, resp = yvar))
}
#==============================================================================
# log-likelihood function computation -----------------------------------------
#==============================================================================
minus_lL_LinReg <- function(param, mat, dist, dist_args, over, link, npar,
                            fixed, par_names, b_length){
    # Linear predictor
    betas_list <- all_betas(b_length = b_length, npar = npar,
                            param = param)
    param <- lapply(X = 1:npar, FUN = LinPred,
                    betas = betas_list, mat = mat[1:npar])
    names(param) <- par_names

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
          param[[linked_params[i]]] <- do.call( what = "g_inv",
                                                args = list(x = param[[linked_params[i]]]) )
        }
      }
    }
    y <- mat$y
    delta <- mat$status
    cdf  <- gsub("d","p", dist)

    logf <- do.call( what = dist, args = c(list(x = y), param,
                                           log = TRUE, fixed) )
    logS <- do.call( what = cdf, args = c(list(q = y), param,
                                          lower.tail = FALSE,
                                          log.p = TRUE, fixed) )
    logF <- do.call( what = cdf, args = c(list(q = y), param,
                                          log.p = TRUE, fixed) )
    l <- sum( logf*delta[,1] + logF*delta[,2] + logS*delta[,3] )
    # Negative of log-Likelihood function
    return(-l)
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
link_list <- function(over, dist_args, npar){
  if ( is.null(over) ){
    return(linked_params = NULL)
  } else {
    if ( length(over) > npar ) stop(paste0("Number of mapped parameters is ",
                                           "greater than the number of parameters ",
                                           "of the distribution.\n Remember, ",
                                           "npar >= over"))
    numeric_list <- vector(mode = "list", length = npar + 1)
    names_numeric <- rep("", times = npar + 1)
    j <- 1
    for ( i in 1:length(dist_args) ){
      if (is.numeric(dist_args[[i]]) | is.symbol(dist_args[[i]])){
        numeric_list[[j]]<- dist_args[[i]]
        names_numeric[j] <- names(dist_args[i])
        j <- j + 1
      }
    }
    numeric_list[which(names_numeric == "x")] <- NULL
    names_numeric <- names_numeric[-which(names_numeric == "x")]
    names(numeric_list) <- names_numeric

    args_names <- names(dist_args)
    mapped_param <- match(over, args_names)

    linked_args <- vector(mode = "list", length = length(over))
    names_linked <- rep("", times = length(over))
    for ( i in 1:length(mapped_param) ){
      linked_args[i] <- dist_args[mapped_param[i]]
      names_linked[i] <- names(dist_args[mapped_param[i]])
    }
    names(linked_args) <- names_linked

    linked_params <- match(names_linked, names_numeric)
    return(linked_params)
  }
}
