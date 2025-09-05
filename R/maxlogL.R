#' @title Maximum Likelihood Estimation for parametric distributions
#' @family maxlogL
#'
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Wrapper function to compute maximum likelihood estimators (MLE)
#' of any distribution implemented in \code{R}.
#'
#' @param x a vector with data to be fitted. This argument must be a matrix
#'          with hierarchical distributions.
#' @param dist a length-one character vector with the name of density/mass function
#'             of interest. The default value is \code{'dnorm'}, to compute maximum
#'             likelihood estimators of normal distribution.
#' @param fixed a list with fixed/known parameters of distribution of interest.
#'              Fixed parameters must be passed with its name.
#' @param link a list with names of parameters to be linked, and names of the link
#'             function object. For names of parameters, please visit documentation
#'             of density/mass function. There are three link functions available:
#'             \code{\link{log_link}}, \code{\link{logit_link}} and
#'             \code{\link{NegInv_link}}.
#' @param start a numeric vector with initial values for the parameters to be estimated.
#' @param lower a numeric vector with lower bounds, with the same length of argument
#'              `start` (for box-constrained optimization).
#' @param upper a numeric vector with upper bounds, with the same length of argument
#'              `start` (for box-constrained optimization).
#' @param optimizer a length-one character vector with the name of optimization routine.
#'                  \code{\link[stats:nlminb]{nlminb}}, \code{\link[stats:optim]{optim}},
#'                  \code{\link[DEoptim:DEoptim]{DEoptim}} and \code{\link[GA:ga]{ga}} are available;
#'                  custom optimization routines can also be implemented.
#'                  \code{\link[stats:nlminb]{nlminb}} is the default routine.
#'
#' @param control control parameters of the optimization routine. See, e.g.,
#'                \code{\link[stats:optim]{optim}}'s \code{control} list,
#'                \code{\link[stats:nlminb]{nlminb}}'s \code{control} list, and
#'                \code{\link[DEoptim:DEoptim.control]{DEoptim.control}} for DEoptim.
#' @param StdE_method a length-one character vector with the routine for Hessian matrix
#'                    computation. The This is needed for standard error estimation. The
#'                    options available are \code{"optim"} and \code{"numDeriv"}. For
#'                    further information, visit \code{\link[stats]{optim}} or
#'                    \code{\link[numDeriv]{hessian}}.
#' @param silent  logical. If TRUE, warnings of \code{maxlogL} are suppressed.
#' @param ... further arguments to be supplied to the optimizer.
#'
#' @return A list with class \code{"maxlogL"} containing the following lists:
#' \item{fit}{A list with output information about estimation.}
#' \item{inputs}{A list with all input arguments.}
#' \item{outputs}{A list with some output additional information:
#'       \itemize{
#'          \item Number of parameters.
#'          \item Sample size
#'          \item Standard error computation method.
#'        }
#' }
#'
#' @details \code{maxlogL} computes the likelihood function corresponding to
#' the distribution specified in argument \code{dist} and maximizes it through
#' \code{\link[stats:optim]{optim}}, \code{\link[stats:nlminb]{nlminb}} or
#' \code{\link[DEoptim:DEoptim]{DEoptim}}. \code{maxlogL}
#' generates an S3 object of class \code{maxlogL}.
#'
#' Noncentrality parameters must be named as \code{ncp} in the distribution.
#'
#' @note The following generic functions can be used with a \code{maxlogL} object:
#' \code{summary, print, AIC, BIC, logLik}.
#'
#' @importFrom stats nlminb optim
#' @importFrom DEoptim DEoptim
#' @importFrom BBmisc is.error
#' @importFrom numDeriv hessian
#' @importFrom GA ga
#'
#' @examples
#' library(EstimationTools)
#'
#' #----------------------------------------------------------------------------
#' # Example 1: estimation with one fixed parameter
#' x <- rnorm(n = 10000, mean = 160, sd = 6)
#' theta_1 <- maxlogL(x = x, dist = 'dnorm', control = list(trace = 1),
#'                  link = list(over = "sd", fun = "log_link"),
#'                  fixed = list(mean = 160))
#' summary(theta_1)
#'
#'
#' #----------------------------------------------------------------------------
#' # Example 2: both parameters of normal distribution mapped with logarithmic
#' # function
#' theta_2 <- maxlogL(x = x, dist = "dnorm",
#'                    link = list(over = c("mean","sd"),
#'                                fun = c("log_link","log_link")))
#' summary(theta_2)
#'
#'
#' #--------------------------------------------------------------------------------
#' # Example 3: parameter estimation in ZIP distribution
#' if (!require('gamlss.dist')) install.packages('gamlss.dist')
#' library(gamlss.dist)
#' z <- rZIP(n=1000, mu=6, sigma=0.08)
#' theta_3  <- maxlogL(x = z, dist = 'dZIP', start = c(0, 0),
#'                    lower = c(-Inf, -Inf), upper = c(Inf, Inf),
#'                    optimizer = 'optim',
#'                    link = list(over=c("mu", "sigma"),
#'                    fun = c("log_link", "logit_link")))
#' summary(theta_3)
#'
#'
#' #--------------------------------------------------------------------------------
#' # Example 4: parameter estimation with fixed noncentrality parameter.
#' y_2 <- rbeta(n = 1000, shape1 = 2, shape2 = 3)
#' theta_41 <- maxlogL(x = y_2, dist = "dbeta",
#'                     link = list(over = c("shape1", "shape2"),
#'                     fun = c("log_link","log_link")))
#' summary(theta_41)
#'
#' # It is also possible define 'ncp' as fixed parameter
#' theta_42 <- maxlogL(x = y_2, dist = "dbeta", fixed = list(ncp = 0),
#'                     link = list(over = c("shape1", "shape2"),
#'                     fun = c("log_link","log_link")) )
#' summary(theta_42)
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
#' @seealso \code{\link{summary.maxlogL}}, \code{\link{optim}}, \code{\link{nlminb}},
#'          \code{\link[DEoptim]{DEoptim}}, \code{\link[DEoptim]{DEoptim.control}},
#'          \code{\link{maxlogLreg}}, \code{\link{bootstrap_maxlogL}}
#'
#==============================================================================
# Maximization routine --------------------------------------------------------
#==============================================================================
#' @export
maxlogL <- function(x, dist = 'dnorm', fixed = NULL, link = NULL,
                    start = NULL, lower = NULL, upper = NULL,
                    optimizer = 'nlminb', control = NULL,
                    StdE_method = c('optim', 'numDeriv'),
                    silent = FALSE, ...){

  if (silent) options(warn = -1)
  call <- match.call()

  # List of arguments of density function
  arguments <- formals(dist)

  # Common errors
  if ( !is.list(control) ){
    if (!is.null(control)){
      stop("control argument must be a list \n \n")
    }
  }

  if ( is.null(dist) ) stop("Distribution not specified \n \n")

  if ( !is.character(dist) ) stop(paste0("'dist' argument must be a character ",
                                         "string \n \n"))

  solvers <- unique(c('nlminb', 'optim', 'DEoptim', 'ga', optimizer))
  solvers <- match.arg(optimizer, solvers)
  # if ( !optimizer %in% solvers ){
  #   stop(c("Select optimizers from the following list: \n \n",
  #          "  --> ",paste0(solvers, collapse=", ")))
  # }

  if ( !is.null(link) ){
    if (length(match(link$over, names(arguments)) ) == 0)
      stop(paste0("Name(s) of linked parameter(s) do not agree with ",
                  "arguments of ", dist, ". \n Please, change name(s) ",
                  "specified in the entry 'over' of 'link' argument in \n",
                  " function maxlogL.\n"))
    if ( is.null(link$over) & !is.null(link$fun) ){
      warn <- paste0("You do not specify parameters to map, ",
                     "however, you specify a link function \n ",
                     "(the entry 'over' in 'link' argument is NULL ",
                     "but the entry 'fun' is not NULL).\n")
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
                  "maxlogL", "\n"))
  }
  if ( length(x) == 0 | is.null(x) ){
    stop(paste0("Vector of data is needed to perform maximum likelihood ",
                "estimation. \n Please, specify the vector x in maxlogL ",
                "function. \n"))
  }

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
    # if ( !pos_and_fix & class_ncp == "numeric" ){
    # if ( class_ncp == "numeric" ){
    #   fixed[[names_arguments[pos_ncp]]] <- arguments[[pos_ncp]]
    #   names_fixed <- names(fixed)
    # }

  pos.deletion <- match(names_fixed, names_arguments)
  if ( length(pos.deletion) > 0 ) arguments <- arguments[-pos.deletion]

  # Parameters counting
  nnum <- sapply(1:length(arguments),
                 FUN = function(x) is.numeric(arguments[[x]]))
  nsym <- sapply(1:length(arguments),
                 FUN = function(x) is.symbol(arguments[[x]]))

  # x is a symbol, must be substracted
  npar <- length(nnum[nnum == TRUE]) + length(nsym[nsym == TRUE]) - 1

  #  Negative of log-Likelihood function
  ll <- minus_lL(x = x, dist, dist_args = arguments, over = link$over,
                 link = link$fun, npar = npar, fixed = fixed)

  #  Default feasible region
  if ( is.null(lower) ) lower <- rep(x = -Inf, times = npar)
  if ( is.null(upper) ) upper <- rep(x = Inf, times = npar)
  if ( is.null(start) ) start <- rep(x = 0, times = npar)

  # Link application over initial values
  if ( !is.null(lower) & !is.null(upper) & !is.null(start)){
    start <- link_apply(values = start, over = link$over,
                        dist_args = arguments, npar = npar,
                        link_fun = link$fun)
  }

  # Optimizers
  fit <- NULL
  if ( optimizer == 'nlminb' ) {
    nlminbcontrol <- control
    nlminb_fit <- nlminb(start = start, objective = ll,
                         lower = lower, upper = upper, control = nlminbcontrol,
                         ...)
    fit$par <- nlminb_fit$par
    fit$objective <- -nlminb_fit$objective
  } else if ( optimizer == 'optim' ) {
    optimcontrol <- control
    if (npar<2) optim_fit <- optim(par = start, fn = ll, lower = lower,
                                   upper=upper)
    optim_fit <- optim(par = start,fn = ll, control = optimcontrol, ...)
    fit$par <- optim_fit$par
    fit$objective <- -optim_fit$value
  } else if ( optimizer == 'DEoptim' ) {
    if (is.null(lower) | is.null(upper)) stop("'lower' and 'upper'
                                               limits must be defined
                                               for 'DEoptim' optimizer",
                                              "\n\n")
    DEoptimcontrol <- c(control, trace = FALSE)
    trace_arg <- which(names(DEoptimcontrol) == "trace")
    if (length(trace_arg) > 1){
      control_index <- switch(as.character(call$control[[1]]),
                              list = trace_arg[2])
      DEoptimcontrol[[control_index]] <- NULL
    }
    # if (length(trace_arg) > 1){
    #   if (length(trace_arg) == 2){
    #     DEoptimcontrol$trace <- NULL
    #   } else {
    #     warn <-"Argument 'trace' in 'DEoptim.control' has multiple definitions \n"
    #     warning(warn)
    #   }
    # }
    DE_fit <- DEoptim(fn = ll, lower = lower, upper = upper,
                      control = DEoptimcontrol, ...)
    fit$original_fit <- DE_fit
    fit$par <- as.numeric(DE_fit$optim$bestmem)
    fit$objective <- -DE_fit$optim$bestval
  } else if ( optimizer == 'ga' ) {
    if (is.null(lower) | is.null(upper)) stop("'lower' and 'upper'
                                               limits must be defined
                                               for 'GA::ga' optimizer", "\n\n")
    plusll <- function(param) -ll(param)
    dots <- substitute(...())
    dots <- c(monitor = FALSE, control, dots)
    trace_arg <- which(names(dots) == "monitor")
    if (length(trace_arg) > 1){
      dots$monitor <- NULL
    }
    ga_fit <- do.call(optimizer, c(list(type = "real-valued", fitness = plusll,
                                        lower = lower, upper = upper), dots))
    fit$original_fit <- ga_fit
    fit$par <- as.numeric(ga_fit@solution)
    fit$objective <- ga_fit@fitnessValue
  } else {
    fit <- do.call(optimizer, c(list(fn, lower, upper, start, control, ...)))
    # pendiente la lista de outputs de un optimizador S3
  }

  # Revert link mapping
  fit$par <- link_apply(values = fit$par, over = link$over,
                        dist_args = arguments, npar = npar,
                        link_fun = link$fun)

  # Hessian computation
  StdE_method <- match.arg(StdE_method, c('optim', 'numDeriv'))

  ll.noLink <- try(
    minus_lL(
      x = x, dist,
      dist_args = arguments,
      over = NULL,
      link = NULL,
      npar = npar,
      fixed = fixed
    ),
    silent = TRUE
  )

  if ( StdE_method == 'optim' ){
    # We could also try with optimHess
    fit$hessian <- try(
      optim(par = fit$par,
            fn = ll.noLink,
            method = 'L-BFGS-B',
            lower = fit$par - 0.5*fit$par,
            upper = fit$par + 0.5*fit$par,
            hessian = TRUE)$hessian,
      silent = TRUE
    )
    StdE_computation <- "Hessian from optim"
  }
  if (
    any(is.na(fit$hessian) |
    is.error(fit$hessian)) |
    any(is.character(fit$hessian)) |
    StdE_method == 'numDeriv'
  ){
    fit$hessian <- try(numDeriv::hessian(ll.noLink, fit$par), silent = TRUE)
    StdE_computation <- "numDeriv::hessian"
  }

  ## Standard error computation
  if (
    any(is.na(fit$hessian) |
    is.error(fit$hessian)) |
    any(is.character(fit$hessian))
  ){
    StdE_computation <- paste0("'", StdE_method, "' failed")
    fit$hessian <- NA
    fit$StdE <- NA
  } else {
    fit$StdE <- sqrt(diag(solve(fit$hessian)))
  }

  ## Parameter names
  names_numeric <- rep("", times = npar)
  j <- 1
  for (i in 1:length(arguments)){
    if (
      is.numeric(arguments[[i]]) ||
      is.symbol(arguments[[i]])
    ){
      names_numeric[j] <- names(arguments[i])
      j <- j + 1
    }
  }
  names_numeric <- names_numeric[-which(names_numeric == "x")]
  names(fit$par) <- names_numeric

  # fit stores the following information:
  # fit <- list(par, objective, hessian, StdE)
  inputs <- list(call = call, dist = dist, fixed = fixed,
                 link = link, optimizer = optimizer,
                 start = start, lower = lower, upper = upper,
                 data = x)
  outputs <- list(npar = npar - length(fixed), n = length(x),
                  StdE_Method = StdE_computation, type = "maxlogL",
                  par_names = names_numeric)
  result <- list(fit = fit, inputs = inputs, outputs = outputs)
  class(result) <- "maxlogL"
  if (silent) options(warn = 0)
  return(result)
}
#==============================================================================
# Link list generation --------------------------------------------------------
#==============================================================================
link_list <- function(over, dist_args, npar){
  if ( is.null(over) ){
    return(linked_params = NULL)
  } else {
  if ( length(over) > npar ) stop(paste0("Number of mapped parameters is ",
                                  "greater than the number of parameters ",
                                  "of the distribution.\n Remember, ",
                                  "npar > over"))
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
#==============================================================================
# Link application ------------------------------------------------------------
#==============================================================================
link_apply <- function(values, over, dist_args, npar, link_fun){
  if ( !is.null(over) & !is.null(link_fun) ){
    linked_parlist <- link_list(over = over, dist_args = dist_args,
                                npar = npar)
    linked_params <- vector(mode = "list", length = length(linked_parlist))
    g_fun <- paste0(link_fun, "()")
    linked_params <- lapply( 1:length(linked_parlist), FUN =
                               function(x) eval(parse(text = g_fun[x])) )
    for (i in 1:length(linked_parlist)){
      g_apply <- paste0("linked_params[[", i, "]]$g_inv")
      g_apply <- eval(parse(text = g_apply))
      # '[[]]' operator works over atomic or lists
      values[[linked_parlist[i]]] <- do.call( what = "g_apply",
                  args = list(eta = values[[linked_parlist[i]]]) )
    }
  }
  return(values)
}
#==============================================================================
# log-likelihood function computation -----------------------------------------
#==============================================================================
minus_lL <- function(x, dist, dist_args, over, link, npar, fixed){
  f <- function(param){
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
                          args = list(eta = param[[linked_params[i]]]) )
        }
      }
    }
    logf <- do.call( what = dist, args = c(list(x = x), param,
                                           log=TRUE, fixed) )
    return(-sum(logf))
  }
  return(f)
}
