#' @title Summarize Maximum Likelihood Estimation
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Displays maximum likelihood estimates computed with \code{\link{maxlogL}} with
#' its standard errors, AIC and BIC.
#' This is a \code{summary} method for \code{\link{maxlogL}} object.
#'
#' @aliases summary.maxlogL
#'
#' @param object an object of \code{\link{maxlogL}} class which summary is desired.
#' @param ... additional arguments affecting the summary produced.
#'
#' @details This \code{summary} method computes and displays AIC, BIC,
#' estimates and standard errors from a estimated model stored i a \code{maxlogL}
#' class object. It  also displays and computes Z-score and p values of significance
#' test of parameters.
#'
#' @return A list with information that summarize results of a \code{maxlogL} class object.
#' @importFrom stats sd printCoefmat
#' @export
#'
#' @examples
#' library(EstimationTools)
#'
#' #--------------------------------------------------------------------------------
#' ### First example: One known parameter
#'
#' x <- rnorm(n = 10000, mean = 160, sd = 6)
#' theta_1 <- maxlogL(x = x, dist = 'dnorm', control = list(trace = 1),
#'                  link = list(over = "sd", fun = "log_link"),
#'                  fixed = list(mean = 160))
#' summary(theta_1)
#'
#'
#' #--------------------------------------------------------------------------------
#' # Second example: Binomial probability parameter estimation with variable
#' # creation
#'
#' N <- rbinom(n = 100, size = 10, prob = 0.3)
#' phat <- maxlogL(x = N, dist = 'dbinom', fixed = list(size = 10),
#'                 link = list(over = "prob", fun = "logit_link"))
#'
#' ## Standard error calculation method
#' print(phat$outputs$StdE_Method)
#'
#' ## 'summary' method
#' summary(phat)
#'
#' #--------------------------------------------------------------------------------
#' # Third example: Binomial probability parameter estimation with no variable
#' # creation
#'
#' N <- rbinom(n = 100, size = 10, prob = 0.3)
#' summary(maxlogL(x = N, dist = 'dbinom', fixed = list(size = 10),
#'                 link = list(over = "prob", fun = "logit_link")))
#'
#' #--------------------------------------------------------------------------------
#' # Fourth example: Estimation in a regression model with simulated normal data
#' n <- 1000
#' x <- runif(n = n, -5, 6)
#' y <- rnorm(n = n, mean = -2 + 3 * x, sd = exp(1 + 0.3* x))
#' norm_data <- data.frame(y = y, x = x)
#' formulas <- list(sd.fo = ~ x, mean.fo = ~ x)
#'
#' norm_mod <- maxlogLreg(formulas, y_dist = y ~ dnorm, data = norm_data,
#'                        link = list(over = "sd", fun = "log_link"))
#'
#' ## 'summary' method
#' summary(norm_mod)
#'
#'
#' #--------------------------------------------------------------------------------
#'
#' @seealso \code{\link{maxlogL}}, \code{\link{maxlogLreg}},
#' \code{\link{bootstrap_maxlogL}}
#'
#' @method summary maxlogL
#' @export
#==============================================================================
# Summary function ------------------------------------------------------------
#==============================================================================
summary.maxlogL <- function(object, ...){
  # .myenv <- environment()
  # list2env(var.list , envir = .myenv)
  # var.list <- as.list(object)
  dots <- substitute(...())
  names_dots <- names(dots)
  match_names <- match(c("Boot_Std_Err", "R"), names_dots)
  match_names <- na.omit(match_names)
  if ( length(match_names) != 0)
    stop(paste0("Bootstrap computation of summary method has been removed.",
                "If you need to compute standard errors by bootstrap, please",
                "use our 'bootstrap_maxlogL' function."))

  estimate <- object$fit$par
  solver <- object$inputs$optimizer
  StdE_Method <- object$outputs$StdE_Method
  n_est <- ifelse( object$outputs$type == "maxlogL", object$outputs$npar,
                   sum(object$outputs$b_length) )
  warn <- NULL

  if ( any(StdE_Method == "'optim' and 'numDeriv' failed") ){
  	condition1 <- any(is.na(estimate)) | any(is.nan(estimate)) |
                  any(is.infinite(estimate)) | any(is.error(estimate))
    if ( condition1 ){
        stop(paste0("'", as.character(as.list(call)[[1]]), "' computes NA ,",
                    "NaN or Inf estimates. Please, change optimization ",
                    "algorithm or set different initial value(s)"))
    } else {
      # codition2 <- any(is.na(object$fit$hessian)) #|
      # 		any(is.nan(object$fit$hessian)) |
      # 		   any(is.infinite(object$fit$hessian)) |
      # 		   any(is.error(object$fit$hessian)) |
      # 		   any(is.character(object$fit$hessian))
      if (  any(is.na(object$fit$hessian)) ){  # this is the condition2
          warn <- paste0("One or more standard errors equals NA. Please,
          				 try bootstrap computation of standard error with
          				 'boot_maxlogL' function.")
          StdE <- rep(NA, times = n_est)
          Zvalue <- rep(NA, times = n_est)
          pvalue <- rep(NA, times = n_est)
      }
    }
  } else {
    StdE <- object$fit$StdE
    Zvalue <- estimate / StdE
    pvalue <- 2 * pnorm(abs(Zvalue), lower.tail = FALSE)
    StdE_Method <- object$outputs$StdE_Method
  }

  cat("_______________________________________________________________\n")
  cat(paste0('Optimization routine: ', object$input$optimizer),'\n')
  cat(paste0('Standard Error calculation: ', StdE_Method),'\n')
  cat("_______________________________________________________________\n")
  AIC <- stats::AIC(object); BIC <- stats::BIC(object)
  table <- data.frame(AIC=round(AIC, digits = 4),
                      BIC=round(BIC, digits = 4))
  rownames(table) <- " "
  print(table)
  cat("_______________________________________________________________\n")

  if ( object$outputs$type == "maxlogL" ){
    ## Summary table
    res <- cbind(estimate = estimate, se = StdE,
                 zvalue = Zvalue, pvalue = pvalue)
    # res <- format(res, digits = 5, nsmall = 4)
    # res <- formatC(res, format = "e", digits = 3)
    res <- data.frame(res)
    colnames(res) <- c('Estimate ', 'Std. Error', 'Z value', 'Pr(>|z|)')
    printCoefmat(res, P.values = TRUE, digits = 4)
    cat("_______________________________________________________________\n")
    estimatePrint <- estimate
    cat('Note: p-values valid under asymptotic normality of estimators \n')
    cat("---\n")
  } else {
    A <- param_index(object$outputs$b_length, object$outputs$npar)
    res <- cbind(estimate = estimate, se = StdE, zvalue = Zvalue,
                 pvalue = pvalue)
    res <- data.frame(res)
    colnames(res) <- c('Estimate', 'Std. Error', 'Z value', 'Pr(>|z|)')
    for (i in 1:object$outputs$npar){
      param_name <- object$outputs$par_names[i]
      link_index <- match(param_name, object$inputs$link$over, nomatch = 0)
      if (link_index == 0){
        fun_note <- paste0("Fixed effects for ", param_name, "\n")
      } else {
        link_function <- object$inputs$link$fun[link_index]
        link_name <- eval(parse(text = paste0(link_function, "()$name")))
        fun_note <- paste0("Fixed effects for ", link_name,"(",
                           param_name, ") \n")
      }
      cat(fun_note)
      cat("---------------------------------------------------------------\n")
      res_temp <- res[A[i,1]:A[i,2],]
      rownames(res_temp) <- names(object$fit$par)[A[i,1]:A[i,2]]
      printCoefmat(res_temp, P.values = TRUE, has.Pvalue = TRUE)
      cat("_______________________________________________________________\n")
    }
    cat('Note: p-values valid under asymptotic normality of estimators \n')
    cat("---\n")
    estimatePrint <- estimate
  }
  if ( !is.null(warn) ) warning(warn)
  ans <- list(Estimate = estimatePrint, Std_Error = StdE,
              Z_value = Zvalue, p_value = pvalue)
  result <- ans
  # class(ans) <- "summary.maxlogL"
  # return(ans)
}
#==============================================================================
# Print method ----------------------------------------------------------------
#==============================================================================
#' @export
print.maxlogL <- function(x, ...) {
  if ( x$outputs$type == "maxlogLreg" ){
    A <- param_index(x$outputs$b_length, x$outputs$npar)
    cat("Call:\n")
    print(x$inputs$call)
    cat("\n Results: \n")
    for (i in 1:x$outputs$npar){
      param_name <- x$outputs$par_names[i]
      link_index <- match(param_name, x$inputs$link$over, nomatch = 0)
      if (link_index == 0){
        fun_note <- paste0("\n Estimated coefficients for ",
                           x$outputs$par_names[i], "\n")
      } else {
        link_function <- x$inputs$link$fun[link_index]
        link_name <- eval(parse(text = paste0(link_function, "()$name")))
        fun_note <- paste0("\n Estimated coefficients for ", link_name,"(",
                           param_name, ") \n")
      }
      cat(fun_note)
      print(x$fit$par[A[i,1]:A[i,2]])
    }
  } else {
    cat("Call:\n")
    print(x$inputs$call)

    cat("\n Results: \n")
    cat("\n Estimated parameters: \n")
    result <- x$fit$par
    # names(result) <-
    print(result)
  }
}
