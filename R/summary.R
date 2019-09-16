#' @title Summarize Maximum Likelihood Estimation
#'
#' @description
#' Displays maximum likelihood estimates computed with \code{\link{maxlogL}} with
#' its standard errors, AIC and BIC.
#' This is a \code{summary} method for \code{\link{maxlogL}} object
#'
#' @aliases summary.maxlogL
#'
#' @param object an object class "\code{\link{maxlogL}}".
#' @param Boot_Std_Err a logical variable for standard Errors computation by
#'        bootstrapping. The default is \code{FALSE}. This computation occurs when hessian
#'        from \code{\link{optim}} and \code{\link[numDeriv]{hessian}} fails in
#'        \code{\link{maxlogL}} routine. If this argument is \code{TRUE}, standard errors
#'        are computed, even if hessian did not fail in \code{\link{maxlogL}} routine.
#' @param ... arguments passed to \code{\link[boot]{boot}} for estimation of stantdard error with
#' non-parametric bootstrap.
#'
#' @details This \code{summary} method takes standard errors from \code{\link{maxlogL}} object and displays them.
#' If \code{\link[numDeriv]{hessian}} and Hessian from \code{\link{optim}} fails, standard errors are
#' computed with bootstrap. However, if user sets \code{Boot_Std_Err = TRUE} in this summary function,
#' standard errors are calculated by bootstrap, even if \code{\link[numDeriv]{hessian}} or Hessian from
#' \code{\link{optim}} converges.
#'
#' Supose that the user creates a variable named \code{fit} that stores \code{\link{maxlogL}} object. The
#' summary method modifies the element \code{fit$outputs$StdE_Method} object from \code{Gobal Environment}
#' (see the Second Example). If user does not creat a variable, the summary methid it simply calculates
#' standard errors (see the Third Example).
#'
#' @return An object of class 'summary.maxlogL'.
#' @importFrom stats sd printCoefmat
#' @importFrom boot boot
#' @export
#'
#' @examples
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
#' ## Standard error value (not computed yet, because is computed with 'summary')
#' print(phat$outputs$StdE)
#'
#' ## 'summary' method
#' summary(phat)
#'
#' ## Now, standard error is updated
#' print(phat$outputs$StdE_Method)
#' print(phat$outputs$StdE)
#'
#'
#' #--------------------------------------------------------------------------------
#' # Third example: Binomial probability parameter estimation with no varaible
#' # creation
#'
#' N <- rbinom(n = 100, size = 10, prob = 0.3)
#' summary(maxlogL(x = N, dist = 'dbinom', fixed = list(size = 10),
#'                 link = list(over = "prob", fun = "logit_link")))
#'
#'
#' #--------------------------------------------------------------------------------
#'
#' @references
#' \insertRef{Canty2017}{EstimationTools}
#'
#' @importFrom Rdpack reprompt
#'
#' @seealso \code{\link{maxlogL}}, \code{\link[boot]{boot}}
#'
#==============================================================================
# Summary function ------------------------------------------------------------
#==============================================================================
summary.maxlogL <- function(object, Boot_Std_Err = FALSE, ...){
  # .myenv <- environment()
  # list2env(var.list , envir = .myenv)
  # var.list <- as.list(object)
  estimate <- object$fit$par
  solver <- object$inputs$optimizer
  StdE_Method <- object$outputs$StdE_Method

  if (Boot_Std_Err == TRUE){
    StdE_Method <- "Bootstrap"
    warn <- paste0("\n...Bootstrap computation of Standard Error. ",
                   "Please, wait a few minutes...\n\n")
    cat(warn)

    stdE <- try(boot_MLE(object = object, ...), silent = TRUE)
    object_name <- deparse(substitute(object))

    ## Standard error updating in "object"
    if (any(object_name != deparse(object$inputs$call))){
      parent <- parent.frame()
      uptodate(p = parent, object_name, stdE, StdE_Method)
    }

    if( (any(is.na(stdE)) | is.error(stdE)) | any(is.character(stdE)) ){
      stdE <- rep(NA, times = object$outputs$npar)
      Zvalue <- rep(NA, times = object$outputs$npar)
      pvalue <- rep(NA, times = object$outputs$npar)
    } else {
      Zvalue <- round(estimate / stdE, digits = 4)
      pvalue <- 2 * pnorm(abs(Zvalue), lower.tail = FALSE)
    }

  } else {
    if ( any(is.na(estimate)) | any(is.nan(estimate)) |
         any(is.infinite(estimate))){
      stop(paste0("'maxlogL' computes NA ,NaN or Inf estimates. ",
                  "Please, change optimization algorithm or ",
                  "set different initial value(s)"))
    } else {
      if( any(is.na(object$fit$hessian)) ){
        StdE_Method <- "Bootstrap"
        warn <- paste0("\n...Bootstrap computation of Standard Error. ",
                       "Please, wait a few minutes...\n\n")
        cat(warn)

        stdE <- try(boot_MLE(object=object, ...), silent = TRUE)
        object_name <- deparse(substitute(object))

        ## Standard error updating in "object"
        if (any(object_name != deparse(object$inputs$call))){
          parent <- parent.frame()
          uptodate(p = parent, object_name, stdE, StdE_Method)
        }

        if( (any(is.na(stdE)) | is.error(stdE)) | any(is.character(stdE)) ){
          stdE <- rep(NA, times = object$outputs$npar)
          Zvalue <- rep(NA, times = object$outputs$npar)
          pvalue <- rep(NA, times = object$outputs$npar)
        } else {
          Zvalue <- round(estimate / stdE, digits = 4)
          pvalue <- 2 * pnorm(abs(Zvalue), lower.tail = FALSE)
        }
      } else {
        # Diagonal of Hessian^-1
        stdE <- round(sqrt(diag(solve(object$fit$hessian))), digits = 4)
        Zvalue <- round(estimate / stdE, digits = 4)
        pvalue <- 2 * pnorm(abs(Zvalue), lower.tail = FALSE)
        object_name <- deparse(substitute(object))

        ## Standard error updating in "object"
        if (any(object_name != deparse(object$inputs$call))){
          parent <- parent.frame()
          uptodate(p = parent, object_name, stdE, StdE_Method,
                   change_SE_method = FALSE)
        }
      }
    }
    if (any(is.na(stdE))){
      stdE <- rep(NA, times = object$outputs$npar)
      Zvalue <- rep(NA, times = object$outputs$npar)
      pvalue <- rep(NA, times = object$outputs$npar)
    }
  }

  ## Summary table
  res <- cbind(Estimate = estimate, stdE = stdE, Zvalue, pvalue)
  # res <- formatC(res, format = "e", digits = 3)
  res <- data.frame(res)
  colnames(res) <- c('Estimate ', 'Std. Error', 'Z value', 'Pr(>z)')

  ## Undo link application
  names_numeric <- rep("", times = object$outputs$npar)
  dist_args <- as.list(args(object$inputs$dist))
  j <- 1
  for (i in 1:length(dist_args)){
    if (is.numeric(dist_args[[i]]) || is.symbol(dist_args[[i]])){
      names_numeric[j] <- names(dist_args[i])
      j <- j + 1
    }
  }
  names_numeric <- names_numeric[-which(names_numeric=="x")]
  pos.del <- length(match(names(object$inputs$fixed), names_numeric))
  if (pos.del > 0){
    names_numeric <- names_numeric[-pos.del]
  }
  rownames(res) <- names_numeric
  cat("---------------------------------------------------------------\n")
  cat(paste0('Optimization routine: ', object$input$optimizer),'\n')
  cat(paste0('Standard Error calculation: ', StdE_Method),'\n')
  cat("---------------------------------------------------------------\n")
  AIC <- 2 * object$outputs$npar - 2 * object$fit$objective
  BIC <- log(length(object$outputs$n)) * object$outputs$npar - 2 * object$fit$objective
  table <- data.frame(AIC=round(AIC, digits = 4),
                      BIC=round(BIC, digits = 4))
  rownames(table) <- " "
  print(table)
  cat("---------------------------------------------------------------\n")
  # print(res[,1:2])
  printCoefmat(res[,1:2], P.values = FALSE)
  # cat("---------------------------------------------------------------\n")
  # cat('Note: p-values under asymptotic normality of estimators \n')
  cat("-----\n")
  estimatePrint <- estimate
  names(estimatePrint) <- names_numeric
  ans <- list(Estimate = estimatePrint, Std_Error = stdE,
              Z_value = Zvalue, p_value = pvalue)
  class(ans) <- "summary.maxlogL"
  result <- ans
}

#==============================================================================
# Standard error by Bootstrap -------------------------------------------------
#==============================================================================
# Inspired by bootstrap implementation on 'cubm' package
# https://github.com/fhernanb/cubm
boot_MLE <- function(object, R = 2000, ...){

  MLE <- function(data, i, object){
    MLE.arguments <- object$inputs[-which(names(object$inputs) == "x")]
    MLE.arguments <- MLE.arguments[-which(names(MLE.arguments) == "call")]
    estimates <- do.call( what = "maxlogL",
                          args = c(list(x = data[i]), MLE.arguments) )
    return(estimates$fit$par)
  }
  data <- object$inputs$x
  btstrp <- boot::boot(data = data, statistic = MLE, R = R, object = object, ...)
  return(apply(X = btstrp$t, MARGIN = 2, FUN = sd))
}

#==============================================================================
# Standard update -------------------------------------------------------------
#==============================================================================
uptodate <- function(p, object_name, stdE, StdE_Method,
                     change_SE_method = TRUE){
  allocation2 <- paste0("p$", object_name, "$outputs$StdE <- ", stdE)
  eval(parse(text = allocation2))
  if (change_SE_method){
    allocation1 <- paste0("p$", object_name,
                          "$outputs$StdE_Method <- StdE_Method")
    eval(parse(text = allocation1))
  }
}

#==============================================================================
# Print method ---------------------------------------------------------------
#==============================================================================
print.maxlogL <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\n Results: \n")
  cat("\n Estimated parameters: \n")
  print(x$par)
}
