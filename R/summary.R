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
  n_est <- ifelse( object$outputs$type == "maxlogL", object$outputs$npar,
                   sum(object$outputs$b_length) )

  if ( any(object$outputs$StdE == "Not computed yet") ){
    if ( Boot_Std_Err == TRUE ){
      StdE_Method <- "Bootstrap"
      warn <- paste0("\n...Bootstrap computation of Standard Error. ",
                     "Please, wait a few minutes...\n\n")
      cat(warn)

      stdE <- try(boot_MLE(object = object, ...), silent = TRUE)

      # Standard error updating in "object"
      object_name <- deparse(substitute(object))
      if (any(object_name != deparse(object$inputs$call))){
        parent <- parent.frame()
        uptodate(p = parent, object_name, stdE, StdE_Method)
      }

      if( (any(is.na(stdE)) | is.error(stdE)) | any(is.character(stdE)) ){
        stdE <- rep(NA, times = n_est)
        Zvalue <- rep(NA, times = n_est)
        pvalue <- rep(NA, times = n_est)
      } else {
        Zvalue <- round(estimate / stdE, digits = 4)
        pvalue <- 2 * pnorm(abs(Zvalue), lower.tail = FALSE)
      }

    } else {
      if ( any(is.na(estimate)) | any(is.nan(estimate)) |
           any(is.infinite(estimate))){
        stop(paste0("'", as.character(as.list(call)[[1]]), "' computes NA ,",
                    "NaN or Inf estimates. Please, change optimization ",
                    "algorithm or set different initial value(s)"))
      } else {
        if( any(is.na(object$fit$hessian)) ){
          StdE_Method <- "Bootstrap"
          warn <- paste0("\n...Bootstrap computation of Standard Error. ",
                         "Please, wait a few minutes...\n\n")
          cat(warn)

          stdE <- try(boot_MLE(object = object, ...), silent = TRUE)

          # Standard error updating in "object"
          object_name <- deparse(substitute(object))
          if (any(object_name != deparse(object$inputs$call))){
            parent <- parent.frame()
            uptodate(p = parent, object_name, stdE, StdE_Method)
          }

          if( (any(is.na(stdE)) | is.error(stdE)) | any(is.character(stdE)) ){
            stdE <- rep(NA, times = n_est)
            Zvalue <- rep(NA, times = n_est)
            pvalue <- rep(NA, times = n_est)
          } else {
            Zvalue <- round(estimate / stdE, digits = 4)
            pvalue <- 2 * pnorm(abs(Zvalue), lower.tail = FALSE)
          }
        } else {
          # Diagonal of Hessian^-1
          stdE <- round(sqrt(diag(solve(object$fit$hessian))), digits = 4)
          Zvalue <- round(estimate / stdE, digits = 4)
          pvalue <- 2 * pnorm(abs(Zvalue), lower.tail = FALSE)

          # Standard error updating in "object"
          object_name <- deparse(substitute(object))
          if (any(object_name != deparse(object$inputs$call))){
            parent <- parent.frame()
            uptodate(p = parent, object_name, stdE, StdE_Method,
                     change_SE_method = FALSE)
          }
        }
      }
      if (any(is.na(stdE))){
        stdE <- rep(NA, times = n_est)
        Zvalue <- rep(NA, times = n_est)
        pvalue <- rep(NA, times = n_est)
      }
    }
  } else {
    stdE <- object$outputs$StdE
    Zvalue <- round(estimate / stdE, digits = 4)
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
    res <- cbind(estimate = estimate, se = stdE, zvalue = Zvalue,
                 pvalue = pvalue)
    # res <- formatC(res, format = "e", digits = 3)
    res <- data.frame(res)
    colnames(res) <- c('Estimate ', 'Std. Error', 'Z value', 'Pr(>|z|)')

    ## Parameter names
    # names_numeric <- rep("", times = n_est)
    # dist_args <- as.list(args(object$inputs$dist))
    # j <- 1
    # for (i in 1:length(dist_args)){
    #   if (is.numeric(dist_args[[i]]) || is.symbol(dist_args[[i]])){
    #     names_numeric[j] <- names(dist_args[i])
    #     j <- j + 1
    #   }
    # }
    # names_numeric <- names_numeric[-which(names_numeric == "x")]
    # pos_del <- length(match(names(object$inputs$fixed), names_numeric))
    # if (pos_del > 0){
    #   names_numeric <- names_numeric[-pos_del]
    # }
    # rownames(res) <- names_numeric
    # AIC <- 2 * object$outputs$npar - 2 * object$fit$objective
    # BIC <- log(length(object$outputs$n)) * object$outputs$npar - 2 * object$fit$objective
    # table <- data.frame(AIC=round(AIC, digits = 4),
                        # BIC=round(BIC, digits = 4))
    # rownames(table) <- " "
    # print(table)
    # cat("_______________________________________________________________\n")
    printCoefmat(res[,1:2], P.values = FALSE)
    cat("_______________________________________________________________\n")
    # cat('Note: p-values valid under asymptotic normality of estimators \n')
    # cat("-----\n")
    estimatePrint <- estimate
    # names(estimatePrint) <- names_numeric
  } else {
    A <- param_index(object$outputs$b_length, object$outputs$npar)
    res <- cbind(estimate = estimate, se = stdE, zvalue = Zvalue,
                 pvalue = pvalue)
    res <- data.frame(res)
    colnames(res) <- c('Estimate', 'Std. Error', 'Z value', 'Pr(>|z|)')
    for (i in 1:object$outputs$npar){
      cat(paste0("Fixed effects for g(", object$outputs$par_names[i],
                 ") \n"))
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
  ans <- list(Estimate = estimatePrint, Std_Error = stdE,
              Z_value = Zvalue, p_value = pvalue)
  result <- ans
  # class(ans) <- "summary.maxlogL"
  # return(ans)
}

#==============================================================================
# Standard error by Bootstrap -------------------------------------------------
#==============================================================================
# Inspired by bootstrap implementation on 'cubm' package
# https://github.com/fhernanb/cubm
boot_MLE <- function(object, R = 2000, ...){

  MLE <- function(data, i, object){
    MLE_arguments <- as.list(object$inputs$call)[-1]
    if ( !is.null(MLE_arguments$control) ){
      MLE_arguments$control <-
        MLE_arguments$control[-which(names(MLE_arguments$control) == "trace")]
    }
    if ( object$outputs$type == "maxlogL" ){
      # MLE_arguments <- object$inputs[-which(names(object$inputs) == "call")]
      # MLE_arguments <- MLE_arguments[-which(names(MLE_arguments) == "data")]
      MLE_arguments <- MLE_arguments[-which(names(MLE_arguments) == "x")]
      estimates <- do.call( what = "maxlogL",
                            args = c(list(x = data[i]), MLE_arguments) )
    } else {
      MLE_arguments <- MLE_arguments[-which(names(MLE_arguments) ==
                                              "formulas")]
      fos <- object$inputs$formulas
      data_names <- names(data)
      data <- data[i,]
      data <- as.data.frame(data)
      names(data) <- data_names
      estimates <- do.call( what = "maxlogLreg",
                            args = c(list(data = data, formulas = fos),
                                     MLE_arguments) )
    }
    return(estimates$fit$par)
  }
  data <- object$inputs$data
  btstrp <- boot::boot(data = data, statistic = MLE, R = R, object = object, ...)
  return(apply(X = btstrp$t, MARGIN = 2, FUN = sd))
}

#==============================================================================
# Standard update -------------------------------------------------------------
#==============================================================================
uptodate <- function(p, object_name, stdE, StdE_Method,
                     change_SE_method = TRUE){
  # allocation2 <- paste0("p$", object_name, "$outputs$StdE <- ", stdE)
  allocation2 <- paste0("p$", object_name, "$outputs$StdE <- stdE")
  eval(parse(text = allocation2))
  if (change_SE_method){
    allocation1 <- paste0("p$", object_name,
                          "$outputs$StdE_Method <- StdE_Method")
    eval(parse(text = allocation1))
  }
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
      cat(paste0("\n Estimated coefficients for g(",
                 x$outputs$par_names[i], "): \n"))
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
#==============================================================================
# logLik method ---------------------------------------------------------------
#==============================================================================
#' @export
logLik.maxlogL <- function(object, ...){
  p <- ifelse(object$outputs$type == "maxlogL",
              object$outputs$npar,
              sum(as.numeric(object$outputs$b_length)))
  val <- object$fit$objective
  attr(val, "nall") <- object$outputs$n
  attr(val, "nobs") <- object$outputs$n
  attr(val, "df") <- p
  class(val) <- "logLik"
  val
}
