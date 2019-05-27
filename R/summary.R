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
#' @param ... arguments passed to \code{\link[boot]{boot}} for estimation of stantdard error with
#' non-parametric bootstrap. This computation occurs when option \code{hessian = TRUE} from \code{\link{optim}}
#' and \code{\link[numDeriv]{hessian}} fails in \code{\link{maxlogL}} routine.
#'
#' @return An object of class "summary.maxlogL".
#' @importFrom stats sd printCoefmat
#' @importFrom boot boot
#' @export
#'
#' @examples
#' \donttest{
#' # One known parameter
#' x <- rnorm(n = 10000, mean = 160, sd = 6)
#' theta_1 <- maxlogL(x = x, dist = 'dnorm', control = list(trace = 1),
#'                  link = list(over = "sd", fun = "log_link"),
#'                  fixed = list(mean = 160))
#' summary(theta_1)}
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

summary.maxlogL <- function(object, ...){
  .myenv <- environment()
  var.list <- as.list(object)
  list2env(var.list , envir = .myenv)
  estimate <- object$fit$par
  solver <- object$inputs$optimizer
  StdE_Method <- object$outputs$StdE_Method

  if( any(is.na(object$fit$hessian)) ){
    StdE_Method <- 'Bootstrap'
    stdE <- try(boot_MLE(object=object, ...), silent = TRUE)
    if( (is.na(stdE) || is.error(stdE)) || is.character(stdE) ){
      stdE <- rep(NaN, times = object$outputs$npar)
      Zvalue <- rep(NaN, times = object$outputs$npar)
      pvalue <- rep(NaN, times = object$outputs$npar)
    } else {
      Zvalue <- round(estimate / stdE, digits = 4)
      pvalue <- 2 * pnorm(abs(Zvalue), lower.tail = FALSE)
    }
  } else {
    # Diagonal of Hessian^-1
    stdE <- round(sqrt(diag(solve(object$fit$hessian))), digits = 4)
    Zvalue <- round(estimate / stdE, digits = 4)
    pvalue <- 2 * pnorm(abs(Zvalue), lower.tail = FALSE)
  }
  if (any(is.na(stdE))){
    stdE <- rep(NaN, times = object$outputs$npar)
    Zvalue <- rep(NaN, times = object$outputs$npar)
    pvalue <- rep(NaN, times = object$outputs$npar)
  }
  res <- cbind(Estimate = estimate, stdE = stdE, Zvalue, pvalue)
  # res <- formatC(res, format = "e", digits = 3)
  res <- data.frame(res)

  colnames(res) <- c('Estimate ', 'Std. Error', 'Z value', 'Pr(>z)')

  names_numeric <- rep("", times=object$outputs$npar)
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
  # cat('Note: p-values under asymptotic approximation \n')
  cat("-----\n")
}

#==============================================================================
# Standard error by Bootstrap -------------------------------------------------
#==============================================================================

boot_MLE <- function(object, R = 2000, ...){

  MLE <- function(data, i, object){
    MLE.arguments <- object$inputs[-which(names(object$inputs) == "x")]
    estimates <- do.call( what = "maxlogL",
                          args = c(list(x = data[i]), MLE.arguments) )
    return(estimates$fit$par)
  }
  data <- object$inputs$x
  btstrp <- boot::boot(data = data, statistic = MLE, R = R, object = object, ...)
  return(apply(X = btstrp$t, MARGIN = 2, FUN = sd))
}
