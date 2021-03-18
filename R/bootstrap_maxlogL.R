#' @title Bootstrap computation of standard error for \code{maxlogL} class objects.
#'
#' @encoding UTF-8
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' \code{bootstrap_maxlogL} computes standard errors of
#' \code{\link{maxlogL}} class objects by non-parametric bootstrap.
#'
#' @param object an object of \code{\link{maxlogL}} class whose standard errors
#'               are going to be computed by bootstrap.
#' @param R numeric. It is the number of resamples performed with the dataset
#'          in bootstrap computation. Default value is 2000.
#' @param silent logical. If TRUE, notifications of \code{bootstrap_maxlogL} are suppressed.
#' @param ... arguments passed to \code{\link[boot]{boot}} used in this routine
#'            for estimation of standard errors.
#'
#' @details The computation performed by this function may be
#' invoked when Hessian from \code{\link{optim}} and
#' \code{\link[numDeriv]{hessian}} fail in  \code{\link{maxlogL}} or
#' in \code{\link{maxlogLreg}}.
#'
#' However, this function can be run even if Hessian matrix calculation
#' does not fails. In this case, standard errors in the \code{\link{maxlogL}}
#' class object is replaced.
#'
#' @return A modified object of class \code{maxlogL}.
#' @importFrom boot boot
#' @export
#'
#' @examples
#' library(EstimationTools)
#'
#' #--------------------------------------------------------------------------------
#' # First example: Comparison between standard error computation via Hessian matrix
#' # and standard error computation via bootstrap
#'
#' N <- rbinom(n = 100, size = 10, prob = 0.3)
#' phat1 <- maxlogL(x = N, dist = 'dbinom', fixed = list(size = 10),
#'                 link = list(over = "prob", fun = "logit_link"))
#'
#' ## Standard error computation method and results
#' print(phat1$outputs$StdE_Method)   # Hessian
#' summary(phat1)
#'
#' ## 'bootstrap_maxlogL' implementation
#' phat2 <- phat1                   # Copy the first 'maxlogL' object
#' bootstrap_maxlogL(phat2, R = 100)
#'
#' ## Standard error computation method and results
#' print(phat2$outputs$StdE_Method)   # Bootstrap
#' summary(phat2)
#'
#'
#' #--------------------------------------------------------------------------------
#'
#' @references
#' \insertRef{Canty2017}{EstimationTools}
#'
#' @importFrom Rdpack reprompt
#' @seealso \code{\link{maxlogL}}, \code{\link{maxlogLreg}}, \code{\link[boot]{boot}}
#'
#==============================================================================
# Main routine ----------------------------------------------------------------
#==============================================================================
bootstrap_maxlogL <- function(object, R = 2000, silent = FALSE, ...){
  if (silent) options(warn = -1)
  StdE_Method <- "Bootstrap"
  msg <- paste0("\n...Bootstrap computation of Standard Error. ",
                "Please, wait a few minutes...\n\n")
  if ( !silent ) message(msg)
  n_est <- ifelse( object$outputs$type == "maxlogL", object$outputs$npar,
                   sum(object$outputs$b_length) )
  StdE <- try(bootstrap_MLE(object = object, R = R, ...),
              silent = TRUE)
  if( (any(is.na(StdE)) | is.error(StdE)) | any(is.character(StdE)) ){
    StdE <- rep(NA, times = n_est)
  }

  # Standard error updating in "object"
  object_name <- deparse(substitute(object))
  if (any(object_name != deparse(object$inputs$call))){
    parent <- parent.frame()
    uptodate(p = parent, object_name, StdE, StdE_Method)
  }
  if ( !silent ) message("\n --> Done <--- \n")
  if (silent) options(warn = 0)
}
#==============================================================================
# Standard error by Bootstrap -------------------------------------------------
#==============================================================================
# Inspired by bootstrap implementation on 'cubm' package
# https://github.com/fhernanb/cubm
bootstrap_MLE <- function(object, R, ...){
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
# Standard Error update -------------------------------------------------------
#==============================================================================
uptodate <- function(p, object_name, StdE, StdE_Method,
                     change_SE_method = TRUE){
  # allocation2 <- paste0("p$", object_name, "$outputs$StdE <- ", StdE)
  allocation2 <- paste0("p$", object_name, "$fit$StdE <- StdE")
  eval(parse(text = allocation2))
  if (change_SE_method){
    allocation1 <- paste0("p$", object_name,
                          "$outputs$StdE_Method <- StdE_Method")
    eval(parse(text = allocation1))
  }
}
