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
#==============================================================================
# Saturated model -------------------------------------------------------------
#==============================================================================
#' @export
saturated_maxlogL <- function(object, silent = TRUE){
  if (silent) options(warn = -1)
  X <- factor(1:length(y))
  formulas <- object$inputs$formulas
  formulas <- sapply(formulas, function(x) formula(~X))
  maxlogL_call <- object$inputs$call
  maxlogL_call[['data']] <- data.frame(X, y)
  maxlogL_call[['formulas']] <- formulas
  saturated_model <- eval(maxlogL_call)
  if (silent) options(warn = 0)
  return(saturated_model)
}
#==============================================================================
# Null model-------------------------------------------------------------------
#==============================================================================
#' @export
null_maxlogL <- function(object, silent = TRUE){
  if (silent) options(warn = -1)
  X <- 1
  formulas <- object$inputs$formulas
  formulas <- sapply(formulas, function(x) formula(~X))
  maxlogL_call <- object$inputs$call
  maxlogL_call[['data']] <- data.frame(X, y)
  maxlogL_call[['formulas']] <- formulas
  saturated_model <- eval(maxlogL_call)
  if (silent) options(warn = 0)
  return(saturated_model)
}
