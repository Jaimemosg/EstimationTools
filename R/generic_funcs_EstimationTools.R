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
# maxlogL function identification ---------------------------------------------
#==============================================================================
is.maxlogL <- function(x){
  inherits(x, c("maxlogL"))
}
#==============================================================================
# EmpiricalTTT function identification ----------------------------------------
#==============================================================================
is.EmpiricalTTT <- function(x){
  inherits(x, c("EmpiricalTTT"))
}
#==============================================================================
# EmpiricalTTT function identification ----------------------------------------
#==============================================================================
is.HazardShape <- function(x){
  inherits(x, c("HazardShape"))
}
