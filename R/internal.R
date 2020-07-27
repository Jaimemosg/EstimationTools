#' @title Internal functions for formula and data handle
#' @description Utility functions useful for passing data from functions
#' inside others.
#'
#' @author Jaime Mosquera Gutiérrez, \email{jmosquerag@unal.edu.co}
#' @param model_frame a model frame build internally in some functions on
#'        \strong{EstimationTools} package
#' @export
#' @keywords internal
#' @rdname internalfunc
#'
#==============================================================================
# Formula conversion for 'survfit' using --------------------------------------
#==============================================================================
formula2Surv <- function(model_frame){
  vars <- names(model_frame)
  y_var <- paste0('Surv(', vars[1L], ', rep(1, nrow(model_frame)))')
  right_side <- if (length(vars) > 1){
    paste(vars[2L:end(vars)[1]], collapse = "+")
  } else {
    "1"
  }
  formula <- as.formula(paste0(y_var, "~", right_side))
  return(formula)
}
#==============================================================================
# Data preparation for TTT computation ----------------------------------------
#==============================================================================
#' @export
#' @keywords internal
#' @rdname internalfunc
#'
fo_and_data <- function(y, fo, model_frame, data){
  if ( !is.Surv(y) ){
    fo <- formula2Surv(model_frame)
    if ( missing(data) ) data <- model_frame
  } else {
    if ( missing(data) ){
      vars <- names(model_frame)
      ySurv <- vars[1L]
      yname <- gsub("Surv\\((.*?),.*", "\\1", ySurv)
      statusname <- gsub(paste0("Surv\\(", yname, ",(.*?)\\)"), "\\1", ySurv)
      factorname <- vars[length(vars)]
      data <- data.frame(y[,1], y[,2], model_frame[,2])
      colnames(data) <- c(yname, statusname, factorname)
    }
  }
  return(list(data = data, fo = fo))
}
