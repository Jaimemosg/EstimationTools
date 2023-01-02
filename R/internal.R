#' @title Internal functions for formula and data handle
#' @description Utility functions useful for passing data from functions
#' inside others.
#'
#' @encoding UTF-8
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
fo_and_data <- function(y, fo, model_frame, data, fo2Surv = TRUE){
  if ( !is.Surv(y) ){
    if ( fo2Surv ) fo <- formula2Surv(model_frame)
    if ( missing(data) | is.null(data) ) data <- model_frame
  } else {
    if ( missing(data) | is.null(data) ){
      vars <- names(model_frame)
      ySurv <- vars[1L]
      yname <- gsub("Surv\\((.*?),.*", "\\1", ySurv)
      statusname <- gsub(paste0("Surv\\(", yname, ",(.*?)\\)"), "\\1", ySurv)
      right_hand <- attr(stats::terms(fo), 'term.labels')

      if (length(right_hand) == 0){
        factorname <- NULL
        data <- data.frame(y[,1], y[,2])
      } else {
        factorname <- as.character(right_hand[1])
        other_column <- model_frame[,2]
        data <- data.frame(y[,1], y[,2], other_column)
      }
      colnames(data) <- c(yname, statusname, factorname)
    }
  }
  return(list(data = data, fo = fo))
}
#==============================================================================
# Extract arguments from ... when it contains inputs for multiple functions ---
# (used only for integration routines in 'integration' wrapper)
#==============================================================================
#' @export
#' @keywords internal
#' @rdname internalfunc
extract_fun_args <- function(fun, exclude, ...){
  dots <- substitute(...())
  args_list <- formals(fun)
  routine_args <- names(args_list[-1])
  if ( !missing(exclude) ){
    routine_args <- routine_args[!(routine_args %in% exclude)]
    key_args <- routine_args[routine_args %in% names(dots)]
    # routine_input_args <- args_list[key_args]
    routine_input_args <- dots[key_args]
  } else {
    routine_input_args <- args_list[routine_args]
  }
  add_args <- dots[!(names(dots) %in% routine_args)]
  return(list(routine_input_args, add_args))
}
#==============================================================================
# Saturated model -------------------------------------------------------------
#==============================================================================
#' @export
#' @keywords internal
#' @rdname internalfunc
saturated_maxlogL <- function(object, silent = TRUE){
  if (silent) options(warn = -1)
  y <- object$outputs$response
  X <- factor(1:length(y))
  formulas <- object$inputs$formulas
  formulas <- sapply(formulas, function(x) formula(~X))
  maxlogL_call <- object$inputs$call
  maxlogL_call[['data']] <- data.frame(X, y)
  maxlogL_call[['formulas']] <- formulas
  maxlogL_call[["y_dist"]][[2]] <- as.name("y")
  saturated_model <- eval(maxlogL_call)
  if (silent) options(warn = 0)
  return(saturated_model)
}
#==============================================================================
# Null model-------------------------------------------------------------------
#==============================================================================
#' @export
#' @keywords internal
#' @rdname internalfunc
null_maxlogL <- function(object, silent = TRUE){
  if (silent) options(warn = -1)
  X <- 1
  y <- object$outputs$response
  formulas <- object$inputs$formulas
  formulas <- sapply(formulas, function(x) formula(~1))
  maxlogL_call <- object$inputs$call
  maxlogL_call[['data']] <- data.frame(X, y)
  maxlogL_call[['formulas']] <- formulas
  saturated_model <- eval(maxlogL_call)
  if (silent) options(warn = 0)
  return(saturated_model)
}
#==============================================================================
# Identity function for Monte Carlo integrals ---------------------------------
#==============================================================================
#' @export
#' @keywords internal
#' @rdname internalfunc
identity <- function(x) x
