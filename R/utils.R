#==============================================================================
# Select custom values for integration ----------------------------------------
#==============================================================================
set_custom_integration_routine<- function(support, routine){
  if (is.null(routine) | missing(routine)){
    if (support$type == 'continuous'){
      routine <- 'integrate'
    } else if (support$type == 'discrete'){
      routine <- 'summate'
    }
  }
  return(routine)
}

create_inputs <- function(object, add_response, as_matrix = TRUE){
  n_data_points <- object$outputs$n
  parameters <- object$outputs$fitted.values
  par_names <- names(parameters)
  fixed_pars <- object$inputs$fixed

  if (!is.null(fixed_pars)){

    for (par_i in names(fixed_pars)){
      fixed_pars[[par_i]] <- rep(fixed_pars[[par_i]], n_data_points)
      par_i_list <- fixed_pars[par_i]
      parameters <- c(parameters, par_i_list)
      par_names <- c(par_names, names(par_i_list))
    }
  }

  response <- object$outputs$response

  if (add_response){
    response_list <- list(response)
    parameters <- c(response_list, parameters)
    par_names <- c("x", par_names)
  }

  if (as_matrix){
    parameters <- matrix(unlist(parameters), nrow = n_data_points)
    # parameters <- cbind(response, parameters)
    # colnames(parameters) <- c("x", par_names)
    colnames(parameters) <- par_names
  } else{
    names(parameters) <- par_names
  }

  return(parameters)
}
