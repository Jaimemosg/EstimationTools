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

create_inputs_matrix <- function(object){
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

  parameters <- matrix(unlist(parameters), nrow = n_data_points)

  response <- object$outputs$response

  inputs_matrix <- cbind(response, parameters)
  colnames(inputs_matrix) <- c("x", par_names)
  return(inputs_matrix)
}
