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
