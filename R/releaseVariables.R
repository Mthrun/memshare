releaseVariables <- function(namespace, variableNames) {
    # releaseVariables(namespace,variableNames)
    #
    # A function to delete R matrices/vectors from a shared memory space.
    #
    # 
    # INPUT
    # namespace                 The string identifier of the shared memory space.
    # variableNames             A character vector of variable names to delete from namespace. The names have to match the ones given in registerVariables.
    #
    #author: JM 05/2025

  #mt: error catching
  if(!is.character(namespace)){
    warning("releaseVariables: namespace is not a character, doing nothing.")
    return(invisible(NULL))
  }
  if(nchar(namespace)==0){
    warning("releaseVariables: namespace is empty, doing nothing.")
    return(invisible(NULL))
  }
  
  if(length(variableNames)==0){
    warning("releaseVariables: variableName has length zero, doe nothing") 
    return(invisible(NULL))
  }
  if(!is.character(variableNames)){
    if(is.list(variableNames)){
      warning("releaseVariables: variableNames is a list and not a character vector, doing nothing.")
    }else{
      warning("releaseVariables: variableNames is not a character, doing nothing.")
    }
    return(invisible(NULL))
  }
    return(invisible(.Call("C_releaseVariables", namespace, variableNames, PACKAGE = "memshare")))

}
