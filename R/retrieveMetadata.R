retrieveMetadata <- function(namespace, variableName) {
    # retrieveMetadata(namespace, variableName)
    #
    # Function to obtain the metadata of a variable from a shared memory space.
    # 
    #
    # INPUT
    # namespace                The string identifier of the shared memory space.
    # variableName             [1:m] A variable name to retrieve the metadata for.
    #
    # OUPUT
    # List V                    [1:m] The names of one ore more than one variable to retrieve the metadata from the shared memory space.
    #
    #author: JM 05/2025
    #1.editor: MT 08/2025, recursive approach for more than one variableName (otherwise rstudio breaks down)
  
  if(!is.character(namespace)){
    warning("retrieveMetadata: namespace is not a character, doing nothing.")
    return(invisible(NULL))
  }
  if(nchar(namespace)==0){
    warning("retrieveMetadata: namespace is empty, doing nothing.")
    return(invisible(NULL))
  }
  if(!is.character(variableName)){
    warning("retrieveMetadata: variableNames is not a character, doing nothing.")
    return(invisible(NULL))
  }
    if(length(variableName)==0){
     warning("retrieveMetadata: variableName has length zero, doe nothing") 
      return(invisible(NULL))
    }else if(length(variableName)==1){
      return(.Call("C_retrieveMetadata", namespace, variableName, PACKAGE = "memshare"))
    }else{
      return(lapply(variableName, function(variableName,namespace){
        return(.Call("C_retrieveMetadata", namespace, variableName, PACKAGE = "memshare"))
      },namespace))
    }
   
}