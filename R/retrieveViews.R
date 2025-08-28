retrieveViews <- function(namespace, variableNames) {
    # retrieveVariables(namespace, variableNames)
    #
    # A function to retrieve shared memory variables from a shared memory space.
    # 
    #
    # INPUT
    # namespace                The string identifier of the shared memory space.
    # variableNames            A vector of variable names of the variables to retrieve from the namespace
    #
    # OUPUT
    # res                      A named list mapping the variable names to their retrieved shared memory ALTREP mockups. Matrices behave the exact same as matrices and vectors the exact same as vectors.
    #
    # NOTE
    #   The produced variables are "views" of the raw C++ shared memory. There might be some weird behavior when trying to print out the loaded matrix.
    #   For the purposes of calculation and code-guarding (e.g. is.matrix, is.numeric aswell as numerical operations) the objects behave the exact same as their original R clones.
    #   Note however that when retrieving Variables this way and modifying them that:
    #       1. The changes are persistent (i.e. retrieving the same variable at a later point will have the earlier changes)
    #       2. When working with multiple R sessions on the same shared memory space the changes are propagated to all sessions in real time and so side-effects have to be considered!
    #       3. Working with multiple R sessions on the exact same memory position is generally unsafe. You can however modify the same matrix from different R threads as long as you work on disjoint memory sections of it (e.g. different rows/cols)
    #author: JM 05/2025
  
  #mt: error catching
  if(!is.character(namespace)){
    warning("retrieveViews: namespace is not a character, doing nothing.")
    return(invisible(NULL))
  }
  if(nchar(namespace)==0){
    warning("retrieveViews: namespace is empty, doing nothing.")
    return(invisible(NULL))
  }
  if(length(retrieveViews)==0){
    warning("retrieveViews: variableName has length zero, doe nothing") 
    return(invisible(NULL))
  }
  if(!is.character(variableNames)){
    if(is.list(variableNames)){
      warning("retrieveViews: variableNames is a list and not a character vector, doing nothing.")
    }else{
      warning("retrieveViews: variableNames is not a character, doing nothing.")
    }
    return(invisible(NULL))
  }
    .Call("C_retrieveViews", namespace, variableNames, PACKAGE = "memshare")
}