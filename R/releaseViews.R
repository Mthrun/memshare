releaseViews <- function(namespace, variableNames) {
    # retrieveVariables(namespace, variableAPIs)
    #
    # A function to retrieve shared memory variables from a shared memory space.
    # 
    #
    # INPUT
    # namespace                The string identifier of the shared memory space.
    # variableNames            A character vector of variable names of the variables to retrieve from the namespace
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
      warning("releaseViews: namespace is not a character, doing nothing.")
      return(invisible(NULL))
    }
    if(nchar(namespace)==0){
      warning("releaseViews: namespace is empty, doing nothing.")
      return(invisible(NULL))
    }
    if(length(variableNames)==0){
      warning("releaseViews: variableName has length zero, doe nothing") 
      return(invisible(NULL))
    }
  if(!is.character(variableNames)){
    if(is.list(variableNames)){
      warning("releaseViews: variableNames is a list and not a character vector, doing nothing.")
    }else{
      warning("releaseViews: variableNames is not a character, doing nothing.")
    }
    return(invisible(NULL))
  }
    #mt correction to make return invisible
    return(invisible(.Call("C_releaseViews", namespace, variableNames, PACKAGE = "memshare")))
}