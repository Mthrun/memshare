registerVariables <- function(namespace, variableList) {
    # registerVariables(namespace,variableList)
    #
    # A function to register R matrices/vectors as shared matrices/vectors in a shared memory space.
    #
    # 
    # INPUT
    # namespace                 The string identifier of the shared memory space.
    # variableList              A named list mapping each variable name to be registered to the value it should be registered as.
    #                           E.g. list(mat1=matrix(rnorm(1000 * 100), 1000, 100), vec=rnorm(1000), mat2=matrix(rnorm(50 * 200), 50, 200))
    #                           Registers in namespace three fields, mat1, vec and mat2, and copies into them the given matrices for later retrieval.
    #
    #
    #author: JM 05/2025
    #1. Editor: MT 08/2025: Input handling improved, error catching added, automatic casting of doubles for non lists
  
  if(!is.character(namespace)){
    warning("registerVariables: namespace is not a character, trying to call as.character.")
    namespace=as.character(namespace)
  }
  if(nchar(namespace)==0){
    warning("retrieveMetadata: namespace is empty, doing nothing.")
    return(invisible(NULL))
  }
  
  if(!is.list(variableList)){
    stop("registerVariables: variableList is not a list, trying to set as list.")
  }
  
  if(length(variableList)==0){
    warning("registerVariables: variableList is empty. Doing nothing.")
    return(invisible(NULL))
  }

  if(is.null(names(variableList))){
    warning("registerVariables: variableList is not named, setting names to L1 to LN.")
    names(variableList)=paste0("L",1:length(variableList))
  }else{
    ind=which(names(variableList)=="")
    if(length(ind)>0){
      warning("registerVariables: some elements of variableList having empty names, setting these names to L1 to LN.")
      names(variableList)[ind]=paste0("L",1:length(ind))
    }
  }
  
  #Identify which elements should be checked (skip lists)
    need_fix <- vapply(variableList, function(x) {
      # only check non-lists
      if (is.list(x)) return(FALSE)
      # needs fix if NOT (double and no class)
      !(is.double(x) && is.null(attr(x, "class")))
    }, logical(1L))
    
    if (any(need_fix)) {
      warning("registerVariables: There were non-double matrices/vectors in variableList (non-list elements). Resetting storage mode to double.")
      variableList[need_fix] <- lapply(variableList[need_fix], function(x) {
        storage.mode(x) <- "double"
        x
      })
    }
  
    return(invisible(.Call("C_registerVariables", namespace, variableList, PACKAGE = "memshare")))
}