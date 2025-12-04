memLapply = function(X, FUN, NAMESPACE = NULL, CLUSTER = NULL, VARS=NULL, MAX.CORES = NULL) {
    # memApply(cluster, namespace, listName, func, sharedNames)
    #
    # Applies a function to each element of a list in parallel on shared memory.
    # 
    #
    # INPUT
    # X                        The target list or the name of the target list in the shared memory space.
    # FUN                      An R function to be applied over the list.
    #                          The first argument of func has to be called "el"; the remaining shared variables also have to have the EXACT same name
    #                          in the function call to func if you want to use them internally.
    # NAMESPACE                A string identifier of the shared memory space to work on; if none is given we use the name of FUN in the parent scope; if FUN is a lambda (i.e. defined inplace) we use "unnamed".
    # CLUSTER                  A parallel::makeCluster cluster, if none is given we initialize a new one with MAX.CORES many cores.
    # VARS                     Either a named list of variables or a vector of variable names in a shared memory space to pass to func. 
    # MAX.CORES                Maximum number of cores to initialize a new cluster with, default is detectCores()-1.
    #
    # OUPUT
    # res                      A list of length length({{listName}}), the i-th element being the results of func for the i-th element.
    #
    # NOTE
    #   If you want to also use copied variables (e.g. if it's not worth it sharing it along the threads as its small or it is neither a matrix nor a vector) you
    #   can do this using parallel::clusterExport. The given cluster is used in the calling of func and thus traditional copying of variables into the R-sessions
    #   is enabled this way.
    #author: JM 06/2025

    namespaceSetByUser = !is.null(NAMESPACE)
    if (!namespaceSetByUser) {
        NAMESPACE = deparse(substitute(FUN))
        if (startsWith(NAMESPACE, "function(")) {
            NAMESPACE = "unnamed"
        }
    }

    registeredList = F
    registeredShared = F

    if (is.null(MAX.CORES)) {
        MAX.CORES = parallel::detectCores() - 1
    }

    if (is.character(X)) {
        if (length(X) > 1) {
            stop("memLapply: Target list has to be a single string when giving the target matrix externally!")
        }
        if (!namespaceSetByUser) {
            stop("memLapply: When giving the target list by name the namespace field has to be set explicitly!")
        }
        listName = X
    } else if (is.list(X)) {
        #listName = deparse(substitute(X))
        #mt correction: returns the expression used in the call, not the “object’s variable name”.
        #R has no obligation to pass a symbol — it can (and does) pass expressions.
        s = substitute(X)
        if (is.symbol(s)){
          listName=as.character(s)
        }else{
          listName=paste0("iL", Sys.getpid(), "_", sample.int(1e3, 1))#2 chars plus 5chars plus 3 chars
        } 
        
        listList = list()
        listList[[listName]] = X
        registerVariables(NAMESPACE, listList)
        registeredList = T
    } else {
        stop("memLapply: Unknown input format for parameter \"X\"!")
    }

    if (is.character(VARS) && is.vector(VARS)) {
        if (!namespaceSetByUser) {
            stop("memLapply: When giving variables by name the namespace field has to be set explicitly!")
        }
        sharedNames = VARS
    } else if (is.list(VARS) && !is.null(names(VARS)) && length(names(VARS)) == length(VARS)) {
        sharedNames = names(VARS)
        registerVariables(NAMESPACE, VARS)
        registeredShared=T
    } else if (!is.null(VARS)) {
        stop("memLapply: Unknown input format for parameter \"VARS\"!")
    } else {
        sharedNames = NULL
    }


    noClusterGiven = is.null(CLUSTER)
    if (is.null(CLUSTER)) {
        CLUSTER = parallel::makeCluster(MAX.CORES)
    }


    resultList = tryCatch(
        {
            parallel::clusterExport(CLUSTER, list("listName", "sharedNames", "NAMESPACE", "FUN"), envir = environment())
            parallel::clusterEvalQ(CLUSTER, {
                library(Rcpp)
                library(memshare)
            })

            inner_env = new.env(parent = environment(FUN))
            inner_env$FUN = FUN
            inner_env$listName = listName
            inner_env$sharedNames = sharedNames
            inner_env$NAMESPACE = NAMESPACE
            inner_env$retrieveViews = memshare::retrieveViews

            inner = function(i) {
                l = retrieveViews(NAMESPACE, c(listName))
                
                firstArgName <- names(formals(FUN))[1]
                if (!is.null(sharedNames)) {
                    sharedVariables = retrieveViews(NAMESPACE, sharedNames)
                    argsList <- c(stats::setNames(list(l[[listName]][[i]]), firstArgName), sharedVariables)
                } else {
                    argsList <- stats::setNames(list(l[[listName]][[i]]), firstArgName)
                }
            
                res = do.call(FUN, argsList)

                releaseViews(NAMESPACE, c(listName))
                if (!is.null(sharedNames)) {
                    releaseViews(NAMESPACE, sharedNames)
                }
                return(res)
            }

            environment(inner) <- inner_env


            listMeta = retrieveMetadata(NAMESPACE, listName)
            
            resultList = parallel::parLapply(CLUSTER, 1:listMeta$n, inner)
            releaseViews(NAMESPACE, c(listName))
            
            resultList
        },
        error = function(cond) {
            message("memLapply: parLapply failed! Here's the original error message:")
            message(conditionMessage(cond))
            # Choose a return value in case of error
            NA
        },
        finally = {
            tryCatch(
                {
                    parallel::clusterEvalQ(CLUSTER, {
                        rm(listName, sharedNames, NAMESPACE, FUN)
                        detach("package:memshare", unload = TRUE, character.only = TRUE)
                        library(memshare)
                    })
                    if (noClusterGiven) {
                        parallel::stopCluster(CLUSTER)
                    }
                },
                error = function(cond) {
                    message("memLapply: There was an error in cleanup code! Here's the original error message:")
                    message(conditionMessage(cond))
                },
                warning = function(cond) {
                    message("memLapply: There was a warning in cleanup code! Here's the original warning message:")
                    message(conditionMessage(cond))
                }
            )
        }
    )
    on.exit({
      if (registeredList) {
        releaseVariables(NAMESPACE, c(listName))
      }
      if (registeredShared) {
        releaseVariables(NAMESPACE, sharedNames)
      }
    })
    return(resultList)
}