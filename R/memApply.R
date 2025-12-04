memApply = function(X, MARGIN, FUN, NAMESPACE = NULL, CLUSTER=NULL, VARS=NULL, MAX.CORES=NULL) {
    # memApply(cluster, namespace, matAPI, func, margin, sharedAPI)
    #
    # Applies a function to a matrix row- or columnwise in parallel on shared memory.
    # 
    #
    # INPUT
    # X                        Either the target matrix itself or the name of the target matrix in the shared memory space.
    # MARGIN                   Whether to apply to rows or to cols; 1 = rowwise, 2 = columnwise.
    # FUN                      An R function to be applied over the matrix row or columnwise.
    #                          The first argument of func has to be called "v"; the remaining shared variables also have to have the EXACT same name
    #                          in the function call to func if you want to use them internally.
    # NAMESPACE                A string identifier of the shared memory space to work on. If none is given we use the function name in parent environment as default; if the function is a lambda (i.e. defined inplace) we use "unnamed".
    # CLUSTER                  A parallel::makeCluster cluster; if none is given we initialize a new one with MAX.CORES many cores.
    # VARS                     Either a named list of variables or a vector of variable names in a shared memory space to pass to func.
    # MAX.CORES                Maximum number of cores to initialize a new cluster with, default is detectCores()-1
    #
    # OUPUT
    # res                      A list of length nrow(mat) or ncol(mat) (depending on margin), the i-th element containing the results of func for the i-th row or column.
    #
    # NOTE
    #   If you want to also use copied variables (e.g. if it's not worth it sharing it along the threads as its small or it is neither a matrix nor a vector) you
    #   can do this using parallel::clusterExport. The given cluster is used in the calling of func and thus traditional copying of variables into the R-sessions
    #   is enabled this way.
    #author: JM 05/2025
    #1.Editor: MT: 08/25: correction of casts, error catching improvment, warning improvement.
    namespaceSetByUser = !is.null(NAMESPACE)
    if (!namespaceSetByUser) {
        NAMESPACE = deparse(substitute(FUN))
        if (startsWith(NAMESPACE, "function(")) {
            NAMESPACE = "unnamed"
        }
    }

    if (MARGIN != 1 && MARGIN != 2) {
        stop("memApply: MARGIN has to be either 1 (row-wise) or 2 (column-wise)!")
    }


    registeredMat = F
    registeredShared = F

    if (is.null(MAX.CORES)) {
        MAX.CORES = parallel::detectCores() - 1
    }

    noClusterGiven = is.null(CLUSTER)
    if (is.null(CLUSTER)) {
        CLUSTER = parallel::makeCluster(MAX.CORES)
    }

    resultList = tryCatch(
      {
        #MT: correction
        CharCheck=FALSE
        if (is.character(X) && !is.matrix(X)) {
          if (length(X) > 1) {
            stop("memApply: Target matrix has to be a single string when giving the target matrix externally!")
          }
          if (!namespaceSetByUser) {
            stop("memApply: When giving the target matrix by name the namespace field has to be set explicitly!")
          }
          matName = X
          #MT: control flag that omoits further checks, the have to be done in the init procedure elsewhere
          CharCheck=TRUE
          
        }else if(!is.character(X) && !is.matrix(X)){ #maybe a dataframe
          warning("memApply: X was not neither matrix nor character vector, trying to apply as.matrix().")
          X=as.matrix(X)
        }else{
          #do nothing an start next input checking
        }# end if check X as character
        
        #set mode to numeric if it is not so and is not character vector
        if(isFALSE(CharCheck) && mode(X)!="numeric"){
          warning("memApply: X was not not numeric matrix, trying to set mode to numeric.")
          mode(X)=="numeric"
        }
        #MT: correction for non character case
        if(isFALSE(CharCheck) && is.numeric(X) && is.matrix(X)) {
          #check for double
          if (! (is.double(X) && is.null(attr(X, "class")))) {
            #mt correction:            
            warning("memApply: X was not double, resetting storage mode to double.")
            storage.mode(X)="double"
          }
          #mt correction: returns the expression used in the call, not the “object’s variable name”.
          #R has no obligation to pass a symbol — it can (and does) pass expressions.
          s = substitute(X)
          if (is.symbol(s)){
            matName=as.character(s)
          }else{
            matName=paste0("iX", Sys.getpid(), "_", sample.int(1e3, 1))#2 chars plus 5chars plus 3 chars
          } 
          matList = list()
          matList[[matName]] = X
          registerVariables(NAMESPACE, matList)
          registeredMat <- T
        } else {
          #MT: X is not character and somehow not numeric or not matrix
          #should not happen as as.matrix() oder mode(X) should faile earlier, fail save
          if(!isTRUE(CharCheck))
            stop("memApply: Unknown input format for parameter \"X\"!")
        }#end if check X as matrix
        
        if (is.character(VARS) && is.vector(VARS)) {
          if (!namespaceSetByUser) {
            stop("memApply: When giving variables by name the namespace field has to be set explicitly!")
          }
          sharedNames = VARS
        } else if (is.list(VARS) && !is.null(names(VARS)) && length(names(VARS)) == length(VARS)) {
          
          if (!all(unlist(lapply(VARS, function(x) {
            return(is.double(x) && is.null(attr(x, "class")))
          })))) {
            #MT: correction
            warning("memApply: There were non-double matrices/vectors in the VARS, trying to reset storage mode to double.")
            VARS=lapply(VARS, function(x){
              storage.mode(x)="double"
              return(x)
            })
          }#end if check for double
          sharedNames = names(VARS)
          registerVariables(NAMESPACE, VARS)
          registeredShared <- T
        } else if (!is.null(VARS)) {
          stop("memApply: Unknown input format for parameter \"VARS\"!")
        } else {
          sharedNames = NULL
        }# end if check vars
        
        parallel::clusterExport(CLUSTER, list("matName", "sharedNames", "NAMESPACE", "FUN", "MARGIN"), envir = environment())
        
        # Load libraries and retrieve views ONCE per worker
        parallel::clusterEvalQ(CLUSTER, {
          library(Rcpp)
          library(memshare)
          
          # Retrieve and cache views once
          .mat <- memshare::retrieveViews(NAMESPACE, c(matName))
          if (!is.null(sharedNames)) {
            .shared <- memshare::retrieveViews(NAMESPACE, sharedNames)
          } else {
            .shared <- NULL
          }
          
          NULL
        })
        
        # Set up the inner function
        inner_env = new.env(parent = environment(FUN))
        inner_env$FUN = FUN
        inner_env$matName = matName
        inner_env$sharedNames = sharedNames
        inner_env$NAMESPACE = NAMESPACE
        inner_env$MARGIN = MARGIN
        
        inner = function(i) {
          if (MARGIN == 1) {
            v = .mat[[matName]][i, ]
          } else {
            v = .mat[[matName]][, i]
          }
          
          firstArgName <- names(formals(FUN))[1]
          if (!is.null(.shared)) {
            argsList <- c(stats::setNames(list(v), firstArgName), .shared)
          } else {
            argsList <- stats::setNames(list(v), firstArgName)
          }
          
          res = do.call(FUN, argsList)
          return(res)
        }
        
        environment(inner) <- inner_env
        
        matMeta = memshare::retrieveMetadata(NAMESPACE, matName)
        memshare::releaseViews(NAMESPACE, c(matName))
        
        resultList = parallel::parLapply(CLUSTER, 1:matMeta$ncol, inner)
        
        # Release views after computation
        parallel::clusterEvalQ(CLUSTER, {
          memshare::releaseViews(NAMESPACE, c(matName))
          if (!is.null(sharedNames)) {
            memshare::releaseViews(NAMESPACE, sharedNames)
            rm(.shared)
          }
          rm(.mat)
        })
        
        resultList
      },
      error = function(cond) {
        message("memApply:  parApply failed! Here's the original error message:")
        message(conditionMessage(cond))
        NA
      },
      finally = {
        tryCatch(
          {
            parallel::clusterEvalQ(CLUSTER, {
              rm(NAMESPACE, FUN)
              detach("package:memshare", unload = TRUE, character.only = TRUE)
              library(memshare)
            })
            if (registeredShared) {
              parallel::clusterEvalQ(CLUSTER, {
                rm(sharedNames)
              })
              if (registeredMat) {
                parallel::clusterEvalQ(CLUSTER, {
                  rm(matName)
                })
              }
            }
            if (noClusterGiven) {
              parallel::stopCluster(CLUSTER)
            }
          },
          error = function(cond) {
            message("memApply: There was an error in cleanup code! Here's the original error message:")
            message(conditionMessage(cond))
          },
          warning = function(cond) {
            message("memApply: There was a warning in cleanup code! Here's the original warning message:")
            message(conditionMessage(cond))
          }
        )
      }
    )
    on.exit({
      if (registeredShared) {
        releaseVariables(NAMESPACE, sharedNames)
      }
      if (registeredMat) {
        releaseVariables(NAMESPACE, c(matName))
      }
    })
    return(resultList)
}