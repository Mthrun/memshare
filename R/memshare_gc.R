memshare_gc = function(namespace, cluster = NULL) {
  # memshare_gc(namespace,cluster)
  #
  # This function clears all views and pages for the master session as well as
  # the workers of the given cluster.
  #
  # 
  # INPUT
  # namespace                 The string identifier of the shared memory space.
  # cluster                   The cluster of workers for which the views and pages should be released (default NULL, then only the master session is gc'd)
  #
  #
  #author: JM 08/2025
  
  filterNamespaceVars <- function(strings) {
    if (.Platform$OS.type == "windows") {
      # Pattern: Local\namespace.variableName
      prefix <- paste0("Local\\\\", namespace, "\\.")
      matches <- grep(paste0("^", prefix), strings, value = TRUE)
      sub(paste0("^", prefix), "", matches)
    } else {
      # Pattern: namespace.variableName
      prefix <- paste0(namespace, "\\.")
      matches <- grep(paste0("^", prefix), strings, value = TRUE)
      sub(paste0("^", prefix), "", matches)
    }
  }
  
  
  # remove views
  masterViews = filterNamespaceVars(unlist(memshare::viewList()))
  if (length(masterViews) > 0) {
    memshare::releaseViews(namespace, masterViews)
  }
  
  if (!is.null(cluster)) {
    parallel::clusterExport(cluster, varlist=c("filterNamespaceVars", "namespace"), envir = environment())
    parallel::clusterEvalQ(cluster, {
      workerViews = filterNamespaceVars(unlist(memshare::viewList()))
      if (length(workerViews) > 0) {
        memshare::releaseViews(namespace, workerViews)
      }
    })
  }
  
  # remove pages
  masterPages = filterNamespaceVars(unlist(memshare::pageList()))
  if (length(masterPages) > 0) {
    memshare::releaseVariables(namespace, masterPages)
  }
  
  if (!is.null(cluster)) {
    parallel::clusterEvalQ(cluster, {
      workerPages = filterNamespaceVars(unlist(memshare::pageList()))
      if (length(workerPages) > 0) {
        memshare::releaseVariables(namespace, workerPages)
      }
    })
  }
}