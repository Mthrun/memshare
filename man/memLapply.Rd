\name{memLapply}
\alias{memLapply}
\title{ Analog of \code{\link[parallel]{parLapply}} function for a shared memory context. Runs a worker pool on the exact same memory (shared memory context). }
\description{
  \code{memLapply} mirrors \code{\link[parallel]{parLapply}} in the shared memory setting. Given a shared memory space \code{namespace} with a target list \code{X} and some shared variables \code{VARS} either as list of variables or as their names in the memory space,
  \code{memLapply} allows you to apply a function \code{FUN} elementwise over the target list.
  Since the memory is shared only the names have to be copied to each worker thread in \code{CLUSTER} (a \code{\link[parallel]{makeCluster}} multithreading cluster) resulting in sharing of arbitrarily large matrices (as long as the fit in RAM once) along a \code{parallel} cluster while only copying a couple of bytes per cluster.

  It is recommended not to change the values of \code{el} inside \code{FUN}, however this will only lead to some copying of the column whenever it is worked upon; the shared memory thus will not be corrupted even if you write to col. Also the copying only ever happens for one column at a time leading to much lower memory consumption than parallel even in this case.
}
\usage{
  memLapply(X, FUN, 
  
  NAMESPACE = NULL, CLUSTER = NULL, VARS=NULL, MAX.CORES = NULL)
}
\arguments{
  \item{X}{ Either a 1:n list object or a the name of an already registered list object in \code{NAMESPACE}. }
  \item{FUN}{ Function to be applied over the list. The first argument will be set to the list element, the remaining ones have to have the same name as they have in the shared memory space! }
  \item{NAMESPACE}{Optional, string. The namespace identifier for the shared memory session. If this is \code{NULL} it will be set to the name of FUN in runtime environment. However for inline-defined functions FUN an explicit NAMESPACE is recommended. }
  \item{CLUSTER}{Optional, A parallel::makeCluster cluster. Will be used for parallelization. By defining clusterExport constant R-copied objects (non-shared) can be shared among different executions of FUN. If \code{NULL} we initialize a new one. }
  \item{VARS}{Optional, Either a named list of variables where the name will be the name under which the variable is registered in shared memory space or a character vector of names of variables already registered which should be provided to FUN. }
  \item{MAX.CORES}{Optional, In case CLUSTER is undefined a new cluster with \code{MAX.CORES} many cores will be initialized. If \code{NULL} we use \code{detectCores() - 1} many. }
}
\value{
  \item{result}{A 1:n list of the results of func(list[[i]],...), for every element of listName.}
}

\author{ Julian Maerte }

\seealso{ \code{\link[parallel]{parLapply}} }
\examples{
  list_length = 1000
  matrix_dim = 100

  l <- lapply(
      1:list_length,
      function(i) matrix(rnorm(matrix_dim * matrix_dim),
      nrow = matrix_dim, ncol = matrix_dim))

  y = rnorm(matrix_dim)

  namespace = "my_namespace"
  res = memshare::memLapply(l, function(el, y) {
    el %*% y
  }, NAMESPACE=namespace, VARS=list(y=y), MAX.CORES = 1)
}
\keyword{ memApply }
\keyword{ multithreading }
