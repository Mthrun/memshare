\name{memLapply}
\alias{memLapply}
\title{ Analog of \code{\link[parallel]{parLapply}} function for a shared memory context. }
\description{
  \code{memLapply} mirrors \code{\link[parallel]{parLapply}} in the shared memory setting given a shared memory space \code{namespace} with a target list \code{X} and some shared variables \code{VARS} either as list of variables or as their names in the memory space,
 }
\usage{
  memLapply(X, FUN, 
  
  NAMESPACE = NULL, CLUSTER = NULL, VARS=NULL, MAX.CORES = NULL)
}
\details{
  \code{memLapply} runs a worker pool on the exact same memory (shared memory context), and allows you to apply a function \code{FUN} elementwise over the target list.
  Since the memory is shared only the names have to be copied to each worker thread in \code{CLUSTER} (a \code{\link[parallel]{makeCluster}} multithreading cluster) resulting in sharing of arbitrarily large matrices (as long as the fit in RAM once) along a \pkg{parallel} cluster while only copying a couple of bytes per cluster.
 It is recommended not to change the values of the list element \code{el} inside \code{FUN}, however this will only lead to some copying of the element whenever it is worked upon; the shared memory thus will not be corrupted even if you write to an element. Also the copying only ever happens for one element at a time leading to much lower memory consumption than parallel even in this case.

\strong{Thread safety}  

Each element \code{el} provided to \code{FUN} is typically an ALTREP view 
of a shared-memory object rather than an ordinary R copy.  
This means that workers can access the same physical memory region concurrently.  

\emph{Read-only operations are fully safe and recommended.} Examples include 
computations such as matrix multiplication, summary statistics, or transformations 
that return new results without altering \code{el} in place.  
Because the data are shared, these operations require almost no additional memory 
and avoid costly data duplication.  

If your function modifies an element directly (e.g., \code{el[1,1] <- 0}), you may 
be writing to a shared memory buffer that other workers can also access.  
This can cause inconsistent results or data corruption if multiple workers write 
simultaneously. Even when no overlap exists, such in-place modification can trigger 
a copy of the element inside that worker, reducing memory efficiency.  

For safety, make an explicit local copy before any in-place changes:
\preformatted{
f <- function(el, y) {
  el <- as.matrix(el)  # force a private copy
  el <- el * y         # modify locally
  colSums(el)
}
}
This ensures that only the current worker modifies its own private memory.  

Finally, note that R’s internal C API is single-threaded.  
If \code{FUN} calls compiled code (e.g., via Rcpp, OpenMP, or TBB) that 
spawns multiple threads, those threads must not interact directly with R 
(e.g., by creating R objects, printing, or evaluating expressions).  
All such interactions must occur in the main R thread of each worker process.

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

  l = lapply(
      1:list_length,
      function(i) matrix(rnorm(matrix_dim * matrix_dim),
      nrow = matrix_dim, ncol = matrix_dim))

  y = rnorm(matrix_dim)

  namespace = "ns_lapply"
  res = memshare::memLapply(l, function(el, y) {
    el %*% y
  }, NAMESPACE=namespace, VARS=list(y=y), MAX.CORES = 1)
}
\keyword{ memApply }
\keyword{ multithreading }
