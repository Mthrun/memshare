\name{memApply}
\alias{memApply}
\title{ Analog of \code{\link[parallel]{parApply}} function for a shared memory context. }
\description{
  \code{memApply} mirrors \code{\link[parallel]{parApply}} in the shared memory setting given a shared memory space \code{namespace} with a target matrix \code{X} and some shared variables \code{VARS} either as variables or as names of their registered variables.
 }
\usage{
  memApply(X, MARGIN, FUN, 
  
  NAMESPACE = NULL, CLUSTER=NULL, VARS=NULL, MAX.CORES=NULL)
}
\arguments{
  \item{X}{ A [1:n,1:d] numerical matrix of n rows and d columns which is worked upon. Can also be a string name of an already registered variable in \code{NAMESPACE}; otherwise will be registered automatically. }
  \item{MARGIN}{ Whether to apply by row (1) or column (2). }
  \item{FUN}{ Function that is applied on either the rows or columns of \code{X}. The first argument will be set to the vector and the subsequent arguments have to have the same name as their registered variables. }
  \item{NAMESPACE}{Optional, string. The namespace identifier for the shared memory session. If this is \code{NULL} it will be set to the name of FUN in runtime environment. However for inline-defined functions FUN an explicit NAMESPACE is recommended. }
  \item{CLUSTER}{Optional, A parallel::makeCluster cluster. Will be used for parallelization. By defining clusterExport constant R-copied objects (non-shared) can be shared among different executions of FUN. If \code{NULL} we initialize a new one. }
  \item{VARS}{Optional, Either a named list of variables where the name will be the name under which the variable is registered in shared memory space or a character vector of names of variables already registered which should be provided to FUN. }
  \item{MAX.CORES}{Optional, In case CLUSTER is undefined a new cluster with \code{MAX.CORES} many cores will be initialized. If \code{NULL} we use \code{detectCores() - 1} many. }
}
\value{
  \item{result}{A list of the results of func(row,...) of size n or func(col, ...) of size d, depending on \code{MARGIN}, for every row/col of \code{X}.}
}
\details{
 \code{memApply} runs a worker pool on the exact same memory (for shared memory context, see \code{\link{registerVariables}}), and allows you to apply a function \code{FUN} row- or columnwise (depending on \code{MARGIN}) over the target matrix.
  Since the memory is shared only the names of variables have to be copied to each worker thread in \code{CLUSTER} (a \code{\link[parallel]{makeCluster}} multithreading cluster) resulting in sharing of arbitrarily large matrices (as long as the fit in RAM once) along a \pkg{parallel} cluster while only copying a couple of bytes per cluster.

 The numerical matrix X and the Vars havee to be objects of base type '\code{double}'.

  It is recommended not to change the values of \code{v} inside \code{FUN}, however this will only lead to some copying of the column whenever it is worked upon; the shared memory thus will not be corrupted even if you write to column or row. Also the copying only ever happens for one column/row at a time leading to much lower memory consumption than parallel even in this case.
  
 \strong{Thread safety}
 
The vector \code{v} passed to \code{FUN} is typically an ALTREP view that 
\emph{directly references shared memory} rather than a private copy.  
This means that multiple worker processes may be reading the same memory region 
simultaneously.  

\emph{Read-only operations are fully safe and recommended.} Examples include 
statistical summaries (\code{mean(v)}, \code{cor(v, y)}), vectorized arithmetic, 
and model-fitting that does not modify \code{v}.  

If you attempt to modify elements of \code{v} directly (for example, 
\code{v[1] <- 0}), you are writing into a shared buffer.  
Concurrent modification by multiple workers can lead to race conditions 
or data corruption. Even if no other process is writing, in-place assignment 
may still trigger an internal copy of that row or column, slightly increasing 
memory usage.  

For safety and clarity, always \emph{copy} \code{v} locally if you need to modify it:
\preformatted{
f <- function(v, y) {
  v <- as.vector(v)  # make a private, normal R copy
  v <- scale(v)
  cor(v, y)
}
}
This ensures isolation between workers and prevents unintended data sharing.  

Finally, remember that R's internal C API is not thread-safe.  
If your function \code{FUN} uses multi-threaded C++ code (e.g., via OpenMP or TBB), 
those internal threads must \emph{not} make calls into R (such as creating 
objects, evaluating expressions, or printing).  
All R interactions must occur in the main thread of each worker process.

}

\author{ Julian Maerte }

\seealso{ \code{\link[parallel]{parApply}} }
\examples{
  library(parallel)
  cl = makeCluster(1)
  i = 1
  A1 = matrix(as.double(1:10^(i+1)),10^i, 10^i)
  
  res = memApply(X = A1, MARGIN = 2, FUN = function(x) {
    return(sd(x))
  }, CLUSTER=cl, NAMESPACE="ns_apply")
  
  SD_vector=unlist(res)
}
\keyword{ memApply }
\keyword{ multithreading }
