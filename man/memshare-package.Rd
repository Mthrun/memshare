\name{memshare-package}
\alias{memshare-package}
\alias{memshare}
\docType{package}
\title{
\packageTitle{memshare}
}
\description{
\packageDescription{memshare}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{memshare}
\packageIndices{memsharey}

If the user detaches the package, all handels are destroyed, meaning that all vairables of all namespaces are cleared as long as there is no other r thread still using the variables.

The two basic definitions are:

1. “\code{Pages}” are variables owned by the current compilation unit of the code (e.g., 'R' session or terminal that loaded the DLL). The pages are coded in Windows via 'MapViewOfFile' and on Unix via 'shm'+'mmap'.
		
2. “\code{Views}” are references to variables owned by another (or their own) compilation unit. The views are always 'ALTREP' wrappers for the pointers to the shared memory chunk.  
		
3. "\code{namespace}" are character of length 1 called here strings, that define the identifier of the shared memory context allowing the initialize shared variables.

\strong{Safety}

R itself is designed around a single-threaded C API, which means that internal 
R functions and memory management cannot be called safely from multiple threads 
at the same time. Each worker process created by \pkg{parallel} runs its own 
independent R interpreter, so ordinary R code is safe as long as all 
R-level operations happen inside one worker at a time.

However, shared-memory buffers created by \pkg{memshare} are visible to multiple 
R sessions simultaneously. These buffers are intended to be \emph{read-shared}: 
many workers can read the same matrix or vector concurrently without conflict.  
If you ever modify a shared object in place (e.g., \code{X[1,1] <- 0}), that write 
immediately affects the shared buffer seen by other workers and can cause 
data races if they also access the same region. To avoid this, treat shared 
variables as read-only, or implement explicit synchronization mechanisms such as 
interprocess locks or task partitioning that guarantees non-overlapping writes.

Inside compiled code (e.g., via Rcpp, OpenMP, or TBB), threads may perform 
numerical computations on raw pointers to shared data, but must never call 
back into R (e.g., create R objects, print, evaluate R expressions, etc.) from 
those secondary threads. All communication with R must occur in the main thread 
of each worker process.

\strong{Resource lifecycle}

Shared memory behaves differently from ordinary R objects because it exists 
outside the R garbage collector. Therefore, \pkg{memshare} provides explicit 
functions to manage it.

Each time you call \code{\link{retrieveViews}}, the package creates one or more 
"handles" that link your R objects to the shared-memory segments. When you are 
done using these views, always call \code{\link{releaseViews}} to remove those 
handles. As long as any active view exists in any R session, the corresponding 
shared memory remains allocated.

Memory can only be reclaimed once all views have been released.  
The call to \code{\link{releaseVariables}} from the master session removes ownership 
of the pages, but they are physically unmapped only when no process holds a view.  
Detaching or unloading the \pkg{memshare} package automatically drops all handles, 
but if another R session still has an open view, that memory remains in use until 
it is released there as well. This design prevents dangling pointers and ensures 
that shared data are not invalidated while still in use by another worker.
}
\author{
\packageAuthor{memshare}

Maintainer: \packageMaintainer{memshare}
}

\keyword{ package }

\examples{
x = rnorm(100)
y = runif(100)
Mat = cbind(x,x,x)
res = memApply(X = Mat, MARGIN = 2,
FUN = function(x,y) {
  cc = memshare::mutualinfo(x,y,isYDiscrete = TRUE,
                      na.rm = TRUE,useMPMI = FALSE)
  return(cc)
},VARS = list(y=y),MAX.CORES=1, #for testing purposes only single thread
NAMESPACE = "namespaceID")
unlist(res)
\dontrun{
	#usually MAX.CORES>1 for application
	
	#alternative usage with manual memory allocation:
}
Data = cbind(x, x, x)
namespace = "ns_package"
memshare::registerVariables(namespace, list(Data = Data, y = y))
res2 = memshare::memApply(
  X = "Data",
  MARGIN = 2,
  FUN = function(x, y) {
    cc = memshare::mutualinfo(x,
                              y,
                              isYDiscrete = TRUE,
                              na.rm = TRUE,
                              useMPMI = FALSE)
    return(cc)
  },
  VARS = c("y"),
  MAX.CORES = 1,
  #for testing purposes only single thread
  NAMESPACE = namespace
)
unlist(res2)
memshare::releaseVariables(namespace, c("Data", "y"))
}
