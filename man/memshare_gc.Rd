\name{memshare_gc}
\alias{memshare_gc}
\title{ Function to remove all handles (ownership and viewership) for a namespace in a worker context. }
\description{
  Given a namespace identifier (identifies the shared memory space to register to), this function removes all handles to shared memory held by the master and a worker context.
}
\usage{
  memshare_gc(namespace, cluster)
}
\arguments{
  \item{namespace}{ The identifier of the shared memory context. }
  \item{cluster}{ A worker context (parallel cluster) that holds views or pages in the same memory context as the master. NULL by default; then only the master session gets its handles removed. }
}

\author{ Julian Maerte }

\seealso{ \code{\link{releaseVariables}}, \code{\link{releaseViews}} }
\examples{
  library(parallel)
  library(memshare)
  cluster = makeCluster(1)
  
  namespace = "namespace"
  mat = matrix(0,5,5)
  registerVariables(namespace, list(mat=mat))
  
  clusterEvalQ(cluster, {
    library(memshare)
    view = retrieveViews("namespace", c("mat"))
  })
  # At this point each worker holds a view of mat
  
  memshare_gc("namespace", cluster)
  # Every workers viewership handle gets destroyed, master sessions page handle
  # gets destroyed.
  # As no handles are left open, the memory is free'd.
  stopCluster(cluster)
}
\concept{ shared memory }
\keyword{ multithreading }
