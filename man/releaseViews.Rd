\name{releaseViews}
\alias{releaseViews}
\title{ Function to release views of a shared memory space. }
\description{
  Given a namespace identifier (identifies the shared memory space to register to), this function releases retrieved views from the shared memory space.

  NOTE: All views have to be free'd upon releasing the variable by the master.
}
\usage{
  releaseViews(namespace, variableNames)
}
\arguments{
  \item{namespace}{ The identifier of the shared memory context. }
  \item{variableNames}{ A character vector of variable names to delete. }
}

\author{ Julian Maerte }

\seealso{ \code{\link{retrieveViews}}, \code{\link{registerVariables}} }
\examples{
  # MASTER SESSION:
  # allocate data
  n = 1000
  m = 100

  mat = matrix(rnorm(n * m), n, m) # target matrix
  y = rnorm(n) # some other constant vector in which the function should not run

  namespace = "my_namespace_rel_view"
  memshare::registerVariables(namespace, list(mat=mat, y=y))
  
  # WORKER SESSION:
  res = retrieveViews(namespace, c("mat", "y"))
  # Perform your shared calculations here
  releaseViews(namespace, c("mat", "y"))

  # MASTER SESSION:
  # free memory
  memshare::releaseVariables(namespace, c("mat", "y"))
}
\concept{ shared memory }
\keyword{ multithreading }
