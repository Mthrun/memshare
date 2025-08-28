\name{releaseVariables}
\alias{releaseVariables}
\title{ Function to delete variables from a shared memory space. }
\description{
  Given a namespace identifier (identifies the shared memory space to register to), this function releases given variables from the shared memory space.
}
\usage{
  releaseVariables(namespace, variableNames)
}
\arguments{
    \item{namespace}{ string of the identifier of the shared memory context. }
  \item{variableNames}{ A character vector of variable names to delete. }
}

\author{ Julian Maerte }

\seealso{ \code{\link{releaseVariables}}, \code{\link{retrieveViews}} }
\examples{
  # MASTER SESSION:
  # allocate data, call calculation, free data
  n = 1000
  m = 100

  NumMatrix = matrix(rnorm(n * m), n, m) # target matrix
  yvec = rnorm(n) # some other constant vector in which the function should not run

  namespace = "my_namespace_rel"
  memshare::registerVariables(namespace, list(NumMatrix=NumMatrix, yvec=yvec))

  # Perform your shared calculations here

  memshare::releaseVariables(namespace, c("NumMatrix", "yvec"))
}
\concept{ shared memory }
\keyword{ multithreading }
