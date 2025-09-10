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
\value{
  No return value, called for deallocation of memory pages.
}

\author{ Julian Maerte }

\seealso{ \code{\link{releaseVariables}}, \code{\link{retrieveViews}} }
\examples{
  \dontrun{
  # MASTER SESSION:
  # allocate data, call calculation, free data
  }
  n = 1000
  m = 100

  NumMatrix = matrix(rnorm(n * m), n, m) # target matrix
  yvec = rnorm(n) 
  \dontrun{
  # yvec os some other constant vector 
  # in which the function should not run
  }
  namespace = "ns_release"
  memshare::registerVariables(namespace, list(NumMatrix=NumMatrix, yvec=yvec))
  \dontrun{
  # Perform your shared calculations here
  }
  memshare::releaseVariables(namespace, c("NumMatrix", "yvec"))
}
\concept{ shared memory }
\keyword{ multithreading }
