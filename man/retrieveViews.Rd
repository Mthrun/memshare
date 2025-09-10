\name{retrieveViews}
\alias{retrieveViews}
\title{ Function to obtain an '\code{ALTREP}' representation of variables from a shared memory space. }
\description{
  Given a namespace identifier (identifies the shared memory space to register to), this function constructs mocked matrices/vectors (depending on the variable type) pointing to 'C++' shared memory instead of 'R'-internal memory state.
  The mockup is constructed as an '\code{ALTREP}' object, which is an \pkg{Rcpp} wrapper around 'C++' raw memory. 'R' thinks of these objects as common matrices or vectors.

  The variables content can be modified, resulting in modification of shared memory. Thus when not using wrapper functions like \code{\link{memApply}} or \code{\link{memLapply}} the user has to be cautious of the side-effects an 'R' session working on shared memory has on other 'R' sessions working on the same namespace.
  
  NOTE: Having a view of a memory chunk introduces an internally tracked handle to the shared memory. Shared memory is not deleted until all handles are gone; before calling \code{\link{releaseVariables}} in the master session, you have to free all view-initialized handles via \code{\link{releaseViews}}!
}
\usage{
  retrieveViews(namespace, variableNames)
}
\arguments{
  \item{namespace}{ string of the identifier of the shared memory context. }
  \item{variableNames}{[1:n] character vector, the names of the variables to retrieve from the shared memory space. }
}

\value{
  An 1:p list of p elements, each element contains a variable that was registered by \code{\link{registerVariables}}
}

\author{ Julian Maerte }

\seealso{ \code{\link{releaseVariables}}, \code{\link{registerVariables}}, \code{\link{releaseViews}} }
\examples{
  \dontrun{ 
  # MASTER SESSION:
  # init some data and make shared
  }
  n = 1000
  m = 100

  mat = matrix(rnorm(n * m), n, m) # target matrix
  y = rnorm(n) # some other constant vector in which the function should not run

  namespace = "ns_retrview"
  memshare::registerVariables(namespace, list(mat=mat, y=y))
  \dontrun{
  # WORKER SESSION
  # retrieve the shared data and work with it
  }
  res = memshare::retrieveViews(namespace, c("mat", "y"))
  \dontrun{
  # res is a list of the format:
  # list(mat=matrix_altrep, y=vector_altrep),
  # altrep-variables can be used
  # exactly the same way as a matrix or vector
  # and also behave like them when checking via
  # is.matrix or is.numeric.


  # important: Free view before resuming
  # to master session to release the variables!
  }
  memshare::releaseViews(namespace, c("mat", "y"))
  
  \dontrun{
  # MASTER SESSION
  # After all view handles have been free'd, release the variable
  }
  memshare::releaseVariables(namespace, c("mat", "y"))
}
\concept{ shared memory }
\keyword{ multithreading }
