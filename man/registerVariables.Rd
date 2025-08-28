\name{registerVariables}
\alias{registerVariables}
\title{ Function to register variables in a shared memory space. }
\description{
  Given a namespace identifier (identifies the shared memory space to register to), this function allows you to allocate shared memory and copy data into it for other R sessions to access it.
}
\usage{
  registerVariables(namespace, variableList)
}
\arguments{
  \item{namespace}{ string of the identifier of the shared memory context. }
  \item{variableList}{ A named list of variables to register. Currently supported are matrices and vectors. }
}

\author{ Julian Maerte }

\seealso{ \code{\link{releaseVariables}}, \code{\link{retrieveViews}} }
\examples{
  library(memshare)
  n = 10
  m = 10

  TargetMat= matrix(rnorm(n * m), n, m) # target matrix
  x_vec = rnorm(n) # some other vector

  namespace = "my_namespace_reg_ex"
  registerVariables(namespace, list(TargetMat=TargetMat, x_vec=x_vec))
  memshare::releaseVariables(namespace, c("TargetMat", "x_vec"))
}
\concept{ shared memory }
\keyword{ multithreading }
