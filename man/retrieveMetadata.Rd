\name{retrieveMetadata}
\alias{retrieveMetadata}
\title{ Function to obtain the metadata of a variable from a shared memory space. }
\description{
  Given a namespace identifier (identifies the shared memory space to register to), this function retrieves the metadata of the stored variable.

  NOTE: If no view of the variable was previously retrieved this implicitly retrieves a view and thus has to free'd afterwards!
}
\usage{
  retrieveMetadata(namespace, variableName)
}
\arguments{
  \item{namespace}{ string of the identifier of the shared memory context. }
  \item{variableName}{ [1:m] character vector, names of one ore more than one variable to retrieve the metadata from the shared memory space. }
}

\value{
 A [1:m] named list mapping the variable names to their retrieved metadata. Each list element contains a list of two elements called "\code{type}" and length "\code{n}"
}

\author{ Julian Maerte }

\seealso{ \code{\link{releaseVariables}}, \code{\link{releaseViews}}, \code{\link{registerVariables}} }
\examples{
  \dontrun{
  # MASTER SESSION:
  # allocate data
  }
  n = 1000
  m = 100

  mat = matrix(rnorm(n * m), n, m) # target matrix

  namespace = "my_namespace_meta"
  memshare::registerVariables(namespace, list(mat=mat))
  \dontrun{
  # WORKER SESSION:
  # retrieve metadata of the variable
  }
  res = memshare::retrieveMetadata(namespace, "mat")
  \dontrun{
  # res$type = "matrix"
  # res$nrow = 1000
  # res$ncol = 100
  }
  releaseViews(namespace, c("mat"))
  \dontrun{
  # MASTER SESSION:
  # free memory
  }
  memshare::releaseVariables(namespace, c("mat"))
}
\concept{ shared memory }
\keyword{ multithreading }
