\name{pageList}
\alias{pageList}
\title{ Function to obtain a list of the registered variables of the current session. }
\description{
  When your current session has registered shared memory variables via \code{\link{registerVariables}} internally the variable is tracked until it is released via \code{\link{releaseVariables}}.
  
  This function serves as a tool to check whether all variables have been free'd after usage or to see what variables are currently held by the session.
}
\usage{
  pageList()
}

\value{
An [1:m] list of characters of the registered p namespaces, each of them having up to k variables, m<=p*k. Each element of the list is a combination of namespace and variable name
}

\details{
The string of each element of the output list has the format environment, backslash, backslash <namespace name>.<variable name>. Default is lokal environment.
}
\author{ Julian Maerte }

\seealso{ \code{\link{registerVariables}}, \code{\link{releaseVariables}} }
\examples{
  pageList()
  \dontrun{
  # = list()
  }
  mat = matrix(0,5,5)
  registerVariables("namespace_pageL", list(mat=mat))
  pageList()
  \dontrun{
  # = list("mat")
  }
  releaseVariables("namespace_pageL", c("mat"))
  pageList()
  \dontrun{
  # = list()
  }
}
\concept{ shared memory }
\keyword{ multithreading }
