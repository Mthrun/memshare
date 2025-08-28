\name{viewList}
\alias{viewList}
\title{Function to obtain a list of the views the current session holds. }
\description{
  When your current session has retrieved views of shared memory via \code{\link{retrieveViews}} internally the view is tracked until it is released via \code{\link{releaseViews}}.
  
  This function serves as a tool to check whether all views have been free'd after usage or to see what views are currently available to the session.
}
\usage{
  viewList()
}
\value{
  An 1:p list of characters of the the p retrieved views
}

\details{
The string of each element of the output list has the format <namespace name>.<variable name>. Default is lokal environment.
}
\author{ Julian Maerte }

\seealso{ \code{\link{retrieveViews}}, \code{\link{releaseViews}} }
\examples{
  viewList()
  # = list()

  #retrieveViews("namespace_viewL", c("my_shared_variable"))
  #viewList()
  # = list("my_shared_variable")

  #releaseViews("namespace", c("my_shared_variable"))
  #viewList()
  # = list()
}

\note{
For windows we prepend the namespace identifier by "\code{Local\\\\}" because otherwise the shared memory is shared system-wide (instead of user-wide) which needs admin privileges.

}
\concept{ shared memory }
\keyword{ multithreading }
