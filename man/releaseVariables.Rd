\name{releaseVariables}
\alias{releaseVariables}
\title{Release variables from a shared memory namespace}
\description{
Delete variables from the shared memory space. Actual release occurs only when no active views remain.
}
\usage{
releaseVariables(namespace, variableNames)
}
\arguments{
  \item{namespace}{Character(1) used at registration time.}
  \item{variableNames}{Character vector of names to free.}
}
\value{
Invisibly, \code{TRUE} on success.
}
\details{
Registered buffers remain allocated until \emph{all} views are released.
If any worker still holds a view, \code{\link{releaseVariables}} cannot reclaim memory.

\strong{Thread safety}

Registered buffers may be read concurrently by many processes.
Concurrent writes must be synchronized externally (e.g., interprocess mutex). Do not call
the R API from secondary threads.

Wrappers such as \code{\link{memApply}} / \code{\link{memLapply}} call this function on.exit.
}

\note{
This call succeeds in removing ownership, but underlying memory is only unmapped
when every process has called \code{\link{releaseViews}} for those variables.
Use \code{\link{viewList}} and \code{\link{pageList}} for diagnostics.
}
\seealso{\code{\link{registerVariables}}, \code{\link{releaseViews}}}
\examples{
ns <- "example"
X <- matrix(rnorm(100), 10, 10)
registerVariables(ns, list(X = X))
# later ...
releaseVariables(ns, "X")
}
