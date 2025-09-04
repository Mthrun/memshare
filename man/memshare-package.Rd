\name{memshare-package}
\alias{memshare-package}
\alias{memshare}
\docType{package}
\title{
\packageTitle{memshare}
}
\description{
\packageDescription{memshare}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{memshare}
\packageIndices{memsharey}

If the user detaches the package, all handels are destroyed, meaning that all vairables of all namespaces are cleared as long as there is no other r thread still using the variables.

The two basic definitions are:

1. “\code{Pages}” are variables owned by the current compilation unit of the code (e.g., 'R' session or terminal that loaded the DLL). The pages are coded in Windows via 'MapViewOfFile' and on Unix via 'shm'+'mmap'.
		
2. “\code{Views}” are references to variables owned by another (or their own) compilation unit. The views are always 'ALTREP' wrappers for the pointers to the shared memory chunk.  
		
3. "\code{namespace}" are character of length 1 called here strings, that define the identifier of the shared memory context allowing the initialize shared variables.
}
\author{
\packageAuthor{memshare}

Maintainer: \packageMaintainer{memshare}
}

\keyword{ package }

\examples{
	x=rnorm(100)
	y=runif(100)
	Mat=cbind(x,x,x)
	res = memApply(X = Mat, MARGIN = 2,
	FUN = function(x,y) {
    cc=memshare::mutualinfo(x,y,isYDiscrete = TRUE,
                  na.rm = TRUE,useMPMI = FALSE)
    return(cc)
	},VARS = list(y=y),MAX.CORES=1, #for testing purposes only single thread
	NAMESPACE="namespaceID")
	unlist(res)
	\dontrun{
	#usually MAX.CORES>1 for application
	}
}
