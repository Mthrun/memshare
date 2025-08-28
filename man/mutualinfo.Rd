\name{mutualinfo}
\alias{mutualinfo}
\title{Mutual Information of continuous and discrete variables.}
\description{
   Return mutual information for a pair of joint variables.
   The variables can either be both numeric, both discrete or a mixture.
   The calculation is done via density estimate whenever necessary (i.e. for the continuous variables).
   The density is estimated via pareto density estimation with subsequent gaussian kernel smoothing.
}
\details{
  Mutual Information is >= 0 and symmetric (in x and y).
  You can think of mutual information as a measure of how much of x's information is contained in y's information
  or put more simply: How much does y predict x.
  Note that mutual information can be compared for pairs that share one variable e.g. (x,y) and (y,z), if MI(x,y) > MI(y,z) then
  x and y are more closely linked than y and z. However given pairs that do not share a variable, e.g. (x,y), (u,v) then MI(x,y) and MI(u,v)
  can not be reasonably compared. In particular: MI defines a partial ordering on the column pairs of a matrix instead of a total ordering (which correlation
  does for example). This is mainly due to MI not being upper-bound and thus is not reasonable put on a scale from 0 to 1.
}
\usage{
mutualinfo(x, y, isXDiscrete = FALSE, isYDiscrete = FALSE,

eps=.Machine$double.eps*1000, useMPMI=FALSE,na.rm=FALSE)
}
\arguments{
  \item{x}{[1:n] a numeric vector (not necessarily continuous)}
  \item{y}{[1:n] a numeric vector (not necessarily continuous)}
  \item{isXDiscrete}{Boolean defining whether or not the first numeric vector resembles a continuous or discrete measurement}
  \item{isYDiscrete}{Boolean defining whether or not the second numeric vector resembles a continuous or discrete measurement}
  \item{eps}{Scalar, The threshold for which the mutual info summand should be ignored (the limit of the summand for x -> 0 is 0 but the logarithm will be -inf...)}
  \item{useMPMI}{Boolean defining whether or not to use the package MPMI for the calculation (will be used as a baseline)}  
  \item{na.rm}{Boolean defining whether or not to use complete obeservations only}
}
\value{
  \item{mutualinfo}{The mutual information of the variables}
}
\author{ Julian Märte, Michael Thrun }
\examples{
x = c(rnorm(1000),rnorm(2000)+8,rnorm(1000)*2-8)
y = c(rep(1, 1000), rep(2, 2000), rep(3,1000))


if(requireNamespace("DataVisualizations", quietly = TRUE) &&
   requireNamespace("ScatterDensity", quietly = TRUE) &&
   packageVersion("ScatterDensity") >= "0.1.1" &&
    packageVersion("DataVisualizations") >= "1.1.5"){
      
  mutualinfo(x, y, isXDiscrete=FALSE, isYDiscrete=TRUE)
}
  
\donttest{
  if(requireNamespace("mpmi", quietly = TRUE)) {
      
  mutualinfo(x, y, isXDiscrete=FALSE, isYDiscrete=TRUE,useMPMI=TRUE)
  }
}
}
\note{This function requires that either \pkg{DataVisualizations} and \pkg{ScatterDensity} of equal or higher version than \code{0.1.1} is installed, or \pkg{mpmi} package}
\references{ Claude E. Shannon: A Mathematical Theory of Communication, 1948 }
\keyword{ mutualinfo }
\concept{ information theory }