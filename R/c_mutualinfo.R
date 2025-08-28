c_mutualinfo <- function(joint, n) {
  .Call("C_mutualinfo", joint, n, PACKAGE = "memshare")
}
