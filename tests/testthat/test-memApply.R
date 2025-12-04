# tests/testthat/test-memApply.R

test_that("memApply matches base apply for correlation with a vector", {
  skip_if_not_installed("parallel")  # Should be there, but just in case
  
  set.seed(1)
  n <- 40
  p <- 10
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y <- rnorm(n)
  
  ns <- paste0("memApply_", Sys.getpid(), "_", sample.int(1e6, 1))
  
  on.exit({
    # memApply should clean up its own pages when X/VARS are passed as objects,
    # but we call releaseVariables just in case the implementation changes.
    try(memshare::releaseVariables(ns, "X"), silent = TRUE)
    try(memshare::releaseVariables(ns, "y"), silent = TRUE)
  }, add = TRUE)
  
  f <- function(v, y) stats::cor(v, y)
  
  res_mem <- memshare::memApply(
    X = X,
    MARGIN = 2,
    FUN = f,
    NAMESPACE = ns,
    VARS = list(y = y),
    MAX.CORES = 1
  )
  
  # memApply returns a list; base apply returns a numeric vector
  res_mem_vec  <- as.numeric(unlist(res_mem))
  res_base_vec <- as.numeric(apply(X, 2, f, y = y))
  
  expect_equal(res_mem_vec, res_base_vec, tolerance = 1e-12)
})

test_that("memApply works when X is already registered by name", {
  ns <- paste0("byname_", Sys.getpid(), "_", sample.int(1e6, 1))
  X <- matrix(runif(30), 6, 5)
  
  on.exit({
    try(memshare::releaseViews(ns, "X"), silent = TRUE)
    try(memshare::releaseVariables(ns, "X"), silent = TRUE)
  }, add = TRUE)
  
  memshare::registerVariables(ns, list(X = X))
  
  # Now call memApply with X = "X"
  out <- memshare::memApply(
    X = "X",
    MARGIN = 2,
    FUN = function(v) mean(v),
    NAMESPACE = ns,
    MAX.CORES = 1
  )
  
  out_vec <- as.numeric(unlist(out))
  expect_equal(out_vec, colMeans(X))
})

