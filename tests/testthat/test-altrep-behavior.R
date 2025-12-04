# tests/testthat/test-altrep-behavior.R

test_that("ALTREP views behave like ordinary matrices/vectors for basic ops", {
  ns <- paste0("altrep_", Sys.getpid(), "_", sample.int(1e6, 1))
  
  mat <- matrix(rnorm(20), 5, 4)
  vec <- rnorm(5)
  
  on.exit({
    try(memshare::releaseVariables(ns, c("mat", "vec")), silent = TRUE)
  }, add = TRUE)
  
  memshare::registerVariables(ns, list(mat = mat, vec = vec))
  views <- memshare::retrieveViews(ns, c("mat", "vec"))
  
  # Basic properties
  expect_true(is.matrix(views$mat))
  expect_true(is.numeric(views$vec))
  expect_equal(dim(views$mat), dim(mat))
  expect_equal(length(views$vec), length(vec))
  
  # Basic operations
  expect_equal(colSums(views$mat), colSums(mat))
  expect_equal(rowMeans(views$mat), rowMeans(mat))
  expect_equal(sum(views$vec), sum(vec))
  
  memshare::releaseViews(ns, c("mat", "vec"))
})
