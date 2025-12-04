# tests/testthat/test-lifecycle.R

test_that("register -> retrieve -> release lifecycle works for matrix and vector", {
  ns <- paste0("lifecycle_", Sys.getpid(), "_", sample.int(1e6, 1))
  
  mat <- matrix(as.double(1:6), nrow = 2, ncol = 3)
  vec <- as.double(1:3)
  
  # Ensure clean state
  on.exit({
    # Try to release; ignore errors in case they were already released
    try(memshare::releaseViews(ns, c("mat", "vec")), silent = TRUE)
    try(memshare::releaseVariables(ns, c("mat", "vec")), silent = TRUE)
  }, add = TRUE)
  
  memshare::registerVariables(ns, list(mat = mat, vec = vec))
  
  # pageList() should show both names
  pages <- memshare::pageList()
  expect_true(any(grepl(paste0(ns, ".mat"), pages, fixed = TRUE)))
  expect_true(any(grepl(paste0(ns, ".vec"), pages, fixed = TRUE)))
  
  # Retrieve views and check contents
  views <- memshare::retrieveViews(ns, c("mat", "vec"))
  expect_true(is.matrix(views$mat))
  expect_true(is.numeric(views$vec))
  expect_equal(views$mat, mat)
  expect_equal(as.numeric(views$vec), vec)
  
  # viewList() must reflect that we hold mat and vec
  vlist <- memshare::viewList()
  expect_true(any(grepl(paste0(ns, ".mat"), vlist, fixed = TRUE)))
  expect_true(any(grepl(paste0(ns, ".vec"), vlist, fixed = TRUE)))
  
  # Release views
  memshare::releaseViews(ns, c("mat", "vec"))
  vlist_after <- memshare::viewList()
  expect_false(any(grepl(paste0(ns, ".mat"), vlist_after, fixed = TRUE)))
  expect_false(any(grepl(paste0(ns, ".vec"), vlist_after, fixed = TRUE)))
  
  # Release variables
  memshare::releaseVariables(ns, c("mat", "vec"))
  pages_after <- memshare::pageList()
  expect_false(any(grepl(paste0(ns, ".mat"), pages_after, fixed = TRUE)))
  expect_false(any(grepl(paste0(ns, ".vec"), pages_after, fixed = TRUE)))
})

test_that("retrieveMetadata reports correct dimensions for matrices", {
  ns <- paste0("metadata_", Sys.getpid(), "_", sample.int(1e6, 1))
  mat <- matrix(runif(12), 3, 4)
  
  on.exit({
    try(memshare::releaseViews(ns, "mat"), silent = TRUE)
    try(memshare::releaseVariables(ns, "mat"), silent = TRUE)
  }, add = TRUE)
  
  memshare::registerVariables(ns, list(mat = mat))
  
  meta <- memshare::retrieveMetadata(ns, "mat")
  # NOTE: retrieveMetadata may implicitly create a view (as per your docs),
  # so we must release that.
  expect_equal(meta$type, "matrix")
  expect_equal(meta$nrow, nrow(mat))
  expect_equal(meta$ncol, ncol(mat))
  
  memshare::releaseViews(ns, "mat")
  memshare::releaseVariables(ns, "mat")
})

test_that("register/retrieve/release lifecycle is consistent", {
  ns <- paste0("test_ns", Sys.getpid(), "_", sample.int(1e6, 1))
  mat <- matrix(as.double(1:6), 2, 3)
  
  registerVariables(ns, list(MATRIX = mat))
  expect_true(length(pageList()) >= 1L)
  
  views <- retrieveViews(ns, "MATRIX")
  expect_true("MATRIX" %in% names(views))
  expect_equal(views$MATRIX, mat)
  
  releaseViews(ns, "MATRIX")
  releaseVariables(ns, "MATRIX")
  
  expect_false(any(grepl("MATRIX", unlist(pageList()), fixed = TRUE)))
})
