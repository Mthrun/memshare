# tests/testthat/test-error-handling.R

test_that("releasing unknown variables produces an informative error", {
  ns <- paste0("release_error_", Sys.getpid(), "_", sample.int(1e6, 1))
  # No variables registered in this namespace
  
  expect_error(
    memshare::releaseVariables(ns, "does_not_exist"),
    regexp = "not previously allocated|not previously.*allocated",
    ignore.case = TRUE
  )
})

test_that("releasing unknown views produces an informative error", {
  ns <- paste0("release_view_error_", Sys.getpid(), "_", sample.int(1e6, 1))
  expect_error(
    memshare::releaseViews(ns, "does_not_exist"),
    regexp = "not previously allocated|not previously.*allocated",
    ignore.case = TRUE
  )
})

test_that("memApply gives warning for non-double matrices and non-matrix inputs", {
  ns <- paste0("type_", Sys.getpid(), "_", sample.int(1e4, 1))
  
  X_int <- matrix(1L:9L, 3, 3)
  
  
  expect_warning(
    memshare::memApply(
      X = X_int,
      MARGIN = 2,
      FUN = function(v) mean(v),
      NAMESPACE = ns,
      MAX.CORES = 1
    ),
    regexp = "not double|non-double",
    ignore.case = TRUE
  )
  mode(X_int)="numeric"
  X_df  <- data.frame(X_int)
  expect_warning(
    memshare::memApply(
      X = X_df,
      MARGIN = 2,
      FUN = function(v) mean(v),
      NAMESPACE = ns,
      MAX.CORES = 1
    ),
    regexp = "matrix",
    ignore.case = TRUE
  )
})

test_that("memLapply detects malformed VARS argument", {
  ns <- paste0("test_ns_memLapply_vars_", Sys.getpid(), "_", sample.int(1e6, 1))
  
  l <- list(1:3, 4:6)
  
  # VARS must be either a named list or character vector; here: unnamed list
  bad_vars <- list(1:3, 4:6)
  
  expect_error(
    memshare::memLapply(
      X = l,
      FUN = function(el, z) el + z,
      NAMESPACE = ns,
      VARS = bad_vars,
      MAX.CORES = 1
    ),
    regexp = "element type",
    fixed  = TRUE
  )
})

test_that("registerVariables errors on duplicate registration", {
  
  ns <- paste0("dup_test_", as.integer(Sys.time()))
  x  <- matrix(as.double(1:4), 2, 2)
  
  vars <- list(x = x)
  
  memshare::registerVariables(ns, vars)
  
  expect_error(
    memshare::registerVariables(ns, vars),
    "Variable was already registered!",
    fixed = TRUE
  )
  
  memshare::releaseVariables(ns, "x")
})

test_that("releaseVariables errors on unknown variable", {
  
  ns <- paste0("unknown_release_", as.integer(Sys.time()))
  
  expect_error(
    memshare::releaseVariables(ns, "does_not_exist"),
    "not previously allocated",
    ignore.case = TRUE
  )
})

test_that("releaseViews errors on unknown variable", {
  skip_on_cran()
  
  ns <- paste0("unknown_view_", as.integer(Sys.time()))
  
  expect_error(
    memshare::releaseViews(ns, "does_not_exist"),
    "not previously allocated",
    ignore.case = TRUE
  )
})

test_that("registerVariables errors on unsupported object type", {

  ns <- paste0("unsupported_type_", as.integer(Sys.time()))
  bad <- data.frame(a = 1:3)  # not double matrix/vector/list of doubles
  
  expect_error(
    memshare::registerVariables(ns, list(bad = bad)),
    "Unknown element type",
    ignore.case = TRUE
  )
})