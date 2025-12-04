# tests/testthat/test-os-specific.R

test_that("re-registering the same variable after release works (no stale shm)", {
  ns <- paste0("reregister_", Sys.getpid(), "_", sample.int(1e5, 1))
  mat <- matrix(runif(6), 2, 3)
  
  # First register / release
  memshare::registerVariables(ns, list(mat = mat))
  memshare::releaseVariables(ns, "mat")
  
  # Re-register should succeed without error
  expect_silent(memshare::registerVariables(ns, list(mat = mat)))
  memshare::releaseVariables(ns, "mat")
})

test_that("macOS namespace length limitations are enforced", {
  # Only relevant on macOS; skip elsewhere
  skip_on_os(c("windows", "linux"))
  
  ns_long <- paste(rep("very_long_namespace", 10), collapse = "_")
  mat <- matrix(as.double(1:4), 2, 2)
  
  # The C++ code checks name.size() > 32 INCLUDING the leading slash used by shm.
  # Because we add ".mat", we only need a very long ns to trigger this.
  expect_error(
    memshare::registerVariables(ns_long, list(mat = mat)),
    regexp = "length < 32|exceeds",
    ignore.case = TRUE
  )
})

test_that("Windows: namespace is prefixed with Local\\\\", {
  skip_on_os(c("linux", "mac"))  # only relevant for Windows
  
  ns <- paste0("testns_", as.integer(Sys.time()))
  x  <- matrix(as.double(1:4), 2, 2)
  
  # clean slate
  memshare::registerVariables(ns, list(x = x))
  pages <- memshare::pageList()
  
  expect_true(
    any(grepl("^Local\\\\", unlist(pages), useBytes = TRUE)),
    info = "On Windows, shared page names should begin with Local\\"
  )
  
  memshare::releaseVariables(ns, "x")
})

test_that("Unix: namespace is not prefixed with Local\\\\", {
  skip_on_os("windows")
  
  ns <- paste0("testns_", as.integer(Sys.time()))
  x  <- matrix(as.double(1:4), 2, 2)
  
  memshare::registerVariables(ns, list(x = x))
  pages <- memshare::pageList()
  names_chr <- unlist(pages)
  
  expect_true(
    any(grepl(paste0("^", ns, "\\.x$"), names_chr)),
    info = "On Unix, shared page names should start with the raw namespace"
  )
  expect_false(
    any(grepl("^Local\\\\", names_chr, useBytes = TRUE)),
    info = "On Unix, no Local\\ prefix should be present"
  )
  
  memshare::releaseVariables(ns, "x")
})