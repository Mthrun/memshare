# tests/testthat/test-memLapply.R

test_that("memLapply matches base lapply for list of matrices", {
  skip_if_not_installed("parallel")
  
  set.seed(123)
  list_length <- 20
  m <- 5
  
  l <- lapply(seq_len(list_length), function(i) {
    matrix(rnorm(m * m), nrow = m, ncol = m)
  })
  y <- rnorm(m)
  
  ns <- paste0("memLapply_", Sys.getpid(), "_", sample.int(1e6, 1))
  
  on.exit({
    try(memshare::releaseViews(ns, c("l", "y")), silent = TRUE)
    try(memshare::releaseVariables(ns, c("l", "y")), silent = TRUE)
  }, add = TRUE)
  
  # memLapply with shared y
  res_mem <- memshare::memLapply(
    X = l,
    FUN = function(el, y) el %*% y,
    NAMESPACE = ns,
    VARS = list(y = y),
    MAX.CORES = 1
  )
  
  # baseline lapply
  res_base <- lapply(l, function(el) el %*% y)
  
  expect_equal(res_mem, res_base)
})

test_that("memLapply works with X passed by shared name", {
  ns <- paste0("byname2_", Sys.getpid(), "_", sample.int(1e6, 1))
  l=list(
    matrix(as.double(1:4), 2, 2),
    matrix(as.double(5:8), 2, 2)
  )
  ListList=list()
  ListList[["L"]] = l
  memshare::registerVariables(ns, ListList)
  
  on.exit({
    try(memshare::releaseViews(ns, "L"), silent = TRUE)
    try(memshare::releaseVariables(ns, "L"), silent = TRUE)
  }, add = TRUE)
  
  res <- memshare::memLapply(
    X = "L",
    FUN = function(el) sum(el),
    NAMESPACE = ns,
    MAX.CORES = 1
  )
  
  expect_equal(unlist(res), c(sum(l[[1]]), sum(l[[2]])))
})
