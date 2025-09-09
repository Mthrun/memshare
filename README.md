[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/memshare)](https://cran.r-project.org/package=memshare)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/memshare?color=blue)](https://r-pkg.org/pkg/memshare)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/last-month/memshare?color=green)](https://r-pkg.org/pkg/memshare)

# memshare: Shared memory multithreading in R via C++17, with zero-copy access through ALTREP data structures.


---

# Overview

`memshare` enables multicore computation in R without redundant memory copies. Large vectors, matrices, or lists are stored once in shared memory and exposed to R processes as **ALTREP views**. This allows workers in a PSOCK cluster to operate on the same physical data while avoiding serialization overhead.

Key features:

- Shared memory allocation via C++17 back-end (`shm_open` on Unix, `MapViewOfFile` on Windows).
- ALTREP wrappers so R sees shared objects as native vectors or matrices.
- High-level parallels to `parallel::parApply` and `parallel::parLapply`:
  - `memApply()` — apply a function row/column-wise over a matrix in shared memory.
  - `memLapply()` — apply a function element-wise over a list in shared memory.
- Automatic cleanup of shared objects when all views are released or when the package is unloaded.
- Tested on Linux, macOS, and Windows.

---

# Installation

From CRAN:

```r
install.packages("memshare")
```

From GitHub (development version):

```r
remotes::install_github("mthrun/memshare")
```

System requirements: R ≥ 4.0 and a C++17 compiler.

# Quick start

## Example: Parallel correlations with a matrix

```r
library(memshare)
library(parallel)

set.seed(1)
n <- 10000
p <- 2000

X <- matrix(rnorm(n * p), n, p)
y <- rnorm(n)

res <- memApply(
  X = X, MARGIN = 2,
  FUN = function(v, y) cor(v, y),
  VARS = list(y = y)
)
str(res)
```

Here res is a list of correlations, one per column, computed in parallel without copying X to each worker.

## Concepts

Pages: memory regions owned by the current R session that loaded the package.

Views: ALTREP wrappers exposing shared memory variables (read/write capable).

Namespaces: string identifiers defining a shared memory context across sessions.

When the package is detached, all handles and associated shared memory pages are released, unless another R process still holds references.

### Manual

The full manual for users or developers is available here:
[Package documentation](https://CRAN.R-project.org/package=memshare/memshare.pdf)

## References <a name="references"/>

[Thrun and Märte, 2025] Thrun, M.C., Märte, J.: Memshare: Memory Sharing for Multicore Computation in R with an Application to Feature Selection by Mutual Information using PDE, 2025.

[Thrun et al., 2020] Thrun, M.C., Gehlert, T., & Ultsch, A.: Analyzing the Fine Structure of Distributions, PLOS ONE, 15(10), e0238835, 2020.

[Ultsch, 2005] Ultsch, A.: Pareto Density Estimation: A Density Estimation for Knowledge Discovery, Proceedings of the 28th Annual Conference of the German Classification Society, Springer, 2005

