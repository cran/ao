
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Alternating optimization <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ao)](https://CRAN.R-project.org/package=ao)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/last-month/ao)](https://CRAN.R-project.org/package=ao)
[![R-CMD-check](https://github.com/loelschlaeger/ao/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/ao/actions)
[![Codecov test
coverage](https://codecov.io/gh/loelschlaeger/ao/branch/main/graph/badge.svg)](https://app.codecov.io/gh/loelschlaeger/ao?branch=main)
<!-- badges: end -->

The `{ao}` package implements a numerical optimization algorithm called
alternating optimization in R.

Alternating optimization is an iterative procedure which optimizes a
function jointly over all parameters by alternately performing
restricted optimization over individual parameter subsets.

For additional details on the method, please refer to the [package
vignette](https://loelschlaeger.de/ao/articles/ao.html).

## Installation

You can install the released version of `{ao}` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ao")
```

## Example

The following is a simple example to perform alternating optimization of
the [Himmelblau’s
function](https://en.wikipedia.org/wiki/Himmelblau%27s_function),
separately for $x_1$ and $x_2$, with the parameter restrictions
$-5 \leq x_1, x_2 \leq 5$.

### Step 1: Load the package

``` r
library("ao")
#> Loading required package: optimizeR
#> Thanks for using {ao} version 0.3.3, happy alternating optimization!
#> Documentation: https://loelschlaeger.de/ao
```

### Step 2: Define the function to be optimized

``` r
himmelblau <- function(x, a, b) (x[1]^2 + x[2] + a)^2 + (x[1] + x[2]^2 + b)^2
```

The function is optimized over its first argument (`x`), which needs to
be a `numeric` `vector`. Other function arguments (`a` and `b` in this
case) remain fixed during the optimization. The function should return a
single `numeric` value.

### Step 3: Define a base optimizer

Alternating optimization requires a base optimizer that numerically
solves the optimization problems in the partitions of the parameter
vector. Such an optimizer must be defined through the framework provided
by the `{optimizeR}` package, please see [its
documentation](https://loelschlaeger.de/optimizeR/) for details.

``` r
base_optimizer <- optimizeR::Optimizer$new(which = "stats::optim", lower = -5, upper = 5, method = "L-BFGS-B")
```

### Step 4: Call the `ao()` function

Despite `f` and `base_optimizer`, which have been defined above, the
`ao()` function requires the following arguments:

- `p` defines the starting parameter values,

- `a` and `b` are fixed function arguments,

- `partition` defines the parameter subsets (here, the first entry of
  `x` and the second are optimized separately).

``` r
ao(f = himmelblau, p = c(0, 0), a = -11, b = -7, partition = list(1, 2), base_optimizer = base_optimizer)
#> $value
#> [1] 1.940035e-12
#> 
#> $estimate
#> [1]  3.584428 -1.848126
#> 
#> $sequence
#>    iteration partition        value      seconds       p1        p2
#> 1          0        NA 1.700000e+02 0.0000000000 0.000000  0.000000
#> 2          1         1 1.327270e+01 0.0090060234 3.395691  0.000000
#> 3          1         2 1.743666e+00 0.0009770393 3.395691 -1.803183
#> 4          2         1 2.847292e-02 0.0007698536 3.581412 -1.803183
#> 5          2         2 4.687472e-04 0.0006618500 3.581412 -1.847412
#> 6          3         1 7.368063e-06 0.0011827946 3.584381 -1.847412
#> 7          3         2 1.157612e-07 0.0004611015 3.584381 -1.848115
#> 8          4         1 1.900153e-09 0.0004670620 3.584427 -1.848115
#> 9          4         2 4.221429e-11 0.0003750324 3.584427 -1.848126
#> 10         5         1 3.598278e-12 0.0004618168 3.584428 -1.848126
#> 11         5         2 1.940035e-12 0.0003538132 3.584428 -1.848126
#> 
#> $seconds
#> [1] 0.01471639
```

The output contains:

- the function `value` at convergence,

- the parameter value `estimate` at convergence,

- `sequence` provides information about the updates in the single
  iterations and partitions,

- and the optimization time in `seconds`.

## Contact

Have a question, found a bug, request a feature, want to contribute?
[Please file an
issue](https://github.com/loelschlaeger/ao/issues/new/choose).
