
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

The `{ao}` package offers useRs easy access to alternating optimization.

## Why?

Alternating optimization is an iterative procedure that optimizes a
function by alternately performing restricted optimization over
individual parameter subsets. Instead of tackling joint optimization
directly, it breaks the problem down into simpler sub-problems. This
approach can make optimization feasible when joint optimization is too
difficult. For more details, please refer to the [package
vignette](https://loelschlaeger.de/ao/articles/ao.html).

## How?

You can install the released package version from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ao")
```

Then load the package via `library("ao")` and you should be ready to go.
Here is a simple example of alternating minimization of the [Rosenbrock
function](https://en.wikipedia.org/wiki/Rosenbrock_function):

``` r
rosenbrock <- function(x) (1 - x[1])^2 + (x[2] - x[1]^2)^2
ao(f = rosenbrock, initial = c(2, 2))
#> $estimate
#> [1] 1.000895 1.001791
#> 
#> $value
#> [1] 8.016137e-07
#> 
#> $details
#>    iteration        value       p1       p2 b1 b2     seconds update_code
#> 1          0 5.000000e+00 2.000000 2.000000  0  0 0.000000000           0
#> 2          1 1.519238e-01 1.366025 2.000000  1  0 0.027111053           0
#> 3          1 1.339744e-01 1.366025 1.866024  0  1 0.001492023           0
#> 4          2 1.176778e-01 1.320824 1.866024  1  0 0.002427101           0
#> 5          2 1.029278e-01 1.320824 1.744575  0  1 0.001779079           0
#> 6          3 8.966402e-02 1.278883 1.744575  1  0 0.002011061           0
#> 7          3 7.777546e-02 1.278883 1.635540  0  1 0.001534939           0
#> 8          4 6.719114e-02 1.240415 1.635540  1  0 0.002342939           0
#> 9          4 5.779955e-02 1.240415 1.538630  0  1 0.001434088           0
#> 10         5 4.952339e-02 1.205560 1.538630  1  0 0.002373934           0
#> 11         5 4.225482e-02 1.205560 1.453374  0  1 0.001579046           0
#> 12         6 3.591491e-02 1.174366 1.453374  1  0 0.002105951           0
#> 13         6 3.040344e-02 1.174366 1.379135  0  1 0.001474142           0
#> 14         7 2.564430e-02 1.146792 1.379135  1  0 0.002417803           0
#> 15         7 2.154801e-02 1.146792 1.315133  0  1 0.001422167           0
#> 16         8 1.804492e-02 1.122712 1.315133  1  0 0.002574921           0
#> 17         8 1.505832e-02 1.122712 1.260483  0  1 0.001725197           0
#> 18         9 1.252724e-02 1.101923 1.260483  1  0 0.002628088           0
#> 19         9 1.038836e-02 1.101923 1.214235  0  1 0.001496077           0
#> 20        10 8.590837e-03 1.084167 1.214235  1  0 0.008152008           0
#> 21        10 7.084101e-03 1.084167 1.175418  0  1 0.001859903           0
#> 22        11 5.827377e-03 1.069149 1.175418  1  0 0.006841898           0
#> 23        11 4.781578e-03 1.069149 1.143079  0  1 0.001681805           0
#> 24        12 3.915156e-03 1.056558 1.143079  1  0 0.007035971           0
#> 25        12 3.198754e-03 1.056558 1.116314  0  1 0.001721859           0
#> 26        13 2.608707e-03 1.046082 1.116314  1  0 0.017139196           0
#> 27        13 2.123531e-03 1.046082 1.094287  0  1 0.001847029           0
#> 28        14 1.725945e-03 1.037424 1.094287  1  0 0.002295017           0
#> 29        14 1.400576e-03 1.037424 1.076249  0  1 0.001765013           0
#> 30        15 1.135093e-03 1.030310 1.076249  1  0 0.002101183           0
#> 31        15 9.187038e-04 1.030310 1.061539  0  1 0.001725912           0
#> 32        16 7.427825e-04 1.024492 1.061539  1  0 0.002171040           0
#> 33        16 5.998755e-04 1.024492 1.049585  0  1 0.002166033           0
#> 34        17 4.840462e-04 1.019754 1.049585  1  0 0.002307892           0
#> 35        17 3.902161e-04 1.019754 1.039898  0  1 0.001731157           0
#> 36        18 3.143566e-04 1.015907 1.039898  1  0 0.002439022           0
#> 37        18 2.530454e-04 1.015907 1.032068  0  1 0.001885176           0
#> 38        19 2.035803e-04 1.012794 1.032068  1  0 0.002171993           0
#> 39        19 1.636760e-04 1.012794 1.025751  0  1 0.001772881           0
#> 40        20 1.315375e-04 1.010279 1.025751  1  0 0.002203941           0
#> 41        20 1.056496e-04 1.010279 1.020663  0  1 0.002334833           0
#> 42        21 8.482978e-05 1.008251 1.020663  1  0 0.002116919           0
#> 43        21 6.807922e-05 1.008251 1.016570  0  1 0.001725912           0
#> 44        22 5.462405e-05 1.006619 1.016570  1  0 0.002103090           0
#> 45        22 4.380882e-05 1.006619 1.013281  0  1 0.001744032           0
#> 46        23 3.513011e-05 1.005307 1.013281  1  0 0.002115011           0
#> 47        23 2.815916e-05 1.005307 1.010641  0  1 0.001759052           0
#> 48        24 2.257018e-05 1.004252 1.010641  1  0 0.002601147           0
#> 49        24 1.808332e-05 1.004252 1.008523  0  1 0.001874924           0
#> 50        25 1.448872e-05 1.003406 1.008523  1  0 0.002107143           0
#> 51        25 1.160399e-05 1.003406 1.006825  0  1 0.002099991           0
#> 52        26 9.294548e-06 1.002728 1.006825  1  0 0.002178907           0
#> 53        26 7.441548e-06 1.002728 1.005463  0  1 0.001872063           0
#> 54        27 5.959072e-06 1.002184 1.005463  1  0 0.002409935           0
#> 55        27 4.769667e-06 1.002184 1.004373  0  1 0.001423120           0
#> 56        28 3.818729e-06 1.001748 1.004373  1  0 0.002345085           0
#> 57        28 3.055717e-06 1.001748 1.003499  0  1 0.001754045           0
#> 58        29 2.446111e-06 1.001399 1.003499  1  0 0.003511906           0
#> 59        29 1.956863e-06 1.001399 1.002800  0  1 0.002917051           0
#> 60        30 1.566279e-06 1.001119 1.002800  1  0 0.002190113           0
#> 61        30 1.252688e-06 1.001119 1.002240  0  1 0.001568794           0
#> 62        31 1.002554e-06 1.000895 1.002240  1  0 0.001603127           0
#> 63        31 8.016137e-07 1.000895 1.001791  0  1 0.001019001           0
#> 
#> $seconds
#> [1] 0.1803207
#> 
#> $stopping_reason
#> [1] "change in function value is < 1e-06"
```

![](vignettes/README-rosenbrock_ao_path-1.png)<!-- -->

## Contact?

If you have a question, found a bug, want to request a feature, give
feedback, or contribute, we would love to hear from you. [Please file an
issue on GitHub](https://github.com/loelschlaeger/ao/issues/new/choose).
😊
