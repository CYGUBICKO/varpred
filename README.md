
# varpred

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/CYGUBICKO/varpred.svg?branch=main)](https://travis-ci.com/CYGUBICKO/varpred)
[![CRAN status](https://www.r-pkg.org/badges/version/varpred)](https://CRAN.R-project.org/package=varpred)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This package implements two approaches for constructing outcome plots (prediction and effect plots). These include:

- **mean-based** approach
- **observed-value** approach

It can also be used to generate bias-corrected prediction and effect estimates for generalized linear models involving non-linear link functions, including models with random effects. This package complements the existing ones by providing: 

- a straightforward way to generate effects plots
- a robust way to correct for non-linear averaging bias in generalized (mixed) models


## Installation

You can install the development version of varpred from [GitHub](https://github.com/cygubicko/varpred) with:

``` r
# install.packages("remotes")
remotes::install_github("CYGUBICKO/varpred")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(varpred)
## basic example code
2 + 2
```

