---
output: 
  md_document:
    toc: false
    fig_width: 10.08
    fig_height: 6
tags: [r, prediction, effect]
vignette: >
  %\VignetteIndexEntry{README}
  \usepackage[utf8]{inputenc}
  %\VignetteEngine{knitr::rmarkdown}
editor_options: 
  chunk_output_type: console
---

```{=html}
<!-- badges: start -->
```
[![Travis build
status](https://travis-ci.com/CYGUBICKO/varpred.svg?branch=main)](https://travis-ci.com/CYGUBICKO/varpred)
[![CRAN
status](https://www.r-pkg.org/badges/version/varpred)](https://CRAN.R-project.org/package=varpred)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
`<!-- badges: end -->`{=html}

This package implements two approaches for constructing outcome plots
(prediction and effect plots). These include:

-   **mean-based** approach
-   **observed-value** approach

It can also be used to generate bias-corrected prediction and effect
estimates for generalized linear models involving non-linear link
functions, including models with random effects. This package
complements the existing ones by providing:

-   a straightforward way to generate effects plots
-   a robust way to correct for non-linear averaging bias in generalized
    (mixed) models

Installation
------------

You can install the development version of varpred from
[GitHub](https://github.com/cygubicko/varpred) with:

``` {.r}
# install.packages("remotes")
remotes::install_github("CYGUBICKO/varpred")
```

Example
-------

We use `mtcars` data to show outcome plots:

-   `isolate=TRUE` to generate effect plot
-   `isolate=FALSE` to generate prediction plot

``` {.r}
library(varpred)
library(ggplot2)

## Set theme for plots
varpredtheme()

## Fit the model
mod <- lm(mpg ~ wt + hp, mtcars)

## Effect
ef <- varpred(mod, "wt", isolate=TRUE, modelname="effect")
plot(ef)
```

`<img src="man/figures/simple_example-1.png" width="100%" />`{=html}

``` {.r}
## Prediction
pred <- varpred(mod, "wt", isolate=FALSE, modelname="prediction")
plot(pred)
```

`<img src="man/figures/simple_example-2.png" width="100%" />`{=html}

``` {.r}
## Compare effect and prediction
all_v <- combinevarpred(list(ef, pred))
p1 <- plot(all_v)

## Add observed data
print(p1
    + geom_point(data=mtcars, aes(x=wt, y=mpg), col="grey")
    + labs(colour="Method", linetype="Method")
)
```

`<img src="man/figures/simple_example-3.png" width="100%" />`{=html}
