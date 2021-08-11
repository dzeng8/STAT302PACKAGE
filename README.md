
# STAT302PACKAGE

<!-- badges: start -->
[![R-CMD-check](https://github.com/dzeng8/STAT302PACKAGE/workflows/R-CMD-check/badge.svg)](https://github.com/dzeng8/STAT302PACKAGE/actions)
[![codecov](https://codecov.io/gh/dzeng8/STAT302PACKAGE/branch/master/graph/badge.svg?token=SD9JJGQSQY)](https://codecov.io/gh/dzeng8/STAT302PACKAGE)
<!-- badges: end -->

The goal of STAT302PACKAGE is to demonstrate how to build a package

## Installation

You can install the released version of STAT302PACKAGE using:


``` r
devtools::install_github("dzeng8/STAT302PACKAGE")
```

To view vignettes, run the following code:

``` r
devtools::install_github("dzeng8/STAT302PACKAGE", build_vignette = TRUE, build_opts = c())
library(STAT302PACKAGE)
# Use this to view the vignette in the STAT302PACKAGE HTML help
help(package = "STAT302PACKAGE", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "STAT302PACKAGE")
```
