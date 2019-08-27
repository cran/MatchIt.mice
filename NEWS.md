# MatchIt.mice <img src="man/figure/logo.png" align="right" width="120" />

<!-- badges: start -->
#### Matching Multiply Imputed Datasets
<!-- badges: end -->

[![](https://img.shields.io/badge/CRAN%20version-3.0.1-orange.svg?color=yellow&style=for-the-badge)](https://cran.r-project.org/package=MatchIt.mice)
[![](https://img.shields.io/badge/github%20version-3.0.1-orange.svg?color=yellow&style=for-the-badge)](https://github.com/FarhadPishgar/MatchIt.mice)

## What's New

![](https://img.shields.io/badge/version-3.0.1-orange.svg?color=yellow&style=for-the-badge)

The `MatchIt.mice` package gets a massive update! Now, you can match treatment and control observations within each imputed dataset, resulting in several estimates of treatment effect size (obtained from the complete case analysis of each imputed dataset), and then pool these several treatment effect size estimates. This approach is called `within` (or `match-then-pool`). Alternatively, you can average each observation's distance measures (propensity scores) from different imputed datasets, match treatment and control observations based on their averaged distance measures, and then average the treatment effect estimates obtained from the complete case analysis of each imputed dataset. This approach is called `across` (or `pool-then-match`). Minor bugs are fixed and the `README.md` and `NEWS.md` files are updated. 

![](https://img.shields.io/badge/version-2.1.5-orange.svg?color=inactive&style=for-the-badge)

The `mergeitmice()` function is now included in the package, which merges `mids`, `mimids`, and `wimids` objects with a dataframe. The `binditmice()` function is also updated to work with `mimids` and `wimids` objects. The `pool()` function is updated to produce more reliable results when numbers of observations in matched datasets are different. The `matchitmice()` function is also updated to sort results before returning the output (as thus, `matchitmice.data()` function is also updated). Minor bugs are fixed.

![](https://img.shields.io/badge/version-2.0.2-orange.svg?color=inactive&style=for-the-badge)

Minor bugs are fixed.

![](https://img.shields.io/badge/version-2.0.1-orange.svg?color=inactive&style=for-the-badge)

The `matchitmice()` and the `weightitmice()` functions are updated to match and weight imputed datasets, their outputs will be saved in the `mimids` and `wimids` class objects, and the `plot()`, `print()`, and `summary()` functions are updated to be able to provide detailed descriptions of these objects. The `matchitmice.data()` and `weightitmice.data()` functions are added for extracting matched and weighted imputed datasets from the `mimids` and `wimids` class objects and to replace the retired `matchmicedata()` function. The `with()` function now works with the `mimids` and `wimids` class objects and the `pool()` function can be used to pool the obtained results from analyses on each imputed dataset. Examples are simplified, the reference manual is updated, the `README.md` and `NEWS.md` files are revised, and plenty of minor bugs are fixed.

![](https://img.shields.io/badge/version-1.0.3-orange.svg?color=inactive&style=for-the-badge)

The `README.md` and `DESCRIPTION` files are updated. The package is released on the Comprehensive R Archive Network (CRAN) repository.

![](https://img.shields.io/badge/version-1.0.2-orange.svg?color=inactive&style=for-the-badge)

Not-so-tiny bugs are fixed and the performance is improved.

![](https://img.shields.io/badge/version-1.0.1-orange.svg?color=inactive&style=for-the-badge)

The `matchmicedata()` function is added to the package.

![](https://img.shields.io/badge/version-0.9.1-orange.svg?color=inactive&style=for-the-badge)

The mentioned examples are simplified and shortened.

![](https://img.shields.io/badge/version-0.9.0-orange.svg?color=inactive&style=for-the-badge)

The package is released on the GitHub.

## Author
Farhad Pishgar

[![](https://img.shields.io/twitter/follow/FarhadPishgar.svg?color=yellow&style=for-the-badge)](https://twitter.com/FarhadPishgar)
