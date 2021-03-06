# MatchIt.mice <img src="man/figure/logo.png" align="right" width="120" />

<!-- badges: start -->
#### Matching Multiply Imputed Datasets
<!-- badges: end -->

[![](https://img.shields.io/badge/CRAN%20version-3.0.1-orange.svg?color=yellow&style=for-the-badge)](https://cran.r-project.org/package=MatchIt.mice)
[![](https://img.shields.io/badge/github%20version-3.0.1-orange.svg?color=yellow&style=for-the-badge)](https://github.com/FarhadPishgar/MatchIt.mice)

## Introduction

One of the major issues in the matching procedures is the presence of missing data since matching relies on the predictions from a logistic regression model and with lacking information for the variables within the model, the predictions cannot be made for that observation. There are a couple of solutions to address this problem and despite the standard approach of the complete case analysis (excluding observations with missing data points from the final analysis), adopting algorithms to multiply impute the missing data is growing as a popular alternative.

Matching of treatment and control observations in multiply imputed datasets can be achieved through different approaches:

1. **The within (match-then-pool) approach**: In this approach, matching is done on each imputed dataset, the complete data analysis is performed on them, and the treatment effects obtained from these analyses are pooled together (please see the article by [Leyrat et al.](https://www.ncbi.nlm.nih.gov/pubmed/28573919) for more details).
2. **The across (pool-then-match) approach**: In this approach, the calculated distances (propensity scores) for each observation across the imputed datasets are pooled and using this pooled measure, matching is done on the imputed datasets. Complete data analysis is performed on the matched datasets, and the treatment effects obtained from these analyses are pooled together (although a bit different, please see the article by [Mitra et al.](https://www.ncbi.nlm.nih.gov/pubmed/22687877) for more details).

The [`mice`](https://cran.r-project.org/package=mice) package is a widely accepted statistical tool for imputing the ignorable missing data in the R platform. The [`MatchIt.mice`](https://cran.r-project.org/package=MatchIt.mice) package simplifies the process of matching the imputed datasets of the [`mice`](https://cran.r-project.org/package=mice) package and enables credible adoption of the two matching approaches and several matching methods in practice. This package enables parametric models for causal inference to work better through selecting matched observations from the control and treatment groups, performing complete data analysis, and pooling the obtained results on imputed datasets using Rubin’s rules.

## Installation

The [`MatchIt.mice`](https://cran.r-project.org/package=MatchIt.mice) package can be installed from the Comprehensive R Archive Network (CRAN) repository as follows:

``` r
install.packages("MatchIt.mice")
```

The latest (though unstable) version of the package can be installed from GitHub as follows:

``` r
devtools::install_github(repo = "FarhadPishgar/MatchIt.mice")
```

## Suggested Workflow

### Overview

Adopting algorithms to multiply impute the missing data, before the matching procedure, and the matching procedure itself may seem to be complicated tasks. This suggested workflow tries to simplify this process into a few steps:

1. **Imputing the Missing Data in the Dataset**: The `mice()` function from the [`mice`](https://cran.r-project.org/package=mice) package can be used to multiply impute the missing data in the dataset.
2. **Matching the Imputed Datasets**: The `matchitmice()` function from the [`MatchIt.mice`](https://cran.r-project.org/package=MatchIt.mice) package should be used to select matched observations from treatment and control groups of each imputed dataset.
3. **Complete Data Analysis**: The `with()` function from the [`MatchIt.mice`](https://cran.r-project.org/package=MatchIt.mice) package should be used to estimate treatment effect size from complete data analysis in each (matched) imputed dataset.
4. **Pooling the Treatment Effect Size Estimates**: The `pool()` function from the [`MatchIt.mice`](https://cran.r-project.org/package=MatchIt.mice) package should be used to pool the obtained treatment effect estimates from the previous step using Rubin’s rules.

### Imputing the Missing Data in the Dataset

The [`mice`](https://cran.r-project.org/package=mice) package and its main function, `mice()`, provide the necessary tools for multiply imputing ignorable missing data in a dataset (several points should be considered before choosing the appropriate method for the imputation procedure, please see the [`mice`](https://cran.r-project.org/package=mice) package reference manual for details). The output of the `mice()` function will be saved in an object of the `mids` class. The `print()` and `summary()` functions can be used to review detailed descriptions of these objects.

### Matching the Imputed Datasets

The [`MatchIt.mice`](https://cran.r-project.org/package=MatchIt.mice) package and its main function, `matchitmice()`, provides the essential tools for selecting matched observations from treatment and control groups of imputed datasets. Currently, two matching approaches (match-then-pool and pool-then-match matching approaches) and two matching methods (nearest neighbor and exact matching methods) are available (within each of these approaches and methods, the [`MatchIt.mice`](https://cran.r-project.org/package=MatchIt.mice) package offers a variety of options).

The `matchitmice()` function requires the imputed datasets to include a column of unique identifier number for each observation (the `binditmice()` and `mergeitmice()` functions can be used to add this column to each imputed dataset) and not to have any missing data (even in variables not included in the matching model).

The output of the `matchitmice()` function will be saved in an object of the `mimids` class. The `plot()`, `print()`, and `summary()` functions can be used to review detailed descriptions of these objects. Moreover, the `matchitmice.data()` function can be used to extract the matched datasets in these objects.

### Complete Data Analysis

The [`MatchIt.mice`](https://cran.r-project.org/package=MatchIt.mice) package and one of its functions, `with()`, provides an easy way to perform complete data analysis on each matched imputed dataset.

The output of the `with()` function will be saved in an object of the `mira` class. The `print()` and `summary()` functions can be used to review detailed descriptions of these objects.

### Pooling the Treatment Effect Size Estimates
The [`MatchIt.mice`](https://cran.r-project.org/package=MatchIt.mice) package and one of its functions, `pool()`, provides the tool to easily pool the obtained treatment effect estimates from complete data analyses according to Rubin’s rules.

The output of the `pool()` function will be saved in an object of the `mipo` class. The `print()` and `summary()` functions can be used to review detailed descriptions of these objects.

## Acknowledgments
I would like to thanks the CRAN team members for their comments and technical support. This package relies on the [`MatchIt`](https://cran.r-project.org/package=MatchIt) and [`mice`](https://cran.r-project.org/package=mice) packages. Please cite their reference manuals and vignettes in your work besides citing reference manual and vignette of this package:

## Author
Farhad Pishgar

[![](https://img.shields.io/twitter/follow/FarhadPishgar.svg?color=yellow&style=for-the-badge)](https://twitter.com/FarhadPishgar)
