# MatchIt.mice <img src="man/figure/logo.png" align="right" width="120" />

<!-- badges: start -->
#### Selecting Matched Samples from Multiply Imputed Datasets
<!-- badges: end -->

[![](https://img.shields.io/badge/CRAN%20version-1.0.3-orange.svg?color=success&style=for-the-badge)](https://cran.r-project.org/package=MatchIt.mice)
[![](https://img.shields.io/badge/github%20version-1.0.3-orange.svg?color=red&style=for-the-badge)](https://github.com/FarhadPishgar/MatchIt.mice)

## Introduction

One of the major issues in estimating propensity scores for a sample is the presence of missing data points since the propensity scores cannot be estimated for individuals with at least one missing covariate value. Despite the two standard approaches of complete case analysis and the missingness pattern approach, a popular alternative is to multiply impute the missing data points. However, as [Leyrat et al.](https://www.ncbi.nlm.nih.gov/pubmed/28573919) recently mentioned, there are several unresolved questions about using a multiple imputation algorithm before estimating propensity scores for a sample. In this regard, there are three approaches to combine information from the imputed datasets:

1.	Combining the treatment effects estimated on each imputed dataset
2.	Combining for each individual their propensity score values across the imputed datasets
3.	Combing for each individual their imputed covariate value across the imputed datasets

It is not clear which approach is superior, however, as [Leyrat et al.](https://www.ncbi.nlm.nih.gov/pubmed/28573919) mentioned the first approach seems to reflect the multiple imputation philosophy better, by applying the full analysis strategy on each imputed dataset, and shows good balancing properties between control and treatment groups.

There are several popular R packages for imputing the missing data points, including the [`mice`](https://cran.r-project.org/package=mice), [`Amelia`](https://cran.r-project.org/package=Amelia), [`missForest`](https://cran.r-project.org/package=missForest), [`Hmisc`](https://cran.r-project.org/package=Hmisc), and [`mi`](https://cran.r-project.org/package=mi) packages, and among these, the [`mice`](https://cran.r-project.org/package=mice) package is widely used since it can impute both ignorable and nonignorable (please see works of [Resseguier et al.](https://www.ncbi.nlm.nih.gov/pubmed/21293212) for details) missing data points. The [`MatchIt.mice`](https://github.com/FarhadPishgar/MatchIt.mice) package applies the first approach in combining information from the multiply imputed datasets from the [`mice`](https://cran.r-project.org/package=mice) package and selects matched samples from the control and treatment groups of each imputed datasets and estimates the weight of each individual in the complete distribution of control and treatment groups of the imputed datasets.

In more details, the [`MatchIt.mice`](https://github.com/FarhadPishgar/MatchIt.mice) package uses the `matchit()` function (from the [`MatchIt`](https://cran.r-project.org/package=MatchIt) package) to selects matched samples from the control and treatment groups of each imputed dataset saved in an object of the `mids` class (from the [`mice`](https://cran.r-project.org/package=mice) package) and estimates weights of individuals in the complete distribution of control and treatment groups of the imputed datasets (using the inverse propensity score weighting method). This package returns the results as an object of the `mids` class, and hence, the `with()` and `pool()` functions (from the [`mice`](https://cran.r-project.org/package=mice) package) can be applied on them to combine the treatment effects estimated on each imputed dataset (preferably in the [linear, logistic, or ordinal logistic] regression studies).

## Installation

The [`MatchIt.mice`](https://github.com/FarhadPishgar/MatchIt.mice) package can be installed both from the GitHub and the Comprehensive R Archive Network (CRAN) repositories.

#### CRAN

The latest (and stable) version can be installed from the CRAN as follows:

``` r
install.packages("MatchIt.mice")
```

#### GitHub

The latest (and unstable) version can be installed from the GitHub as follows:

``` r
install.packages("devtools")
devtools::install_github(repo = "FarhadPishgar/MatchIt.mice")
```

## Examples

#### Loading Packages

The [`mice`](https://cran.r-project.org/package=mice) and the [`MatchIt.mice`](https://github.com/FarhadPishgar/MatchIt.mice) packages are loaded.

``` r
library(mice)
library(MatchIt.mice)
```

#### Loading the Datasets

The dataset `HandOsteoarthritis` contains data of 4,704 individuals with (`HANDOA = 1`) or without (`HANDOA = 0`) hand osteoarthritis. The recorded data has missing values in body mass index (`BMI`, a quantitative variable), hand use habits (`HANDUSE`, a binary qualitative variable), and smoking status (`SMOKING`, a categorical qualitative variable).

``` r
data(HandOsteoarthritis)
factorized <- c("SIDE", "SEX", "HANDUSE", "SMOKING", "HANDOA")
HandOsteoarthritis[factorized] <- lapply(HandOsteoarthritis[factorized], factor)
```

The dataset `KneeOsteoarthritis` contains data of 4,796 individuals (including the 4,704 individuals of the `HandOsteoarthritis` dataset) with (`KNEEOA = 1`) or without (`KNEEOA = 0`) knee osteoarthritis.

``` r
data(KneeOsteoarthritis)
factorized <- c("KNEEOA")
KneeOsteoarthritis[factorized] <- lapply(KneeOsteoarthritis[factorized], factor)
```

#### Imputing the Missing Data Points

The `mice()` function from the [`mice`](https://cran.r-project.org/package=mice) package is called to impute the missing data in the `HandOsteoarthritis` dataset.

``` r
datasets <- mice(HandOsteoarthritis, m = 5, maxit = 10,
                 method = c("", "", "", "", "mean", "polyreg", "logreg", ""))
```

#### Matching the Imputed Datasets

The `matchitmice()` function from the [`MatchIt.mice`](https://github.com/FarhadPishgar/MatchIt.mice) package is called to select matched samples (for age, gender, and BMI) from the control (`HANDOA = 0`) and treatment (`HANDOA = 1`) groups of the each imputed dataset (in a 1:3 ratio and with a calliper of 5% of the standard deviation of the estimated propensity score values).

``` r
matcheddatasets <- matchitmice(HANDOA ~ SEX + AGE + BMI, datasets, Ratio = 3, Caliper  =  0.05)
```

The `matcheddatasets` is an object of the `mids` class and can be used in other studies, using the `with()` and `pool()` functions (from the [`mice`](https://cran.r-project.org/package=mice) package).

``` r
results <- with(data = matcheddatasets,
                exp = glm(HANDOA ~ SMOKING, na.action = na.omit, family = binomial))
print(pool(results))
```

#### Estimating Weights of Individuals in the Imputed Datasets
The `matchitmice()` function is applied to estimate inverse propensity score weights (from the model with age, gender, and BMI as the covariates) for each individual in the control and treatment groups of each imputed dataset.

``` r
matcheddatasets <- matchitmice(HANDOA ~ SEX + AGE + BMI, datasets, Weighting = TRUE)
```

Similarly, the `matcheddatasets` can be used to estimate treatment effects on each imputed dataset.

``` r
results <- with(data = matcheddatasets,
                exp = glm(HANDOA ~ SMOKING, weights = weight, na.action = na.omit, family = binomial))
print(pool(results))
```

#### Merging Imputed Dataset with Other Datasets

The `matchitmice()` function is applied to both select matched samples and to merge the imputed datasets with the `KneeOsteoarthritis` dataset, using the column name specified by the `By`.

``` r
matcheddatasets <- matchitmice(HANDOA ~ SEX + AGE + BMI, datasets, Ratio = 3, Caliper  =  0.05,
                               Merging = TRUE, MergingData = KneeOsteoarthritis, By = "ID")
```

Similarly, the `matcheddatasets` can be used to estimate treatment effects on each imputed dataset.

``` r
results <- with(data = matcheddatasets,
                exp = glm(HANDOA ~ KNEEOA, na.action = na.omit, family = binomial))
print(pool(results))
```

## Limitations

#### The Missing Data

The `matchitmice()` function replaces data of individuals not selected in the final matched dataset with `NA`. Please remember to include the argument `na.action = na.omit` in your regression models while analyzing outputs of this function.

#### The Matching Method

Currently, the `matchitmice()` function uses the nearest neighbor matching method for selecting matched samples from control and treatment groups. I am currently working on including other matching methods, and hopefully, they will be added to the future versions of this package.

#### The Unique Identifier Variable

The original dataset (before multiple imputation procedure) must have a unique identifier variable for each individual, named as `ID`.

## Acknowledgments
I would like to thanks the CRAN team members for their comments and technical support. I also thank the [Freepik](https://www.freepik.com/) team from the [FLATICON](https://www.flaticon.com/) group for the designing the free icon, I used in the logo of this package. Moreover, this package relies on the following packages. Please cite their reference manuals and vignettes in your work besides citing reference manual of this package:
1. [`MatchIt`](https://cran.r-project.org/package=MatchIt) package
2. [`mice`](https://cran.r-project.org/package=mice) package

## Author
Farhad Pishgar

[![](https://img.shields.io/twitter/follow/FarhadPishgar.svg?color=informational&style=for-the-badge)](https://twitter.com/FarhadPishgar)
