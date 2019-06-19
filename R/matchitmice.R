#' Selects Matched Samples from Multiply Imputed Datasets
#'
#' @param Formula This argument takes the usual syntax of the R formula, "treat ~ x1 + x2", where "treat" is a binary treatment indicator and "x1" and "x2" are the pre-treatment covariates. Both the treatment indicator and pre-treatment covariates must be contained in all of the multiply imputed datasets, which is specified as "Datasets" (see below). All of the usual R syntax for "Formula" works. For example, "x1:x2" represents the first order interaction term between "x1" and "x2", and "I(x1^2)" represents the square term of "x1". See "help(formula)" for details.
#' @param Datasets This argument specifies the datasets containing the variables called in the "Formula". This must be an object of class "mids", which is typically produced by a previous call to "mice()" or "mice.mids()" functions (from the "mice" package).
#' @param Distance This argument specifies the method used to estimate the distance (propensity score) measures. The default is logistic regression, "logit". A variety of other methods are available.
#' @param Distance.options This optional argument specifies the optional arguments that are passed to the model for estimating the distance measures. The input to this argument should be a list.
#' @param Discard This argument specifies whether to discard units that fall outside some measures of support of the distance scores before matching, and not allow them to be used at all in the matching procedure. Note that discarding units may change the quantity of interest being estimated. The options are: "none" (default), which discards no units before matching, "both", which discards all units (treatment and control) that are outside the support of the distance measure, "control", which discards only control units outside the support of the distance measure of the treatment units, and "treat", which discards only treatment units outside the support of the distance measure of the control units.
#' @param Reestimate This argument specifies whether the model for distance measure should be re-estimated after units are discarded. The input must be a logical value. The default is "FALSE".
#' @param Replace This argument specifies whether each control unit can be matched to more than one treatment unit. The input must be a logical value. The default is "FALSE".
#' @param Ratio  This argument specifies the number of control units to match to each treatment unit. The input must be a numeric value. The default is 1.
#' @param Caliper This argument specifies the number of standard deviations of the distance measure within which to draw control units. The input must be a numeric value. The default is 0.
#' @param Calclosest When "Caliper" is greater than 0, this argument specifies whether to take the nearest available match if no matches are available within the "Caliper". The input must be a logical value. The default is "FALSE".
#' @param Mahvars When "Caliper" is greater than 0, this argument specifies variables on which to perform Mahalanobis-metric matching within each "Caliper". The default is "NULL".
#' @param Merging This argument specifies whether a dataset should be merged with each imputed dataset or not. The input must be a logical value. The default is "FALSE".
#' @param MergingData This optional argument specifies the dataset that should be merged with each imputed dataset. The input to this argument should be a dataframe.
#' @param By This optional argument specifies the variable based on which the "MergingData" and imputed datasets should be merged. The input to this argument should be the name of the dataset column. The default is "ID".
#' @param Weighting This argument specifies whether the matched samples should be combined or the inverse propensity score weighted samples. The default is "FALSE", meaning that the matched samples will be returned.
#' @param ... Additional arguments to be passed to the matching method.
#'
#' @return This function returns an object of the "mids" class (multiply imputed datasets), either combined from the matched samples (when the "Weighting" is "FALSE") or from the inverse propensity score weighted samples (when the "Weighting" is "TRUE").
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#' #### Loading Packages
#'
#' #The `mice` and the `MatchIt.mice` packages are loaded.
#'
#' library(mice)
#' library(MatchIt.mice)
#'
#' #### Loading the Datasets
#'
#' #The dataset `HandOsteoarthritis` contains data of 4,704 individuals with (`HANDOA = 1`) or without
#' #(`HANDOA = 0`) hand osteoarthritis. The recorded data has missing values in body mass index
#' #(`BMI`, a quantitative variable), hand use habits (`HANDUSE`, a binary qualitative variable), and
#' #smoking status (`SMOKING`, a categorical qualitative variable).
#'
#' data(HandOsteoarthritis)
#' factorized <- c("SIDE", "SEX", "HANDUSE", "SMOKING", "HANDOA")
#' HandOsteoarthritis[factorized] <- lapply(HandOsteoarthritis[factorized], factor)
#'
#' #The dataset `KneeOsteoarthritis` contains data of 4,796 individuals (including the 4,704
#' #individuals of the `HandOsteoarthritis` dataset) with (`KNEEOA = 1`) or without (`KNEEOA = 0`) knee
#' #osteoarthritis.
#'
#' data(KneeOsteoarthritis)
#' factorized <- c("KNEEOA")
#' KneeOsteoarthritis[factorized] <- lapply(KneeOsteoarthritis[factorized], factor)
#'
#' #### Imputing the Missing Data Points
#'
#' #The `mice()` function from the `mice` package is called to impute the missing data in the
#' #`HandOsteoarthritis` dataset.
#'
#' datasets <- mice(HandOsteoarthritis, m = 5, maxit = 10,
#'                  method = c("", "", "", "", "mean", "polyreg", "logreg", ""))
#'
#' #### Matching the Imputed Datasets
#'
#' #The `matchitmice()` function from the `MatchIt.mice` package is called to select matched samples
#' #(for age, gender, and BMI) from the control (`HANDOA = 0`) and treatment (`HANDOA = 1`) groups of
#' #the each imputed dataset (in a 1:3 ratio and with a calliper of 5% of the standard deviation of the
#' #estimated propensity score values).
#'
#' matcheddatasets <- matchitmice(HANDOA ~ SEX + AGE + BMI, datasets, Ratio = 3, Caliper  =  0.05)
#'
#' #The `matcheddatasets` is an object of the `mids` class and can be used in other studies, using the
#' #`with()` and `pool()` functions (from the `mice`) package).
#'
#' results <- with(data = matcheddatasets,
#'                 exp = glm(HANDOA ~ SMOKING, na.action = na.omit, family = binomial))
#' print(pool(results))
#'
#' #### Estimating Weights of Individuals in the Imputed Datasets
#'
#' #The `matchitmice()` function is applied to estimate inverse propensity score weights (from the
#' #model with age, gender, and BMI as the covariates) for each individual in the control and treatment
#' #groups of each imputed dataset.
#'
#' matcheddatasets <- matchitmice(HANDOA ~ SEX + AGE + BMI, datasets, Weighting = TRUE)
#'
#' #Similarly, the `matcheddatasets` can be used to estimate treatment effects on each imputed dataset.
#'
#' results <- with(data = matcheddatasets,
#'                 exp = glm(HANDOA ~ SMOKING, na.action = na.omit, family = binomial,
#'                 weights = weight))
#' print(pool(results))
#'
#' #### Merging Imputed Dataset with Other Datasets
#'
#' #The `matchitmice()` function is applied to both select matched samples and to merge the imputed
#' #datasets with the `KneeOsteoarthritis` dataset, using the column name specified by the `By`.
#'
#' matcheddatasets <- matchitmice(HANDOA ~ SEX + AGE + BMI, datasets, Ratio = 3, Caliper  =  0.05,
#'                                Merging = TRUE, MergingData = KneeOsteoarthritis, By = "ID")
#'
#' #Similarly, the `matcheddatasets` can be used to estimate treatment effects on each imputed dataset.
#'
#' results <- with(data = matcheddatasets,
#'                 exp = glm(HANDOA ~ KNEEOA, na.action = na.omit, family = binomial))
#' print(pool(results))
#' }
#'
#' #Loading the 'mice' and `MatchIt.mice` packages:
#'
#' library(mice)
#' library(MatchIt.mice)
#'
#' #Loading and polishing the `HandOsteoarthritis` dataset:
#'
#' data(HandOsteoarthritis)
#' factorized <- c("SIDE", "SEX", "HANDUSE", "SMOKING", "HANDOA")
#' HandOsteoarthritis[factorized] <- lapply(HandOsteoarthritis[factorized], factor)
#'
#' #Imputing the missing data points in 1 dataset of the `datasets` object after 1 iteration (In
#' #practice, usually there are 5 imputed datasets after 50 iterations, please see the
#' #https://github.com/FarhadPishgar/MatchIt.mice/ for more details):
#'
#' datasets <- mice(HandOsteoarthritis, m = 1, maxit = 1,
#'                  method = c("", "", "", "", "mean", "polyreg", "logreg", ""))
#'
#' #Selecting matched samples (for age, gender, and body mass index) from the control and treatment
#' #groups of each imputed dataset:
#'
#' matcheddatasets <- matchitmice(HANDOA ~ SEX + AGE + BMI, datasets)

matchitmice <- function (Formula,
                         Datasets,
                         Distance = "logit", Distance.options = list(), Discard = "none", Reestimate = FALSE,
                         Replace = FALSE, Ratio = 1, Caliper = 0, Calclosest = FALSE, Mahvars = NULL,
                         Merging = FALSE, MergingData = NULL, By = "ID",
                         Weighting = FALSE, ...) {

  #Importing functions
  #' @importFrom mice is.mids as.mids complete
  #' @importFrom MatchIt matchit
  #' @importFrom stats as.formula terms
  mice::is.mids
  mice::as.mids
  mice::complete
  MatchIt::matchit
  stats::as.formula
  stats::terms
  #' @export

  #Polishing variables
  Formula <- as.formula(Formula)
  Env <- environment()

  #Checking inputs format
  if(is.null(Datasets)) {stop("Imputed datasets must be specified")}
  if(!is.mids(Datasets)) {stop("Imputed datasets must be an object of the 'mids' class")}
  if(Merging == TRUE & is.null(MergingData)) {stop("The merging dataset must be specified")}
  if(Merging == TRUE & !is.data.frame(MergingData)) {stop("The merging dataset must be a dataframe")}
  if(Merging == TRUE & is.null(By)) {stop("The merging variable(s) must be specified")}
  if(is.null(Datasets$data)) {stop("Imputed datasets must have a unique identifier variable for each row named as ID")}

  cat("This may take few seconds, depending on your system configuration", "\n")
  cat("\n")

  #Defining the raw data
  #Whole data - not imputed (for weighting)
  wdataset0 <- Datasets$data
  wdataset0$distance <- NA
  wdataset0$weight <- NA
  wdataset0$.imp <- 0
  wdataset0$.id <- 1:nrow(Datasets$data)
  if (Merging == TRUE) wdataset0 <- merge(wdataset0, MergingData, by = By, all.x = T)

  #Whole data - not imputed (for matching)
  mdataset0 <- Datasets$data
  mdataset0$distance <- NA
  mdataset0$.id <- 1:nrow(Datasets$data)
  mdataset0$.imp <- 0
  if (Merging == TRUE) mdataset0 <- merge(mdataset0, MergingData, by = By, all.x = T)

  #Defining the DatasetList
  wDatasetsList <- list(wdataset0)
  mDatasetsList <- list(mdataset0)

  #Longing the datasets
  for (i in 1:Datasets$m) {
    #Building the model
    wdataset <- complete(Datasets, i)
    wdataset_for_matchit <- complete(Datasets, i)
    mmodel <- matchit(Formula, wdataset_for_matchit,
                      distance = Distance,
                      distance.options = Distance.options,
                      discard = Discard,
                      reestimate = Reestimate,
                      ratio = Ratio,
                      caliper = Caliper,
                      replace = Replace,
                      calclosest = Calclosest,
                      mahvars = Mahvars)

    #Printing out
    if(Weighting == FALSE) {cat("Matched imputed dataset #", i, ":", "\n", sep = ""); cat("Control group: ", mmodel$nn[2,1], " (of ", mmodel$nn[1,1], ") | Treatment group: ", mmodel$nn[2,2], " (of ", mmodel$nn[1,2], ")", "\n", sep = "")}

    #Printing out
    if(Weighting == TRUE) {cat("Compelete imputed dataset #", i, ":", "\n", sep = ""); cat("Control group: ", mmodel$nn[1,1], " | Treatment group: ", mmodel$nn[1,2], " (estimated weights added)", "\n", sep = "")}

    #weighting datasets
    weights <- as.data.frame(mmodel$distance)
    colnames(weights) <- "distance"
    weights <- cbind(wdataset["ID"], weights)
    weights$weight <- NA
    weights$.imp <- i
    weights$.id <- 1:nrow(Datasets$data)
    weights <- cbind(weights, wdataset[paste(terms(Formula)[[2]])])
    ifelse(weights[paste(terms(Formula)[[2]])] == 1,
           weights$weight <- 1 / (weights$distance),
           weights$weight <- 1 / (1 - weights$distance))
    weights[paste(terms(Formula)[[2]])] <- NULL

    if (Merging == TRUE) weights <- merge(weights, MergingData, by = By, all.x = T)
    weights$ID <- NULL
    wdataset <- cbind(wdataset, weights)
    wDatasetsList[i + 1] <- list(wdataset)

    #matched datasets
    mdataset <- matchmicedata(mmodel, environment = Env)
    mdataset$weights <- NULL
    wdataset_subset <- wDatasetsList[[i + 1]][c("ID", ".id")]
    mdataset <- merge(mdataset, wdataset_subset, by = "ID")

    x = nrow(mdataset) + 1
    for (j in 1:nrow(wdataset0)){
      if (j %in% mdataset$.id) {
        #
      }
      else {
        mdataset[x, ".id"] <- j
        x = x + 1
      }
    }
    mdataset$.imp <- i
    if (Merging == TRUE) mdataset <- merge(mdataset, MergingData, by = By, all.x = T)
    mDatasetsList[i+1] <- list(mdataset)
  }

  #Merging the datasets
  wDatasets0 <- do.call("rbind", as.list(noquote(wDatasetsList)))
  wDatasets <- as.mids(wDatasets0)

  mDatasets0 <- do.call("rbind", as.list(noquote(mDatasetsList)))
  mDatasets <- as.mids(mDatasets0)

  #Returning output
  ifelse(Weighting == FALSE,
         return(mDatasets),
         return(wDatasets))
}
