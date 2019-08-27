#' @title Weights Multiply Imputed Datasets
#'
#' @keywords function
#'
#' @aliases weightitmice
#'
#' @rdname weightitmice
#'
#' @param formula This argument takes the usual syntax of R formula, \code{y ~ x1 + x2}, where \code{y} is a binary treatment indicator and \code{x1} and \code{x2} are the matching covariates. Both the treatment indicator and matching covariates must be contained in the imputed datasets, which are specified as \code{datasets} (see below). All of the usual R syntax for formula works. For example, \code{x1:x2} represents the first order interaction term between \code{x1} and \code{x2} and \code{I(x1^2)} represents the square term of \code{x1}. See \code{help(formula)} for details.
#' @param datasets This argument specifies the datasets containing the treatment indicator and matching covariates called in the \code{formula}. This argument must be an object of the \code{mids} class, which is typically produced by a previous call to \code{mice()} or \code{mice.mids()} functions from the \pkg{mice} package.
#' @param approach This argument specifies a matching approach. Currently, \code{"within"} (calculating distance measures and matching based on them within each imputed dataset) and \code{"across"} (calculating distance measures within each imputed dataset, averaging distance measure for each observation across imputed datasets, and matching based on the averaged measures in each imputed dataset) approaches are available. The default is \code{"within"}.
#' @param method This argument specifies a matching method. Currently, only \code{"nearest"} (nearest neighbor matching) method is available. The default is \code{"nearest"}. Note that within \code{"nearest"} method, \pkg{MatchIt.mice} offers a variety of options.
#' @param distance This argument specifies the method used to estimate the distance measure. The default is logistic regression, \code{"logit"}. A variety of other methods are available.
#' @param distance.options This optional argument specifies the optional arguments that are passed to the model for estimating the distance measure. The input to this argument should be a list.
#' @param discard This argument specifies whether to discard observations that fall outside some measure of support of the distance score before matching, and not allow them to be used at all in the matching procedure. Note that discarding observations may change the quantity of interest being estimated. The current options are \code{"none"} (discarding no observations before matching), \code{"both"} (discarding all observations, both the control and treatment observations, that are outside the support of the distance measure), \code{"control"} (discarding only control observations outside the support of the distance measure of the treatment observations), and \code{"treat"} (discarding only treatment obsrvations outside the support of the distance measure of the control observations). The default is \code{"none"}.
#' @param reestimate This argument specifies whether the model for estimating the distance measure should be reestimated after observations are discarded. The input must be a logical value. The default is \code{FALSE}.
#' @param ... Additional arguments to be passed to the matching method.
#'
#' @description The \code{weightitmice()} function enables parametric models for causal inference to work better by estimating inverse propensity score weights of the control and treatment observations of each imputed dataset of a \code{mids} class object.
#'
#' @details The weighting is done using the \code{weightitmice(y ~ x1, ...)} command, where \code{y} is the vector of treatment assignments and \code{x1} represents the covariates to be used in the matching model. There are a number of matching options, detailed below. The default syntax is \code{weightitmice(formula, datasets = NULL, method = "nearest", model = "logit", ...)}. Summaries of the results can be seen graphically using \code{plot()} or numerically using \code{summary()} functions. The \code{print()} function also prints out the output.
#'
#' @return This function returns an object of the \code{wimids} (weighted multiply imputed datasets) class, that includes inverse propensity score weights of observations of the imputed datasets (listed as the \code{inverse.weights} variables in each) primarily passed to the function by the \code{datasets} argument.
#'
#' @seealso \code{\link[=wimids]{wimids}}
#' @seealso \code{\link[=with]{with}}
#' @seealso \code{\link[=pool]{pool}}
#'
#' @author Farhad Pishgar
#'
#' @references Daniel Ho, Kosuke Imai, Gary King, and Elizabeth Stuart (2007). Matching as Nonparametric Preprocessing for Reducing Model Dependence in Parametric Causal Inference. \emph{Political Analysis}, 15(3): 199-236. \url{http://gking.harvard.edu/files/abs/matchp-abs.shtml}
#' @references Stef van Buuren and Karin Groothuis-Oudshoorn (2011). \code{mice}: Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical Software}, 45(3): 1-67. \url{https://www.jstatsoft.org/v45/i03/}
#'
#' @export
#'
#' @examples
#' \donttest{
#' #Loading the 'dt.osa' dataset
#' data(dt.osa)
#'
#' #Imputing missing data points in the'dt.osa' dataset
#' datasets <- mice(dt.osa, m = 5, maxit = 1,
#'                  method = c("", "", "mean", "", "polyreg", "logreg", "logreg"))
#'
#' #Weighting the imputed datasets, 'datasets'
#' weighteddatasets <- weightitmice(KOA ~ SEX + AGE + SMK, datasets,
#'                                  approach = 'within', method = 'nearest')
#' }

weightitmice <- function (formula, datasets,
                          approach = "within",
                          method = "nearest", distance = "logit", distance.options = list(),
                          discard = "none", reestimate = FALSE, ...) {

  #External function


  #Importing functions
  #' @importFrom mice is.mids complete
  #' @importFrom MatchIt matchit
  #' @importFrom stats as.formula terms
  mice::is.mids
  mice::complete
  MatchIt::matchit
  stats::as.formula
  stats::terms
  #' @export

  #Polishing variables
  formula <- as.formula(formula)
  if(approach == "pool-then-match") {approach == "across"}
  if(approach == "match-then-pool") {approach == "within"}

  #Checking inputs format
  if(is.null(datasets)) {stop("The input for the datasets must be specified.")}
  if(!is.mids(datasets)) {stop("The input for the datasets must be an object of the 'mids' class.")}
  if(!is.null(datasets$data$distance)) {stop("The input for the datasets shouldn't have a variable named 'distance'.")}
  if(!is.null(datasets$data$weights)) {stop("The input for the datasets shouldn't have a variable named 'weights'.")}
  if(!is.null(datasets$data$inverse.weights)) {stop("The input for the datasets shouldn't have a variable named 'inverse.weights'.")}
  if(method != "nearest") {stop("The input for the matching method must be 'nearest'.")}
  if(approach != "within" && approach != "across") {stop("The input for the matching approach must be either 'within' or 'across'.")}

  #Within
  if (approach == "within") {

    #The raw data
    dataset0 <- datasets$data
    dataset0$distance <- NA
    dataset0$weights <- NA
    dataset0$inverse.weights <- NA
    dataset0$.id <- 1:nrow(datasets$data)
    dataset0$.imp <- 0

    #Defining the lists
    weighteddatasetslist <- list(dataset0)
    matchingmodelslist <- list(0)

    #Longing the datasets
    for (i in 1:datasets$m) {

      #Building the model
      dataset <- complete(datasets, i)
      matchingdataset <- complete(datasets, i)
      model <- matchit(formula, matchingdataset,
                       method = method, distance = distance,
                       distance.options = distance.options, discard = discard,
                       reestimate = reestimate, ...)

      #Printing out
      cat("Weighting imputed dataset: #", i,  "\n", sep = "")

      #Dataset
      measures <- cbind(as.data.frame(model$distance), as.data.frame(model$weights))
      colnames(measures) <- c("distance", "weights")
      dataset <- cbind(dataset, measures)
      dataset$inverse.weights <- NA
      ifelse(dataset[paste(terms(formula)[[2]])] == 1,
             dataset["inverse.weights"] <- 1 / (dataset$distance),
             dataset["inverse.weights"] <- 1 / (1 - dataset$distance))
      dataset$.imp <- i
      dataset$.id <- 1:nrow(datasets$data)

      #Updating the lists
      weighteddatasetslist[i+1] <- list(dataset)
      matchingmodelslist[i+1] <- list(model)
    }

    #Binding the datasets
    weighteddatasets <- do.call("rbind", as.list(noquote(weighteddatasetslist)))
    weighteddatasets <- as2.mids(weighteddatasets)

    #Others
    weightedothers <- list(method)

    #Returning output
    output <- list(weighteddatasets, matchingmodelslist, weightedothers, weighteddatasetslist)
    class(output) <- "wimids"
    return(output)
  }

  #Across
  if (approach == "across") {

    #Raw data
    dataset0 <- datasets$data
    dataset0$average.distance <- NA
    dataset0$average.distance <- 0
    dataset0$distance <- NA
    dataset0$weights <- NA
    dataset0$inverse.weights <- NA
    dataset0$.id <- 1:nrow(datasets$data)
    dataset0$.imp <- 0

    #Defining the lists
    weighteddatasetslist <- list(0)
    matchingmodelslist <- list(0)

    #Calculating the averaged distances
    for (i in 1:datasets$m) {

      #Building the model
      dataset <- complete(datasets, i)
      matchingdataset <- complete(datasets, i)
      model <- matchit(formula, matchingdataset,
                       method = method, distance = distance,
                       distance.options = distance.options, discard = discard,
                       reestimate = reestimate, ...)

      #Printing out
      cat("Calculating distance measures, imputed dataset: #", i,  "\n", sep = "")

      #Measures
      measures <- as.data.frame(model$distance)
      colnames(measures) <- c("distance")

      #Dataset
      dataset <- cbind(dataset, measures)

      #Updating the list
      weighteddatasetslist[i+1] <- list(dataset)
      matchingmodelslist[i+1] <- list(model)
      for (i in 1:nrow(dataset0)) {
        dataset0[i, "average.distance"] <- dataset0[i, "average.distance"] + measures[i, "distance"]
      }
    }

    #Updating the list
    dataset0$average.distance <- (dataset0$average.distance) / (datasets$m)
    average.distance.df <- as.data.frame(dataset0$average.distance)
    colnames(average.distance.df) <- "average.distance"
    weighteddatasetslist[1] <- list(dataset0)

    #Adding averaged distance to datasets
    for (i in 1:(length(weighteddatasetslist) - 1)) {
      dataset <- weighteddatasetslist[[i+1]]
      dataset <- cbind(dataset, average.distance.df)
      dataset$distance <- dataset$average.distance

      #Printing out
      cat("Weighting imputed dataset: #", i,  "\n", sep = "")

      #Doing it
      dataset$inverse.weights <- NA
      ifelse(dataset[paste(terms(formula)[[2]])] == 1,
             dataset["inverse.weights"] <- 1 / (dataset$average.distance),
             dataset["inverse.weights"] <- 1 / (1 - dataset$average.distance))
      dataset$.imp <- i
      dataset$.id <- 1:nrow(datasets$data)
      dataset$weights <- 1

      #Updating the list
      weighteddatasetslist[i+1] <- list(dataset)
    }

    #Binding the datasets
    weighteddatasets <- do.call("rbind", as.list(noquote(weighteddatasetslist)))
    weighteddatasets <- as2.mids(weighteddatasets)

    #Others
    weightedothers <- list(method)

    #Returning output
    output <- list(weighteddatasets, matchingmodelslist, weightedothers, weighteddatasetslist)
    class(output) <- "wimids"
    return(output)
  }
}
