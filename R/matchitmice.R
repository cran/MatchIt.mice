#' @title Matches Multiply Imputed Datasets
#'
#' @keywords function
#'
#' @aliases matchitmice
#'
#' @rdname matchitmice
#'
#' @param formula This argument takes the usual syntax of R formula, \code{y ~ x1 + x2}, where \code{y} is a binary treatment indicator and \code{x1} and \code{x2} are the matching covariates. Both the treatment indicator and matching covariates must be contained in the imputed datasets, which are specified as \code{datasets} (see below). All of the usual R syntax for formula works. For example, \code{x1:x2} represents the first order interaction term between \code{x1} and \code{x2} and \code{I(x1^2)} represents the square term of \code{x1}. See \code{help(formula)} for details.
#' @param datasets This argument specifies the datasets containing the treatment indicator and matching covariates called in the \code{formula}. This argument must be an object of the \code{mids} class, which is typically produced by a previous call to \code{mice()} or \code{mice.mids()} functions from the \pkg{mice} package.
#' @param approach This argument specifies a matching approach. Currently, \code{"within"} (calculating distance measures and matching based on them within each imputed dataset) and \code{"across"} (calculating distance measures within each imputed dataset, averaging distance measure for each observation across imputed datasets, and matching based on the averaged measures in each imputed dataset) approaches are available. The default is \code{"within"}.
#' @param method This argument specifies a matching method. Currently, \code{"nearest"} (nearest neighbor matching) and \code{"exact"} (exact matching) methods are available. The default is \code{"nearest"}. Note that within each of these matching methods, \pkg{MatchIt.mice} offers a variety of options.
#' @param distance This argument specifies the method used to estimate the distance measure. The default is logistic regression, \code{"logit"}. A variety of other methods are available.
#' @param distance.options This optional argument specifies the optional arguments that are passed to the model for estimating the distance measure. The input to this argument should be a list.
#' @param discard This argument specifies whether to discard observations that fall outside some measure of support of the distance score before matching, and not allow them to be used at all in the matching procedure. Note that discarding observations may change the quantity of interest being estimated. The current options are \code{"none"} (discarding no observations before matching), \code{"both"} (discarding all observations, both the control and treatment observations, that are outside the support of the distance measure), \code{"control"} (discarding only control observations outside the support of the distance measure of the treatment observations), and \code{"treat"} (discarding only treatment observations outside the support of the distance measure of the control observations). The default is \code{"none"}.
#' @param reestimate This argument specifies whether the model for estimating the distance measure should be reestimated after observations are discarded. The input must be a logical value. The default is \code{FALSE}.
#' @param ... Additional arguments to be passed to the matching method.
#'
#' @description The \code{matchitmice()} function enables parametric models for causal inference to work better by selecting matched subsets of the control and treatment groups of imputed datasets of a \code{mids} class object.
#'
#' @details The matching is done using the \code{matchitmice(y ~ x1, ...)} command, where \code{y} is the vector of treatment assignments and \code{x1} represents the covariates to be used in the matching model. There are a number of matching options, detailed below. The default syntax is \code{matchitmice(formula, datasets = NULL, method = "nearest", model = "logit", ratio = 1, caliper = 0, ...)}. Summaries of the results can be seen graphically using \code{plot()} or numerically using \code{summary()} functions. The \code{print()} function also prints out the output.
#'
#' @return This function returns an object of the \code{mimids} (matched multiply imputed datasets) class, that includes matched subsets of the imputed datasets primarily passed to the function by the \code{datasets} argument.
#'
#' @seealso \code{\link[=mimids]{mimids}}
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
#' #Loading the 'dt.osa' dataset
#' data(dt.osa)
#'
#' #Imputing missing data points in the'dt.osa' dataset
#' datasets <- mice(dt.osa, m = 5, maxit = 1,
#'                  method = c("", "", "mean", "", "polyreg", "logreg", "logreg"))
#'
#' #Matching the imputed datasets, 'datasets'
#' matcheddatasets <- matchitmice(KOA ~ SEX + AGE + SMK, datasets,
#'                                approach = 'within', method = 'exact')

matchitmice <- function (formula, datasets,
                         approach = "within",
                         method = "nearest", distance = "logit", distance.options = list(),
                         discard = "none", reestimate = FALSE, ...) {

  #External function

  #Importing functions
  #' @importFrom mice is.mids complete
  #' @importFrom MatchIt matchit
  #' @importFrom stats as.formula
  mice::is.mids
  mice::complete
  MatchIt::matchit
  stats::as.formula
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
  if(method != "nearest" && method != "exact") {stop("The input for the matching method must be either 'nearest' or 'exact'.")}
  if(approach != "within" && approach != "across") {stop("The input for the matching approach must be either 'within' or 'across'.")}
  if(approach == "across" && method == "exact") {stop("The input for the matching method must be 'nearest', if the 'across' matching approch is selected.")}

  #Within
  if (approach == "within") {

    #The raw data
    matcheddataset0 <- datasets$data
    if (method != "exact") {matcheddataset0$distance <- NA}
    matcheddataset0$weights <- NA
    matcheddataset0$.id <- 1:nrow(datasets$data)
    matcheddataset0$.imp <- 0

    #Defining the lists
    matcheddatasetslist <- list(matcheddataset0)
    matchingmodelslist <- list(0)

    #Longing the datasets
    for (i in 1:datasets$m) {

      #Building the model
      dataset <- complete(datasets, i)
      dataset$.id <- 1:nrow(datasets$data)

      matchingdataset <- complete(datasets, i)
      matchingdataset$.id <- 1:nrow(datasets$data)

      model <- matchit(formula, matchingdataset,
                       method = method, distance = distance,
                       distance.options = distance.options, discard = discard,
                       reestimate = reestimate, ...)

      #Printing out
      cat("Matching imputed dataset: #", i,  "\n", sep = "")

      #Matched dataset
      matcheddataset <- match2.data(model, environment = environment())

      x = nrow(matcheddataset) + 1
      for (j in 1:nrow(matcheddataset0)){
        if (j %in% matcheddataset$.id) {
          #
        }
        else {
          matcheddataset[x, ".id"] <- j
          x = x + 1
        }
      }

      matcheddataset$.imp <- i
      matcheddataset <- matcheddataset[order(matcheddataset$.id),]

      #Updating the lists
      matcheddatasetslist[i+1] <- list(matcheddataset)
      matchingmodelslist[i+1] <- list(model)
    }

    #Binding the datasets
    matcheddatasets <- do.call("rbind", as.list(noquote(matcheddatasetslist)))
    matcheddatasets <- as2.mids(matcheddatasets)

    #Others
    matchedothers <- list(method)

    #Returning output
    output <- list(matcheddatasets, matchingmodelslist, matchedothers, matcheddatasetslist)
    class(output) <- "mimids"
    return(output)
  }

  #Across
  if (approach == "across") {

    #Raw data
    matcheddataset0 <- datasets$data
    matcheddataset0$average.distance <- NA
    matcheddataset0$average.distance <- 0
    matcheddataset0$distance <- NA
    matcheddataset0$weights <- NA
    matcheddataset0$.id <- 1:nrow(datasets$data)
    matcheddataset0$.imp <- 0

    #Defining the lists
    matcheddatasetslist <- list(0)
    matchingmodelslist <- list(0)

    #Calculating the averaged distances
    for (i in 1:datasets$m) {

      #Building the model
      dataset <- complete(datasets, i)
      dataset$.id <- 1:nrow(datasets$data)

      matchingdataset <- complete(datasets, i)
      matchingdataset$.id <- 1:nrow(datasets$data)

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
      dataset$.imp <- i

      #Updating the list
      matcheddatasetslist[i+1] <- list(dataset)
      for (i in 1:nrow(matcheddataset0)) {
        matcheddataset0[i, "average.distance"] <- matcheddataset0[i, "average.distance"] + measures[i, "distance"]
      }
    }

    #Updating the list
    matcheddataset0$average.distance <- (matcheddataset0$average.distance) / (datasets$m)
    average.distance.df <- as.data.frame(matcheddataset0$average.distance)
    colnames(average.distance.df) <- "average.distance"
    matcheddatasetslist[1] <- list(matcheddataset0)

    #Adding averaged distance to datasets
    for (i in 1:(length(matcheddatasetslist) - 1)) {
      dataset <- matcheddatasetslist[[i+1]]
      dataset$distance <- NULL
      dataset <- cbind(dataset, average.distance.df)
      matcheddatasetslist[i+1] <- list(dataset)
    }

    #Matching each dataset
    for (i in 1:datasets$m) {

      #Building the model
      matchingdataset <- matcheddatasetslist[[i+1]]
      model <- matchit(formula, matchingdataset,
                       method = method, distance = matchingdataset$average.distance,
                       distance.options = distance.options, discard = discard,
                       reestimate = reestimate, ...)

      #Printing out
      cat("Matching imputed dataset: #", i,  "\n", sep = "")

      #Matched dataset
      matcheddataset <- match2.data(model, environment = environment())

      x = nrow(matcheddataset) + 1
      for (j in 1:nrow(matcheddataset0)){
        if (j %in% matcheddataset$.id) {
          #
        }
        else {
          matcheddataset[x, ".id"] <- j
          x = x + 1
        }
      }

      matcheddataset$.imp <- i
      matcheddataset <- matcheddataset[order(matcheddataset$.id),]

      #Updating the lists
      matcheddatasetslist[i+1] <- list(matcheddataset)
      matchingmodelslist[i+1] <- list(model)
    }

    #Binding the datasets
    matcheddatasets <- do.call("rbind", as.list(noquote(matcheddatasetslist)))
    matcheddatasets <- as2.mids(matcheddatasets)

    #Others
    matchedothers <- list(method)

    #Returning output
    output <- list(matcheddatasets, matchingmodelslist, matchedothers, matcheddatasetslist)
    class(output) <- "mimids"
    return(output)
  }
}
