#' @title Weights Multiply Imputed Datasets
#'
#' @keywords functions
#'
#' @aliases weightitmice
#'
#' @rdname weightitmice
#'
#' @param formula This argument takes the usual syntax of R formula, \code{y ~ x1 + x2}, where \code{y} is a binary treatment indicator and \code{x1} and \code{x2} are the matching covariates. Both the treatment indicator and matching covariates must be contained in the imputed datasets, which are specified as \code{datasets} (see below). All of the usual R syntax for formula works. For example, \code{x1:x2} represents the first order interaction term between \code{x1} and \code{x2} and \code{I(x1^2)} represents the square term of \code{x1}. See \code{help(formula)} for details.
#' @param datasets This argument specifies the datasets containing the treatment indicator and matching covariates called in the \code{formula}. This argument must be an object of the \code{mids} class, which is typically produced by a previous call to \code{mice()} or \code{mice.mids()} functions from the \pkg{mice} package.
#' @param method This argument specifies a matching method. Currently, only \code{"nearest"} (nearest neighbor matching) method is available. The default is \code{"nearest"}. Note that within \code{"nearest"} method, \pkg{MatchIt.mice} offers a variety of options.
#' @param distance This argument specifies the method used to estimate the distance measure. The default is logistic regression, \code{"logit"}. A variety of other methods are available.
#' @param distance.options This optional argument specifies the optional arguments that are passed to the model for estimating the distance measure. The input to this argument should be a list.
#' @param discard This argument specifies whether to discard individuals that fall outside some measure of support of the distance score before matching, and not allow them to be used at all in the matching procedure. Note that discarding individuals may change the quantity of interest being estimated. The current options are \code{"none"} (discarding no individuals before matching), \code{"both"} (discarding all individuals, both the control and treatment individuals, that are outside the support of the distance measure), \code{"control"} (discarding only control individuals outside the support of the distance measure of the treatment individuals), and \code{"treat"} (discarding only treatment individuals outside the support of the distance measure of the control individuals). The default is \code{"none"}.
#' @param reestimate This argument specifies whether the model for estimating the distance measure should be reestimated after individuals are discarded. The input must be a logical value. The default is \code{FALSE}.
#' @param ... Additional arguments to be passed to the matching method.
#'
#' @description The \code{weightitmice()} function enables parametric models for causal inference to work better by estimating inverse propensity score weights of the control and treatment individuals of each imputed dataset of a \code{mids} class object.
#'
#' @details The weighting is done using the \code{weightitmice(y ~ x1, ...)} command, where \code{y} is the vector of treatment assignments and \code{x1} represents the covariates to be used in the matching model. There are a number of matching options, detailed below. The default syntax is \code{weightitmice(formula, datasets = NULL, method = "nearest", model = "logit", ...)}. Summaries of the results can be seen graphically using \code{plot()} or numerically using \code{summary()} functions. The \code{print()} function also prints out the output.
#'
#' @return This function returns an object of the \code{wimids} (weighted multiply imputed datasets) class, that includes inverse propensity score weights of individuals of the imputed datasets (listed as the \code{inverse.weights} variables in each) primarily passed to the function by the \code{datasets} argument.
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
#' #Please see the package repository <https://github.com/FarhadPishgar/MatchIt.mice> for details.
#'
#' #Loading the 'handoa' dataset
#' data(handoa)
#'
#' #Imputing the missing data points in the 'handoa' dataset
#' datasets <- mice(handoa, m = 5, maxit = 1,
#'                  method = c("", "", "", "mean", "polyreg", "logreg", "", ""))
#'
#' #Weighting the imputed datasets, 'datasets'
#' weighteddatasets <- weightitmice(HANDOA ~ SEX + AGE, datasets)
#' }

weightitmice <- function (formula, datasets,
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

  #Checking inputs format
  if(is.null(datasets)) {stop("The input for the datasets must be specified.")}
  if(!is.mids(datasets)) {stop("The input for the datasets must be an object of the 'mids' class.")}
  if(!is.null(datasets$data$distance)) {stop("The input for the datasets shouldn't have a variable named 'distance'.")}
  if(!is.null(datasets$data$weights)) {stop("The input for the datasets shouldn't have a variable named 'weights'.")}
  if(!is.null(datasets$data$inverse.weights)) {stop("The input for the datasets shouldn't have a variable named 'inverse.weights'.")}
  if(is.null(datasets$data$ID)) {stop("The input for the datasets must have a unique identifier variable named 'ID'.")}
  if(method != "nearest") {stop("The input for the matching method must be 'nearest'.")}

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
    cat("Weighting the imputed dataset: #", i,  "\n", sep = "")

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
