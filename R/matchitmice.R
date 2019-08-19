#' @title Matches Multiply Imputed Datasets
#'
#' @aliases matchitmice
#'
#' @rdname matchitmice
#'
#' @param formula This argument takes the usual syntax of R formula, \code{y ~ x1 + x2}, where \code{y} is a binary treatment indicator and \code{x1} and \code{x2} are the matching covariates. Both the treatment indicator and matching covariates must be contained in the imputed datasets, which are specified as \code{datasets} (see below). All of the usual R syntax for formula works. For example, \code{x1:x2} represents the first order interaction term between \code{x1} and \code{x2} and \code{I(x1^2)} represents the square term of \code{x1}. See \code{help(formula)} for details.
#' @param datasets This argument specifies the datasets containing the treatment indicator and matching covariates called in the \code{formula}. This argument must be an object of the \code{mids} class, which is typically produced by a previous call to \code{mice()} or \code{mice.mids()} functions from the \pkg{mice} package.
#' @param method This argument specifies a matching method. Currently, \code{"nearest"} (nearest neighbor matching) and \code{"exact"} (exact matching) methods are available. The default is \code{"nearest"}. Note that within each of these matching methods, \pkg{MatchIt.mice} offers a variety of options.
#' @param distance This argument specifies the method used to estimate the distance measure. The default is logistic regression, \code{"logit"}. A variety of other methods are available.
#' @param distance.options This optional argument specifies the optional arguments that are passed to the model for estimating the distance measure. The input to this argument should be a list.
#' @param discard This argument specifies whether to discard individuals that fall outside some measure of support of the distance score before matching, and not allow them to be used at all in the matching procedure. Note that discarding individuals may change the quantity of interest being estimated. The current options are \code{"none"} (discarding no individuals before matching), \code{"both"} (discarding all individuals, both the control and treatment individuals, that are outside the support of the distance measure), \code{"control"} (discarding only control individuals outside the support of the distance measure of the treatment individuals), and \code{"treat"} (discarding only treatment individuals outside the support of the distance measure of the control individuals). The default is \code{"none"}.
#' @param reestimate This argument specifies whether the model for estimating the distance measure should be reestimated after individuals are discarded. The input must be a logical value. The default is \code{FALSE}.
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
#'
#' #Please see the package repository <https://github.com/FarhadPishgar/MatchIt.mice> for details.
#'
#' #Loading the 'handoa' dataset
#' data(handoa)
#'
#' #Imputing the missing data points in the'handoa' dataset
#' datasets <- mice(handoa, m = 5, maxit = 1,
#'                  method = c("", "", "", "mean", "polyreg", "logreg", "", ""))
#'
#' #Matching the imputed datasets, 'datasets'
#' matcheddatasets <- matchitmice(HANDOA ~ SEX + AGE, datasets, method = 'exact')
#'

matchitmice <- function (formula, datasets,
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

  #Checking inputs format
  if(is.null(datasets)) {stop("The input for the datasets must be specified.")}
  if(!is.mids(datasets)) {stop("The input for the datasets must be an object of the 'mids' class.")}
  if(!is.null(datasets$data$distance)) {stop("The input for the datasets shouldn't have a variable named 'distance'.")}
  if(!is.null(datasets$data$weights)) {stop("The input for the datasets shouldn't have a variable named 'weights'.")}
  if(is.null(datasets$data$ID)) {stop("The input for the datasets must have a unique identifier variable named 'ID'.")}
  if(method != "nearest" && method != "exact") {stop("The input for the matching method must be either 'nearest' or 'exact'.")}

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
    matchingdataset <- complete(datasets, i)
    model <- matchit(formula, matchingdataset,
                     method = method, distance = distance,
                     distance.options = distance.options, discard = discard,
                     reestimate = reestimate, ...)

    #Printing out
    cat("Matching the imputed dataset: #", i,  "\n", sep = "")

    #Matched dataset
    matcheddataset <- match2.data(model, environment = environment())
    matcheddataset_small <- matcheddataset0[c("ID", ".id")]
    matcheddataset <- merge(matcheddataset, matcheddataset_small, by = "ID")

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
