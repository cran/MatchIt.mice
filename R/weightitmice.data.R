#' @title Outputs Weighted Imputed Datasets
#'
#' @aliases weightitmice.data
#'
#' @rdname weightitmice.data
#'
#' @param object This argument specifies an object of the \code{wimids} class.
#' @param n This argument specifies number of the weighted imputed dataset intended to extract its data. The input must be a positive integer. The default is \code{1}.
#'
#' @description The \code{weightitmice.data()} function extracts data from an object of the \code{wimids} class.
#'
#' @details The weighted dataset of a \code{wimids} class object is extracted.
#'
#' @return This function returns the imputed dataset sent to \code{weightitmice()} with weights of individuals of the datasets added (listed as the \code{inverse.weights} variables).
#'
#' @seealso \code{\link[=wimids]{wimids}}
#'
#' @author Farhad Pishgar
#'
#' @references Daniel Ho, Kosuke Imai, Gary King, and Elizabeth Stuart (2007). Matching as Nonparametric Preprocessing for Reducing Model Dependence in Parametric Causal Inference. \emph{Political Analysis}, 15(3): 199-236. \url{http://gking.harvard.edu/files/abs/matchp-abs.shtml}
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#'
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
#'
#' #Extracting data of the first imputed dataset
#' data1 <- weightitmice.data(weighteddatasets, n = 1)
#'
#' }

weightitmice.data <- function (object, n = 1) {

  #External function

  #Checking inputs format
  if(object[[1]]$m < n) {stop("The input for the 'n' is out of bounds.")}

  #Printing out
  cat("The weighted imputed dataset: #", n,  "\n", sep = "")

  #Returning the output
  output <- object[[4]][[n + 1]]
  return(output)
}

