#' @title Outputs Matched Imputed Datasets
#'
#' @aliases matchitmice.data
#'
#' @rdname matchitmice.data
#'
#' @param object This argument specifies an object of the \code{mimids} class.
#' @param n This argument specifies number of the matched imputed dataset intended to extract its matching data. The input must be a positive integer. The default is \code{1}.
#'
#' @description The \code{matchitmice.data()} function extracts matching data from an object of the \code{mimids} class.
#'
#' @details The matched dataset of a \code{mimids} class object is extracted.
#'
#' @return This function returns a subset of the imputed dataset sent to \code{matchitmice()} with just the matched individuals.
#'
#' @seealso \code{\link[=mimids]{mimids}}
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
#' #Matching the imputed datasets, 'datasets'
#' matcheddatasets <- matchitmice(HANDOA ~ SEX + AGE, datasets)
#'
#' #Extracting data of the first imputed dataset
#' data1 <- matchitmice.data(matcheddatasets, n = 1)
#'
#' }

matchitmice.data <- function (object, n = 1) {

  #External function

  #Importing functions
  #' @importFrom stats na.omit
  stats::na.omit
  #' @export

  #Checking inputs format
  if(object[[1]]$m < n) {stop("The input for the 'n' is out of bounds.")}

  #Printing out
  cat("The matched imputed dataset: #", n,  "\n", sep = "")

  #Returning the output
  output <- na.omit(object[[4]][[n + 1]])
  return(output)
}

