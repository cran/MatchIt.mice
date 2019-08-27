#' @title Outputs Weighted Imputed Datasets
#'
#' @keywords function
#'
#' @aliases weightitmice.data
#'
#' @rdname weightitmice.data
#'
#' @param object This argument specifies an object of the \code{wimids} class.
#' @param n This argument specifies the weighted imputed dataset number, intended to extract its data. The input must be a positive integer. The default is \code{1}.
#'
#' @description The \code{weightitmice.data()} function extracts data from an object of the \code{wimids} class.
#'
#' @details The weighted datasets within the \code{wimids} class object are extracted.
#'
#' @return This function returns the imputed dataset after weighting with weights of observations included in the dataset (listed as the \code{inverse.weights} variables).
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
#'
#' #Extracting data of the first imputed dataset
#' data.1 <- weightitmice.data(weighteddatasets, n = 1)
#' }

weightitmice.data <- function (object, n = 1) {

  #External function

  #Importing functions
  #' @importFrom stats complete.cases
  stats::complete.cases
  #' @export

  #Checking inputs format
  if(object[[1]]$m < n) {stop("The input for the 'n' is out of bounds.")}

  #Returning the output
  output <- object[[4]][[n + 1]][complete.cases(object[[4]][[n + 1]][ , "weights"]),]
  output$.id <- NULL
  output$.imp <- NULL
  return(output)
}

