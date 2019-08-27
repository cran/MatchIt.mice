#' @title Outputs Matched Imputed Datasets
#'
#' @keywords function
#'
#' @aliases matchitmice.data
#'
#' @rdname matchitmice.data
#'
#' @param object This argument specifies an object of the \code{mimids} class.
#' @param n This argument specifies the matched imputed dataset number, intended to extract its matching data. The input must be a positive integer. The default is \code{1}.
#'
#' @description The \code{matchitmice.data()} function extracts matching data from an object of the \code{mimids} class.
#'
#' @details The matched datasets wihtin the \code{mimids} class object are extracted.
#'
#' @return This function returns a subset of the imputed dataset after matching with just the matched observations from treatment and control groups.
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
#' \donttest{
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
#'
#' #Extracting data of the first imputed dataset
#' data.1 <- matchitmice.data(matcheddatasets, n = 1)
#' }

matchitmice.data <- function (object, n = 1) {

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

