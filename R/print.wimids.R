#' @title Prints a \code{wimids} Class Object
#'
#' @aliases print.wimids
#'
#' @rdname print.wimids
#'
#' @method print wimids
#'
#' @param x This argument specifies an object of the \code{wimids} class.
#' @param n This argument specifies number of the weighted imputed dataset intended to print its matching profile. The input must be a positive integer. The default is \code{1}.
#' @param digits This argument specifies minimal number of significant digits.
#' @param ... Additional arguments to be passed to the \code{print.wimids()} function.
#'
#' @description The \code{print.wimids()} function prints an object of the \code{wimids} class.
#'
#' @details The matching profile of the \code{wimids} class objects is printed.
#'
#' @return NULL
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
#' #Printing data of the first imputed dataset
#' print1 <- print(weighteddatasets, n = 1)
#' }

print.wimids <- function(x, n = 1, digits = getOption("digits"), ...) {

  #S3 method

  #Based on: The MatchIt:::print.matchit()
  #URL: <https://cran.r-project.org/package=MatchIt>
  #URL: <https://github.com/kosukeimai/MatchIt>
  #URL: <https://cran.r-project.org/web/packages/MatchIt/MatchIt.pdf>
  #URL: <https://imai.fas.harvard.edu/research/files/matchit.pdf>
  #Authors: Daniel Ho et al.
  #Changes: Some

  #Checking inputs format
  if(x[[1]]$m < n) {stop("The input for the 'n' is out of bounds.")}

  #Printing out
  cat("The matched imputed dataset: #", n,  "\n", sep = "")
  if (x[[3]][[1]] == 'exact') {cat("\nExact subclasses: ", max(x[[2]][[n + 1]]$subclass, na.rm = TRUE), "\n", sep="")}
  cat("\nSample sizes: ", sep="\n")

  #Printing
  print(x[[2]][[n + 1]]$nn)

}
