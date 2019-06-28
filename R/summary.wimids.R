#' @title Summarizes a \code{wimids} Class Object
#'
#' @aliases summary.wimids
#'
#' @rdname summary.wimids
#'
#' @method summary wimids
#'
#' @param object This argument specifies an object of the \code{wimids} class.
#' @param n This argument specifies number of the weighted imputed dataset intended to summarize its matching profile. The input must be a positive integer. The default is \code{1}.
#' @param interactions This argument specifies whether to show the balance of all squares and interactions of the covariates used in the weighting procedure. The input must be a logical value. The default is \code{FALSE}
#' @param addlvariables This argument specifies whether to provide balance measures on additional variables not included in the original weighting procedure. The input should be a list. The default is \code{NULL}
#' @param standardize This argument specifies whether to print out standardized versions of the balance measures, where the mean difference is standardized (divided) by the standard deviation in the original treated group. The input must be a logical value. The default is \code{FALSE}.
#' @param ... Additional arguments to be passed to the \code{summary.wimids()} function.
#'
#' @description The \code{summary.wimids()} function summarizes an object of the \code{wimids} class.
#'
#' @details The matching profile of the \code{wimids} class objects is summarized.
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
#'
#' \donttest{
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
#' #Summarizing data of the first imputed dataset
#' summ1 <- summary(weighteddatasets, n = 1)
#'
#' }

summary.wimids <- function(object, n = 1, interactions = FALSE, addlvariables = NULL,
                           standardize = FALSE, ...) {

  #S3 method

  #Based on: The MatchIt:::summary.matchit()
  #URL: <https://cran.r-project.org/package=MatchIt>
  #URL: <https://github.com/kosukeimai/MatchIt>
  #URL: <https://cran.r-project.org/web/packages/MatchIt/MatchIt.pdf>
  #URL: <https://imai.fas.harvard.edu/research/files/matchit.pdf>
  #Authors: Daniel Ho et al.
  #Changes: Some

  #Checking inputs format
  if(object[[1]]$m < n) {stop("The input for the 'n' is out of bounds.")}

  #Printing out
  cat("The matched imputed dataset: #", n,  "\n", sep = "")

  #Summarizing
  #if (object[[3]][[1]] == 'exact') {
  #  cat("\nSample sizes:", "\n", sep = "")
  #  print(summary(object[[2]][[n + 1]])[[2]])
  #  cat("\n    Matched sample sizes by subclass:", "\n", sep = "")
  #  print(summary(object[[2]][[n + 1]])[[1]])
  #
  #}

  if (object[[3]][[1]] == 'nearest') {
    cat("\nSummary of balance for all data:", "\n", sep = "")
    print(summary(object[[2]][[n + 1]], interactions = interactions, addlvariables = addlvariables, standardize = standardize)[[3]])
    cat("\nSummary of balance for matched data:", "\n", sep = "")
    print(summary(object[[2]][[n + 1]], interactions = interactions, addlvariables = addlvariables, standardize = standardize)[[4]])
    cat("\nPercent balance improvement:", "\n", sep = "")
    print(summary(object[[2]][[n + 1]], interactions = interactions, addlvariables = addlvariables, standardize = standardize)[[5]])
    cat("\nSample sizes:", "\n", sep = "")
    print(summary(object[[2]][[n + 1]], interactions = interactions, addlvariables = addlvariables, standardize = standardize)[[2]])

  }
}
