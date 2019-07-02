#' @title Evaluates an Expression in Weighted Imputed Datasets
#'
#' @rdname with.wimids
#'
#' @method with wimids
#'
#' @param data This argument specifies an object of the \code{wimids} class, typically produced by a previous call to the function \code{weightitmice()}.
#' @param expr This argument specifies an expression of the usual syntax of R formula. See \code{help(formula)} for details.
#' @param ... Additional arguments to be passed to \code{expr}.
#'
#' @description The \code{with()} function performs a computation on each of the \code{m} imputed datasets. The typical sequence of steps to estimate weights of individuals of the imputed datasets are:
#' \enumerate{
#'  \item Impute the missing data points by the \code{mice} function (from the \pkg{mice} package), resulting in a multiple imputed dataset (an object of the \code{mids} class);
#'  \item Estimate weights of individuals in the imputed datasets by the \code{weightitmice()} function, resulting in an object of the \code{wimids} class;
#'  \item Fit the model of interest (scientific model) on each weighted dataset by the \code{with()} function, resulting in an object of the \code{mira} class;
#'  \item Pool the estimates from each model into a single set of estimates and standard errors, resulting in an object of the \code{mipo} class.
#' }
#'
#' @details The \code{with()} performs a computation on each of the imputed datasets.
#'
#' @return This function returns an object of the \code{mira} class (multiply imputed repeated analyses).
#'
#' @seealso \code{\link[=weightitmice]{weightitmice}}
#'
#' @author Extracted from the \pkg{mice} package written by Stef van Buuren et al.
#'
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
#'
#' #Analyzing the imputed datasets
#' results <- with(data = weighteddatasets,
#'                 exp = glm(HANDOA ~ SMOKING, weights = inverse.weights,
#'                           na.action = na.omit, family = binomial))
#' }

with.wimids <- function(data, expr, ...) {

  #S3 method

  #Based on: The mice:::with.mids()
  #URL: <https://cran.r-project.org/package=mice>
  #URL: <https://github.com/stefvanbuuren/mice>
  #URL: <https://cran.r-project.org/web/packages/mice/mice.pdf>
  #URL: <https://www.jstatsoft.org/article/view/v045i03/v45i03.pdf>
  #Authors: Stef van Buuren et al.
  #Changes: Few

  #Polishing variables
  object <- data[[1]]
  call <- match.call()
  analyses <- as.list(seq_len(object$m))

  #Do the repeated analysis, store the result.
  for (i in seq_along(analyses)) {
    data.i <- weightitmice.data(data, i)
    analyses[[i]] <- eval(expr = substitute(expr), envir = data.i, enclos = parent.frame())
    if (is.expression(analyses[[i]]))
      analyses[[i]] <- eval(expr = analyses[[i]], envir = data.i, enclos = parent.frame())
  }

  #Return the complete data analyses as a list of length nimp
  output <- list(call = call, call1 = object$call, nmis = object$nmis, analyses = analyses)

  #Return the output
  oldClass(output) <- c("mira", "matrix")
  return(output)

}
