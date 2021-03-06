#' @title Evaluates an Expression in Matched Imputed Datasets
#'
#' @keywords function
#'
#' @rdname with.mimids
#'
#' @method with mimids
#'
#' @param data This argument specifies an object of the \code{mimids} class, typically produced by a previous call to the function \code{matchitmice()}.
#' @param expr This argument specifies an expression of the usual syntax of R formula. See \code{help(formula)} for details.
#' @param ... Additional arguments to be passed to \code{expr}.
#'
#' @description The \code{with()} function performs a computation on each of the \code{n} imputed datasets. The typical sequence of steps to do a matching procedure on the imputed datasets are:
#' \enumerate{
#'  \item Impute the missing data points by the \code{mice} function (from the \pkg{mice} package), resulting in a multiple imputed dataset (an object of the \code{mids} class);
#'  \item Match each imputed dataset using a matching model by the \code{matchitmice()} function, resulting in an object of the \code{mimids} class;
#'  \item Fit the model of interest (scientific model) on each matched dataset by the \code{with()} function, resulting in an object of the \code{mira} class;
#'  \item Pool the estimates from each model into a single set of estimates and standard errors, resulting in an object of the \code{mipo} class.
#' }
#'
#' @details The \code{with()} performs a computation on each of the imputed datasets.
#'
#' @return This function returns an object of the \code{mira} class (multiply imputed repeated analyses).
#'
#' @seealso \code{\link[=matchitmice]{matchitmice}}
#'
#' @author Extracted from the \pkg{mice} package written by Stef van Buuren et al. with few changes
#'
#' @references Stef van Buuren and Karin Groothuis-Oudshoorn (2011). \code{mice}: Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical Software}, 45(3): 1-67. \url{https://www.jstatsoft.org/v45/i03/}
#'
#' @export
#'
#' @examples
#' \donttest{
#' #Loading the 'dt.osa' and 'dt.osp' datasets
#' data(dt.osa)
#' data(dt.osp)
#'
#' #Imputing missing data points in the'dt.osa' dataset
#' datasets <- mice(dt.osa, m = 5, maxit = 1,
#'                  method = c("", "", "mean", "", "polyreg", "logreg", "logreg"))
#'
#' #Matching the imputed datasets, 'datasets'
#' matcheddatasets <- matchitmice(KOA ~ SEX + AGE + SMK, datasets,
#'                                approach = 'within', method = 'exact')
#'
#' #Merging the dataframe, 'dt.osp', with each imputed dataset of the 'matcheddatasets' object
#' matcheddatasets <- mergeitmice(matcheddatasets, dt.osp, by = "IDN")
#'
#' #Analyzing the imputed datasets
#' models <- with(data = matcheddatasets,
#'                exp = glm(KOA ~ PTH,
#'                          na.action = na.omit, family = binomial))
#' }

with.mimids <- function(data, expr, ...) {

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
    data.i <- matchitmice.data(data, i)
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
