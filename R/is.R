#' @title Checks for the \code{mimids} Class
#'
#' @keywords function
#'
#' @aliases is.mimids
#'
#' @rdname is.mimids
#'
#' @param object This argument specifies the object that should be checked to see if is of the \code{mimids} class or not.
#'
#' @description The \code{is.mimids()} function checks whether class of objects are \code{mimids} or not.
#'
#' @details The class of objects are checked to be of the \code{mimids}.
#'
#' @return This function returns a logical value indicating whether \code{object} is of the \code{mimids} class.
#'
#' @seealso \code{\link[=matchitmice]{matchitmice}}
#' @seealso \code{\link[=mimids]{mimids}}
#'
#' @author Farhad Pishgar
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
#' #Checking the 'matcheddatasets' object
#' is.mimids(matcheddatasets)
#' is(matcheddatasets)
#' }

is.mimids <- function(object) {

  #Importing functions
  #' @importFrom methods is
  methods::is
  #' @export

  output <- is(object, "mimids")
  return(output)
}

#' @title Checks for the \code{wimids} Class
#'
#' @keywords function
#'
#' @aliases is.wimids
#'
#' @rdname is.wimids
#'
#' @param object This argument specifies the object that should be checked to see if is of the \code{wimids} class or not.
#'
#' @description The \code{is.wimids()} function checks whether class of objects are \code{wimids} or not.
#'
#' @details The class of objects are checked to be of the \code{wimids}.
#'
#' @return This function returns a logical value indicating whether \code{object} is of the \code{wimids} class.
#'
#' @seealso \code{\link[=weightitmice]{weightitmice}}
#' @seealso \code{\link[=wimids]{wimids}}
#'
#' @author Farhad Pishgar
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
#' #Checking the 'weighteddatasets' object
#' is.wimids(weighteddatasets)
#' is(weighteddatasets)
#' }

is.wimids <- function(object) {

  #Importing functions
  #' @importFrom methods is
  methods::is
  #' @export

  output <- is(object, "wimids")
  return(output)
  }
