#' @title Binds Imputed Datasets and Dataframes
#'
#' @keywords function
#'
#' @aliases binditmice
#'
#' @rdname binditmice
#'
#' @param datasets This argument specifies an object of the \code{mids}, \code{mimids}, or \code{wimids} class.
#' @param data This argument specifies a dataframe.
#'
#' @description The \code{binditmice()} function binds a dataframe to each imputed dataset of the \code{mids}, \code{mimids}, or \code{wimids} class objects in a row-wise fashion.
#'
#' @details This functions can be used instead of the \code{cbind()} function (from the \pkg{mice} package).
#'
#' @return This function returns an object of the \code{mids}, \code{mimids}, or \code{wimids} class after binding a dataframe to each imputed dataset of the inputted object.
#'
#' @seealso  \code{\link[=matchitmice]{matchitmice}}
#' @seealso  \code{\link[=weightitmice]{weightitmice}}
#' @seealso  \code{\link[=mergeitmice]{mergeitmice}}
#'
#' @author Farhad Pishgar
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
#' #Binding the dataframe, 'dt.osp', to each imputed dataset of the 'matcheddatasets' object
#' matcheddatasets <- binditmice(matcheddatasets, dt.osp)
#' }

binditmice <- function(datasets, data) {

  #External function

  #Importing functions
  #' @importFrom mice is.mids complete
  mice::is.mids
  mice::complete
  #' @export

  #Checking inputs format
  if(is.null(datasets)) {stop("The input for the datasets must be specified.")}
  if(is.null(data)) {stop("The input for the data must be specified.")}
  if(!is.mids(datasets) & !is.mimids(datasets) & !is.wimids(datasets)) {stop("The input for the datasets must be an object of the 'mids', 'mimids', or 'wimids' class.")}
  if(!is.data.frame(data)) {stop("The input for the data must be a data frame.")}

  if (is.mids(datasets)) {
    #Polishing variables
    data.0 <- datasets$data
    data.0$.id <- 1:nrow(datasets$data)
    data.0$.imp <- 0
    data.0 <- cbind(data.0, data)

    #Preparing the list
    datasetlist <- list(0)
    datasetlist[[1]] <- data.0

    #Binding
    for (i in 1:datasets$m) {
      data.i <- complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- cbind(data.i, data)
      datasetlist[[i+1]] <- data.i
    }

    #Returning output
    comdatasets <- do.call("rbind", as.list(noquote(datasetlist)))
    newdatasets <- as2.mids(comdatasets)
    return(newdatasets)
  }

if (is.mimids(datasets)) {
  #Polishing variables
  matchingmodelslist <- datasets[[2]]
  matchedothers <- datasets[[3]]
  datasets <- datasets[[1]]

  data.0 <- datasets$data
  data.0$.id <- 1:nrow(datasets$data)
  data.0$.imp <- 0
  data.0 <- cbind(data.0, data)

  #Preparing the list
  datasetlist <- list(0)
  datasetlist[[1]] <- data.0

  #Binding
  for (i in 1:datasets$m) {
    data.i <- complete(datasets, i)
    data.i$.id <- 1:nrow(datasets$data)
    data.i$.imp <- i
    data.i <- cbind(data.i, data)
    datasetlist[[i+1]] <- data.i
  }

  #Prepating the output
  comdatasets <- do.call("rbind", as.list(noquote(datasetlist)))
  newdatasets <- as2.mids(comdatasets)

  #Returning output
  output <- list(newdatasets, matchingmodelslist, matchedothers, datasetlist)
  class(output) <- "mimids"
  return(output)

}

  if (is.wimids(datasets)) {
    #Polishing variables
    matchingmodelslist <- datasets[[2]]
    weightedothers <- datasets[[3]]
    datasets <- datasets[[1]]

    data.0 <- datasets$data
    data.0$.id <- 1:nrow(datasets$data)
    data.0$.imp <- 0
    data.0 <- cbind(data.0, data)

    #Preparing the list
    datasetlist <- list(0)
    datasetlist[[1]] <- data.0

    #Binding
    for (i in 1:datasets$m) {
      data.i <- complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- cbind(data.i, data)
      datasetlist[[i+1]] <- data.i
    }

    #Prepating the output
    comdatasets <- do.call("rbind", as.list(noquote(datasetlist)))
    newdatasets <- as2.mids(comdatasets)

    #Returning output
    output <- list(newdatasets, matchingmodelslist, weightedothers, datasetlist)
    class(output) <- "wimids"
    return(output)

  }
}
