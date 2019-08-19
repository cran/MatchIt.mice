#' @title Merges Imputed Datasets with Dataframes
#'
#' @keywords functions
#'
#' @aliases mergeitmice
#'
#' @rdname mergeitmice
#'
#' @param datasets This argument specifies an object of the \code{mids}, \code{mimids}, or \code{wimids} class.
#' @param data This argument specifies a data frame.
#' @param by This argument specifies a variable name, present in bot \code{datasets} and \code{data}.
#'
#' @description The \code{mergeitmice()} function merges a dataframe with each imputed dataset of the \code{mids}, \code{mimids}, or \code{wimids} class objects based on the variables passed to the function as \code{by}.
#'
#' @details This functions can be used instead of the \code{cbind()} function (from the \pkg{mice} package).
#'
#' @return This function returns an object of the \code{mids}, \code{mimids}, or \code{wimids} class after merging a dataframe with each imputed dataset of the inputted object.
#'
#' @seealso \code{\link[=matchitmice]{matchitmice}}
#' @seealso \code{\link[=weightitmice]{weightitmice}}
#' @seealso \code{\link[=binditmice]{binditmice}}
#'
#' @author Farhad Pishgar
#'
#' @references Stef van Buuren and Karin Groothuis-Oudshoorn (2011). \code{mice}: Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical Software}, 45(3): 1-67. \url{https://www.jstatsoft.org/v45/i03/}
#'
#' @export
#'
#' @examples
#' \donttest{
#' #Please see the package repository <https://github.com/FarhadPishgar/MatchIt.mice> for details.
#'
#' #Loading and preparing the 'handoa' dataset
#' data(handoa)
#' idenoa <- handoa["ID", "HANDOA"]
#' handoa <- handoa[c("ID", "AGE", "SEX", "BMI", "SMOKING", "HANDUSE", "KNEEOA")]
#'
#' #Imputing the missing data points in the'handoa' dataset
#' datasets <- mice(handoa, m = 5, maxit = 10)
#'
#' #Merging the dataframe, 'idenoa', with each imputed dataset of the 'datasets' object
#' datasets <- mergeitmice(datasets, idenoa, by = "ID")
#' }

mergeitmice <- function(datasets, data, by = "ID") {

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
    data.0 <- merge(data.0, data, by = by, all.x = TRUE, all.y = FALSE)

    #Preparing the list
    datasetlist <- list(0)
    datasetlist[[1]] <- data.0


    #Merging
    for (i in 1:datasets$m) {
      data.i <- complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- merge(data.i, data, by = by, all.x = TRUE, all.y = FALSE)
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
    data.0 <- merge(data.0, data, by = by, all.x = TRUE, all.y = FALSE)

    #Preparing the list
    datasetlist <- list(0)
    datasetlist[[1]] <- data.0

    #Merging
    for (i in 1:datasets$m) {
      data.i <- complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- merge(data.i, data, by = by, all.x = TRUE, all.y = FALSE)
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
    data.0 <- merge(data.0, data, by = by, all.x = TRUE, all.y = FALSE)

    #Preparing the list
    datasetlist <- list(0)
    datasetlist[[1]] <- data.0

    #Binding
    for (i in 1:datasets$m) {
      data.i <- complete(datasets, i)
      data.i$.id <- 1:nrow(datasets$data)
      data.i$.imp <- i
      data.i <- merge(data.i, data, by = by, all.x = TRUE, all.y = FALSE)
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
