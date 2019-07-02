#' Binds Imputed Datasets and Dataframes
#'
#' @aliases binditmice
#'
#' @rdname binditmice
#'
#' @param datasets This argument specifies an object of the \code{mids} class.
#' @param data This argument specifies a dataframe.
#'
#' @description The \code{binditmice()} function binds a dataframe to each imputed dataset of the \code{mids} class object in a row-wise fashion.
#'
#' @details This functions can be used instead of the \code{cbind()} function (from the \pkg{mice} package).
#'
#' @return This function returns an object of the \code{mids} (multiply imputed datasets) class after binding a dataframe to each imputed dataset of the \code{mids} class object.
#'
#' @seealso  \code{\link[=matchitmice]{matchitmice}}
#' @seealso  \code{\link[=weightitmice]{weightitmice}}
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
#' idenoa <- handoa["ID"]
#' handoa <- handoa[c("AGE", "SEX", "BMI", "SMOKING", "HANDUSE", "KNEEOA", "HANDOA")]
#'
#' #Imputing the missing data points in the'handoa' dataset
#' datasets <- mice(handoa, m = 5, maxit = 10)
#'
#' #Binding the data frame, 'idenoa', to each imputed dataset of the 'datasets' object
#' datasets <- binditmice(datasets, idenoa)
#' }

binditmice <- function(datasets, data) {

  #External function

  #Importing functions
  #' @importFrom mice is.mids as.mids complete
  mice::is.mids
  mice::as.mids
  mice::complete
  #' @export

  #Checking inputs format
  if(is.null(datasets)) {stop("The input for the datasets must be specified.")}
  if(is.null(data)) {stop("The input for the data must be specified.")}
  if(!is.mids(datasets)) {stop("The input for the datasets must be an object of the 'mids' class.")}
  if(!is.data.frame(data)) {stop("The input for the data must be a data frame.")}

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
  newdatasets <- as.mids(comdatasets)
  return(newdatasets)
}
