#' Outputs Matched Dataset (Based on match.data() from the MatchIt Package, with Few Changes)
#'
#' @param object The output object from "matchit()" function. This is a required input.
#' @param group This argument specifies for which matched group the user wants to extract the data. Available options are "all" (all matched units), "treat" (matched units in the treatment group), and "control" (matched units in the control group). The default is "all".
#' @param distance This argument specifies the variable name used to store the distance measure. The default is "distance".
#' @param weights This argument specifies the variable name used to store the resulting weights from matching. The default is "weights".
#' @param subclass This argument specifies the variable name used to store the subclass indicator. The default is "subclass".
#' @param environment This argument specifies the environment within which the original dataset is saved. The default is .GlobalEnv.
#'
#' @return This function returns a subset of the original dataset sent to matchit() function, with just the matched units. The dataset also contains the additional variables distance, weights, and subclass. The variable distance gives the estimated distance measure, and weights gives the weights for each unit, generated in the matching procedure. The variable subclass gives the subclass index for each unit (if applicable). See the http://gking.harvard.edu/matchit/ for more details.
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#' #Loading the `mice`, `MatchIt`, and `MatchIt.mice` packages:
#'
#' library(mice)
#' library(MatchIt)
#' library(MatchIt.mice)
#'
#' #The dataset `HandOsteoarthritis` contains data of 4,704 individuals with (`HANDOA = 1`) or without
#' #(`HANDOA = 0`) hand osteoarthritis. The recorded data has missing values in body mass index (`BMI`,
#' #a quantitative variable), hand use habits (`HANDUSE`, a binary qualitative variable), and smoking
#' #status (`SMOKING`, a categorical qualitative variable).
#' #Loading and polishing the `HandOsteoarthritis` dataset:
#'
#' data(HandOsteoarthritis)
#' factorized <- c("SIDE", "SEX", "HANDUSE", "SMOKING", "HANDOA")
#' HandOsteoarthritis[factorized] <- lapply(HandOsteoarthritis[factorized], factor)
#'
#' #The `mice()` function is used to impute the missing data points in the `HandOsteoarthritis`
#' #dataset.
#' #Calling the `mice()` function to impute 5 datasets after 50 iterations:
#'
#' datasets <- mice(HandOsteoarthritis, m = 5, maxit = 10,
#'                  method = c("", "", "", "", "mean", "polyreg", "logreg", ""))
#'
#' #The `matchit()` is used to match two samples of individuals with and without hand osteoarthritis
#' #for age, gender, and BMI, within the first imputed dataset.
#' #Calling the `macthit()` function to match individuals with and without hand osteoarthritis:
#'
#' matchedmodel <- matchit(HANDOA ~ AGE + SEX + BMI, complete(datasets, 1))
#'
#' #The `matchmicedata()` is used to extract a subset of the original dataset, with just the matched
#' #units (based on match.data() from the MatchIt package, with few changes).
#' #Calling the `matchmicedata()` function:
#'
#' matcheddataset <- matchmicedata(matchedmodel, environment = environment())
#' }

matchmicedata <- function(object, group = "all", distance = "distance",
                           weights = "weights", subclass = "subclass", environment = .GlobalEnv) {

  data <- eval(object$call$data, envir = environment)
  treat <- object$treat
  wt <- object$weights
  vars <- names(data)
  if (distance %in% vars)
    stop("invalid input for distance. choose a different name.")
  else if (!is.null(object$distance)) {
    dta <- data.frame(cbind(data, object$distance))
    names(dta) <- c(names(data), distance)
    data <- dta
  }
  if (weights %in% vars)
    stop("invalid input for weights. choose a different name.")
  else if (!is.null(object$weights)){
    dta <- data.frame(cbind(data, object$weights))
    names(dta) <- c(names(data), weights)
    data <- dta
  }
  if (subclass %in% vars)
    stop("invalid input for subclass. choose a different name.")
  else if (!is.null(object$subclass)){
    dta <- data.frame(cbind(data, object$subclass))
    names(dta) <- c(names(data), subclass)
    data <- dta
  }
  if (group == "all")
    return(data[wt > 0,])
  else if (group == "treat")
    return(data[wt > 0 & treat == 1,])
  else if (group == "control")
    return(data[wt > 0 & treat == 0,])
  else
    stop("error: invalid input for group.")
}
