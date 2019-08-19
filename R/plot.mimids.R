#' @title Plots a \code{mimids} Class Object
#'
#' @keywords functions
#'
#' @aliases plot.mimids
#'
#' @rdname plot.mimids
#'
#' @method plot mimids
#'
#' @param x This argument specifies an object of the \code{mimids} class.
#' @param n This argument specifies the matched imputed dataset number, intended to plot its matching profile. The input must be a positive integer. The default is \code{1}.
#' @param type This argument specifies type of the plot. Currently, \code{"QQ"} (the empirical quantile-quantile plots of each covariate to check balance of marginal distributions), \code{"jitter"} (the jitter plots of the propensity score for control and treatment individuals), and \code{"hist"} (the histograms of the propensity score in the original control and treatment groups and weighted histograms of the propensity score in the matched control and treatment groups) types are available. The default is \code{"QQ"}.
#' @param discrete.cutoff This argument specifies number of values of covariates that are jittered for visibility (for quantile-quantile plots). The input must be a positive integer. The default is \code{5}, meaning that discrete covariates that take 5 or fewer values are jittered.
#' @param numdraws This argument specifies number of observations that are sampled for plotting full or ratio matching (using weights). The input must be a positive integer. The default is \code{5000}.
#' @param interactive This argument specifies whether users can identify individuals by clicking on the graph with the left mouse button and (when applicable) choose subclasses to plot. The input must be a logical value. The default is \code{TRUE}.
#' @param which.xs This argument specifies particular covariate names in a character vector to plot only a subset of the covariates (for quantile-quantile plots). The default is \code{"NULL"}.
#' @param ... Additional arguments to be passed to the \code{plot.mimids()} function.
#'
#' @description The \code{plot.mimids()} function plots an object of the \code{mimids} class.
#'
#' @details The matching profile of the \code{mimids} class objects is plotted.
#'
#' @return NULL
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
#' #Loading the 'handoa' dataset
#' data(handoa)
#'
#' #Imputing the missing data points in the 'handoa' dataset
#' datasets <- mice(handoa, m = 5, maxit = 1,
#'                  method = c("", "", "", "mean", "polyreg", "logreg", "", ""))
#'
#' #Matching the imputed datasets, 'datasets'
#' matcheddatasets <- matchitmice(HANDOA ~ SEX + AGE, datasets)
#'
#' #Plotting data of the first imputed dataset
#' plot1 <- plot(matcheddatasets, n = 1)
#' }

plot.mimids <- function(x, n = 1, type = "QQ", discrete.cutoff = 5,
                        numdraws = 5000, interactive = TRUE, which.xs = NULL, ...){

  #S3 method

  #Importing functions
  #' @importFrom graphics plot
  graphics::plot
  #' @export

  #Based on: The MatchIt:::plot.matchit()
  #URL: <https://cran.r-project.org/package=MatchIt>
  #URL: <https://github.com/kosukeimai/MatchIt>
  #URL: <https://cran.r-project.org/web/packages/MatchIt/MatchIt.pdf>
  #URL: <https://imai.fas.harvard.edu/research/files/matchit.pdf>
  #Authors: Daniel Ho et al.
  #Changes: Few

  #Checking inputs format
  if(x[[1]]$m < n) {stop("The input for the 'n' is out of bounds.")}

  #Polishing variables
  model <- x[[2]][[n + 1]]

  #Printing out
  cat("The matched imputed dataset: #", n,  "\n", sep = "")

  #Plotting
  plot(model, discrete.cutoff = discrete.cutoff, type = type,
       numdraws = numdraws, interactive = interactive, which.xs = which.xs, ...)

}

