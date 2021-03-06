% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pool.R
\name{pool}
\alias{pool}
\title{Combines Estimates by Rubin’s Rules}
\usage{
pool(object, dfcom = NULL)
}
\arguments{
\item{object}{This argument specifies an object of the \code{mira} class (produced by a previous call to \code{with()} function) or a list with model fits.}

\item{dfcom}{This argument specifies a positive number representing the degrees of freedom in the complete data analysis. The default is \code{NULL}, which means to extract this information from the first fitted model or the fitted model with the lowest number of observations (when that fails the warning \code{Large sample assumed} is printed and the parameter is set to \code{999999}).}
}
\value{
This function returns an object of the \code{mipo} class (multiple imputation pooled outcome).
}
\description{
The \code{pool()} function combines the estimates from \code{n} repeated complete data analyses. The typical sequence of steps to do a matching procedure or estimating weights of observations of the imputed datasets are:
\enumerate{
 \item Impute the missing data points by the \code{mice} function (from the \pkg{mice} package), resulting in a multiple imputed dataset (an object of the \code{mids} class);
 \item Match each imputed dataset using a matching model by the \code{matchitmice()} function, resulting in an object of the \code{mimids} class or estimate weights of observations in the imputed datasets by the \code{weightitmice()} function, resulting in an object of the \code{wimids} class;
 \item Fit the model of interest (scientific model) on each matched or weighted dataset by the \code{with()} function, resulting in an object of the \code{mira} class;
 \item Pool the estimates from each model into a single set of estimates and standard errors, resulting in an object of the \code{mipo} class.
}
}
\details{
The \code{pool()} function averages the estimates of the complete data model and computes the total variance over the repeated analyses by the Rubin’s rules.
}
\examples{
\donttest{
#Loading the 'dt.osa' and 'dt.osp' datasets
data(dt.osa)
data(dt.osp)

#Imputing missing data points in the'dt.osa' dataset
datasets <- mice(dt.osa, m = 5, maxit = 1,
                 method = c("", "", "mean", "", "polyreg", "logreg", "logreg"))

#Matching the imputed datasets, 'datasets'
matcheddatasets <- matchitmice(KOA ~ SEX + AGE + SMK, datasets,
                               approach = 'within', method = 'exact')

#Merging the dataframe, 'dt.osp', with each imputed dataset of the 'matcheddatasets' object
matcheddatasets <- mergeitmice(matcheddatasets, dt.osp, by = "IDN")

#Analyzing the imputed datasets
models <- with(data = matcheddatasets,
               exp = glm(KOA ~ PTH,
                         na.action = na.omit, family = binomial))

#Printing pooled results
results <- pool(models)
}
}
\references{
Stef van Buuren and Karin Groothuis-Oudshoorn (2011). \code{mice}: Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical Software}, 45(3): 1-67. \url{https://www.jstatsoft.org/v45/i03/}
}
\seealso{
\code{\link[=with]{with}}
}
\author{
Extracted from the \pkg{mice} package written by Stef van Buuren et al. with few changes
}
\keyword{function}
