% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dt.osp.R
\docType{data}
\name{dt.osp}
\alias{dt.osp}
\title{Clinical Data of 4,796 Participants in the OAI Project}
\format{This dataset contains 4,796 rows and 3 columns. Each row presents data of an observation (individual) and each column presents data of a characteristics of that observation. The columns are:
\describe{
  \item{IDN}{Unique identifier number for each observation;}
  \item{PTH}{History of Bisphosphonate intake, coded as \code{0} (negative) and \code{1} (positive); and}
  \item{BSP}{History of Bisphosphonate intake, coded as \code{0} (negative) and \code{1} (positive).}
}}
\source{
The information presented in the \code{dt.osa} dataset is based on the publicly available data of the Osteoarthritis Initiative (OAI) project (see \url{https://nda.nih.gov/oai/} for details), with changes.
}
\usage{
dt.osp
}
\description{
The dataset \code{dt.osa} includes clinical data of 4,796 individuals with or at risk of knee osteoarthritis. The recorded data has missing values in history of PTH intake (\code{PTH}, a binary qualitative variable) and history of Bisphosphanate intake (\code{BSP}, a binary qualitative variable).
}
\keyword{datasets}
