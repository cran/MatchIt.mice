#' @title Demographic Data of 4,796 Participants in the OAI Project
#'
#' @rdname dt.osa
#'
#' @description The dataset \code{dt.osa} includes demographic data of 4,796 individuals with or at risk of knee osteoarthritis. The recorded data has missing values in body mass index (\code{BMI}, a quantitative variable), race (\code{RAC}, a categorical qualitative variable), radiographic knee osteoarthritis (\code{KOA}, a binary qualitative variable) and smoking status (\code{SMOKING}, a binary qualitative variable).
#'
#' @format This dataset contains 4,796 rows and 7 columns. Each row presents data of an observation (individual) and each column presents data of a characteristics of that observation. The columns are:
#' \describe{
#'   \item{IDN}{Unique identifier number for each observation;}
#'   \item{AGE}{Age of each observation;}
#'   \item{BMI}{Estimated body mass index of each observation;}
#'   \item{SEX}{Gender of each observation, coded as \code{0} (female) and \code{1} (male);}
#'   \item{RAC}{Race of rach observation, coded as \code{0} (other), \code{1} (white), \code{2} (black), and \code{3} (asian);}
#'   \item{KOA}{Knee osteoarthritis status of each observation, coded as \code{0} (at risk) and \code{1} (diagnosed); and}
#'   \item{SMK}{The smoking status of each observation, coded as \code{0} (non-smoker) and \code{1} (smoker).}
#' }
#' @source The information presented in the \code{dt.osa} dataset is based on the publicly available data of the Osteoarthritis Initiative (OAI) project (see \url{https://nda.nih.gov/oai/} for details), with changes.

"dt.osa"
