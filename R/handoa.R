#' @title Data of 4,704 Individuals (hands) with or without Hand Osteoarthritis
#'
#' @rdname handoa
#'
#' @description The dataset \code{handoa} contains data of 4,704 individuals (hands) with or without hand osteoarthritis. The recorded data has missing values in body mass index (\code{BMI}, a quantitative variable), hand use habits (\code{HANDUSE}, a binary qualitative variable), and smoking status (\code{SMOKING}, a categorical qualitative variable).
#'
#' @format A dataset with 4,704 rows and 8 columns. Each row presents data of a single individual (hand) and columns present data of several characteristics of that individual. The columns are:
#' \describe{
#'   \item{ID}{Unique identifier number of each individual;}
#'   \item{AGE}{Age of each individual;}
#'   \item{SEX}{Gender of each individual, coded as \code{0} (female) and \code{1} (male);}
#'   \item{BMI}{Estimated body mass index of each individual;}
#'   \item{SMOKING}{The smoking status of each individual, coded as \code{0} (never smoked), \code{1} (current smoker), and \code{2} (former smoker);}
#'   \item{HANDUSE}{The hand use habits of each individual, coded as \code{0} (not lifting heavy objects regularly) and \code{1} (lifting heavy objects regularly);}
#'   \item{KNEEOA}{Diagnosis of knee osteoarthritis, coded as \code{0} (not affected) and \code{1} (affected); and}
#'   \item{HANDOA}{Diagnosis of hand osteoarthritis, coded as \code{0} (not affected) and \code{1} (affected).}
#'   ...
#'
#' }
#' @source The information presented in the \code{handoa} dataset is based on the publicly available data of the Osteoarthritis Initiative (OAI) project (see \url{https://nda.nih.gov/oai/} for details), with changes.

"handoa"
