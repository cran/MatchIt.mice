#' Data of 4,704 individuals with or without hand osteoarthritis
#'
#' The dataset `HandOsteoarthritis` contains data of 4,704 individuals with or without hand osteoarthritis. The recorded data has missing values in body mass index (BMI, a quantitative variable), hand use habits (a binary qualitative variable), and smoking status (a categorical qualitative variable).
#'
#' @format A dataset with 4,704 rows and 8 columns:
#' \describe{
#'   \item{ID}{Unique identifier variable for each individual}
#'   \item{SIDE}{The examined hand, coded as 1 (left) and 2 (right)}
#'   \item{AGE}{Age of each individual}
#'   \item{SEX}{Gender of each individual, coded as 0 (female) and 1 (male)}
#'   \item{BMI}{Estimated body mass index of each individual}
#'   \item{SMOKING}{The smoking status of each individual, coded as 0 (never smoked), 1 (current smoker), and 2 (former smoker)}
#'   \item{HANDUSE}{The hand use habits of each individual, coded as 0 (not lifting heavy objects regularly) and 1 (lifting heavy objects regularly)}
#'   \item{HANDOA}{Diagnosis of hand osteoarthritis, coded as 0 (not affected) and 1 (affected)}
#'   ...
#'
#' }
#' @source \url{https://nda.nih.gov/oai/}
"HandOsteoarthritis"
