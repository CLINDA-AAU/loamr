#' Aortic diameter data
#'
#' Data frame containing measurements of aortic diameters obtained by the ITI method, see \insertCite{borgbjerg;textual}{loamr}.
#' For 50 aortas the diameter have been measured by 18 observers, giving a total of 900 observations.
#'
#' @docType data
#' @usage data(Borgbjerg)
#' @format A data frame with 900 observations and 3 variables:
#' \describe{
#'   \item{subject}{aorta ID}
#'   \item{observer}{observer ID}
#'   \item{value}{aorta diameter, in mm}
#' }
#' @keywords datasets
#' @references
#' \insertRef{borgbjerg}{loamr}
#'
#' @examples
#' data(Borgbjerg)
#'
#' @importFrom Rdpack reprompt
"Borgbjerg"


#' Aortic diameter data with multiple measurements per observer
#'
#' Data frame containing measurements of aortic diameters obtained by the ITI method, see \insertCite{borgbjerg;textual}{loamr}.
#' For 50 aortas the diameter have been measured twice by 12 observers, giving a total of 1200 observations.
#'
#' @docType data
#' @usage data(Borgbjerg_m)
#' @format A data frame with 1200 observations and 4 variables:
#' \describe{
#'   \item{subject}{aorta ID}
#'   \item{observer}{observer ID}
#'   \item{value}{aorta diameter, in mm}
#'   \item{measurement}{repetition ID, 1/2 = first/second measurement made by the specific observer on the specific aorta}
#' }
#' @keywords datasets
#' @references
#' \insertRef{borgbjerg}{loamr}
#'
#' @examples
#' data(Borgbjerg_m)
#'
#' @importFrom Rdpack reprompt
"Borgbjerg_m"