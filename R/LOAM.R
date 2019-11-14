#' Calculates estimates of ...
#'
#' @description This function...
#'
#' @details The function returns ...
#'
#' @param data A dataframe,
#'
#' @return An object of class "ccobject".
#'
#' @examples
#' data(Jones)
#' LOAM(Jones)
#'
#'
#' @export
#' @import dplyr tidyr magrittr

LOAM <- function(data) {

  m <- length(unique(data$reader))
  n <- length(unique(data$subject))

  da <- data %>%
    group_by(reader) %>%
      mutate(readerMean = mean(value)) %>%
      ungroup() %>%
    group_by(subject) %>%
      mutate(subjectMean = mean(value)) %>%
      ungroup() %>%
    mutate(valueMean = mean(value))


  ## Calculate S:
  J.S <- sqrt(sum((da$value - da$subjectMean - da$readerMean + da$valueMean)^2)/((m-1)*(n-1)))
  J.LoAM <- c(-1, 1) * 1.96 * J.S

  B.S <- sqrt(sum((da$value - da$subjectMean)^2)/(m*n))
  B.LoAM <- c(-1, 1) * 1.96 * B.S

  SSE <- sum((da$value - da$subjectMean - da$readerMean + da$valueMean)^2)
  SSA <- sum((da$readerMean - da$valueMean)^2)

  sigma2e <- SSE/((m-1)*(n-1))
  sigma2A <- (1/m) * (SSA/(n-1) - SSE/((m-1)*(n-1)))

  SE1 <- (n-1)*(sigma2e + sigma2A)/n
  SE2 <- sum((da$value - da$subjectMean)^2)/(m*n)

  result <- list(da, J.LoAM, B.LoAM, SE1, SE2)
  class(result) <- "loamobject"

  return(result)

}


















