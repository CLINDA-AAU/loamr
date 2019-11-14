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
#' Jones
#' LOAM(Jones)
#'
#'
#' @export
#' @import dplyr tidyr

LOAM <- function(data) {

  m <- ncol(data)
  n <- nrow(data)

  Jones <- data %>%
    mutate(lesion = 1:nrow(.)) %>%
    gather(reader, value, -lesion) %>%
    group_by(reader) %>%
    mutate(readerMean = mean(value)) %>%
    ungroup() %>%
    group_by(lesion) %>%
    mutate(lesionMean = mean(value)) %>%
    ungroup() %>%
    mutate(valueMean = mean(value))


  ## Calculate S:
  J.S <- sqrt(sum((Jones$value - Jones$lesionMean - Jones$readerMean + Jones$valueMean)^2)/((m-1)*(n-1)))
  J.LoAM <- c(-1, 1) * 1.96 * J.S

  B.S <- sqrt(sum((Jones$value - Jones$lesionMean)^2)/(m*n))
  B.LoAM <- c(-1, 1) * 1.96 * B.S

  SSE <- sum((Jones$value - Jones$lesionMean - Jones$readerMean + Jones$valueMean)^2)
  SSA <- sum((Jones$readerMean - Jones$valueMean)^2)

  sigma2e <- SSE/((m-1)*(n-1))
  sigma2A <- (1/m) * (SSA/(n-1) - SSE/((m-1)*(n-1)))

  SE1 <- (n-1)*(sigma2e + sigma2A)/n
  SE2 <- sum((Jones$value - Jones$lesionMean)^2)/(m*n)

  result <- list(Jones, J.LoAM, B.LoAM, SE1, SE2)
  class(result) <- "loamobject"

  return(result)

}
