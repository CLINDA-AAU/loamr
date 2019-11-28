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

LOAM <- function(data, CI = 0.95, CICI = 0.95) {

  z <- abs(qnorm((1-CI)/2))
  up <- 1-(1-CICI)/2
  lo <- (1-CICI)/2

  readers <- length(unique(data$reader))
  subjects <- length(unique(data$subject))

  da <- data %>%
    group_by(reader) %>%
      mutate(readerMean = mean(value)) %>%
      ungroup() %>%
    group_by(subject) %>%
      mutate(subjectMean = mean(value)) %>%
      ungroup() %>%
    mutate(valueMean = mean(value))


  ## Calculate S:
  J.S <- sqrt(sum((da$value - da$subjectMean - da$readerMean + da$valueMean)^2)/((readers-1)*(subjects-1)))
  J.LoAM <- c(-1, 1) * z * J.S

  B.S <- sqrt(sum((da$value - da$subjectMean)^2)/(readers*subjects))
  B.LoAM <- c(-1, 1) * z * B.S

  SSE <- sum((da$value - da$subjectMean - da$readerMean + da$valueMean)^2)
  SSA <- sum((da$readerMean - da$valueMean)^2)

  sigma2E <- SSE / ((readers - 1) * (subjects - 1))
  sigma2A <- (1 / readers) * (SSA / (subjects - 1) - SSE / ((readers - 1) * (subjects - 1)))

  SE1 <- (subjects - 1) * (sigma2E + sigma2A) / subjects
  SE2 <- sum((da$value - da$subjectMean)^2) / (readers * subjects)



  lB <- 1 - 1 / qf(up,                  (readers - 1), Inf)
  hB <- 1     / qf(lo,                  (readers - 1), Inf) - 1
  le <- 1 - 1 / qf(up, (subjects - 1) * (readers - 1), Inf)
  he <- 1     / qf(lo, (subjects - 1) * (readers - 1), Inf) - 1

  H <- sqrt(hB^2 * SSA^2 + he^2 * SSE^2)
  L <- sqrt(lB^2 * SSA^2 + le^2 * SSE^2)

  das <- data.frame(name     = c("Jones", "Borgbjerg"),
                    lower    = c(J.LoAM[1],       B.LoAM[1]),
                    llower   = c(J.LoAM[1] - SE1, (1.96 * sqrt(c(SSA + SSE + H) / (subjects * readers))*-1)),
                    ulower   = c(J.LoAM[1] + SE1, (1.96 * sqrt(c(SSA + SSE - L) / (subjects * readers))*-1)),
                    upper    = c(J.LoAM[2],       B.LoAM[2]),
                    lupper   = c(J.LoAM[2] - SE1, (1.96 * sqrt(c(SSA + SSE - L) / (subjects * readers)))),
                    uupper   = c(J.LoAM[2] + SE1, (1.96 * sqrt(c(SSA + SSE + H) / (subjects * readers)))),
                    outliers = c(round((sum(abs(da$value - da$subjectMean) > J.LoAM[2]) / (subjects * readers)) * 100,2),
                                 round((sum(abs(da$value - da$subjectMean) > B.LoAM[2]) / (subjects * readers)) * 100,2))
                    )

  parts <- data.frame(sigma2E, sigma2A, SE1, SE2, SSE, SSA, L, H, CI)

  result <- list(data = da, estimates = das, parts = parts)
  class(result) <- "loamobject"

  return(result)

}






