#' Calculates estimate of ...
#'
#' @description This function...
#'
#' @details The function returns ...
#'
#' @param data A dataframe,
#' @param CI confidence interval on the LoAM,
#'
#'
#' @return An object of class "ccobject".
#'
#' @examples
#' data(Jones)
#'
#' Jones$subject = 1:nrow(Jones)
#' Jones <- tidyr::gather(Jones, observer, value, -subject)
#'
#' LOAM(Jones)
#'
#' @export
#' @import dplyr magrittr tibble

LOAM <- function(data, CI = 0.95) {

  if (!(tibble::has_name(data, "measurement"))) {
    data$measurement <- as.integer(1)
  }

  CI_LOAM <- 0.95
  z  <- abs(qnorm((1 - CI_LOAM) / 2))
  z2 <- abs(qnorm((1 - CI) / 2))
  up <- 1 - (1 - CI) / 2
  lo <-     (1 - CI) / 2

  b <- length(unique(data$observer))
  a <- length(unique(data$subject))
  c <- length(unique(data$measurement))
  N <- a * b * c

  vE <- a * b * c - a - b + 1
  vA <- a - 1
  vB <- b - 1

  da <- data %>%
    group_by(observer) %>%
    mutate(observerMean = mean(value)) %>%
    ungroup() %>%
    group_by(subject) %>%
    mutate(subjectMean = mean(value)) %>%
    ungroup() %>%
    mutate(valueMean = mean(value))

  SSE <- sum((da$value - da$subjectMean - da$observerMean + da$valueMean)^2)
  SSA <- sum((da$subjectMean - da$valueMean)^2)
  SSB <- sum((da$observerMean - da$valueMean)^2)

  MSE <- SSE / vE
  MSA <- SSA / vA
  MSB <- SSB / vB

  sigma2E <-  MSE
  sigma2A <- (MSA - MSE) / (b * c)
  sigma2B <- (MSB - MSE) / (a * c)

  sigmaE <- sqrt(sigma2E)
  sigmaA <- sqrt(sigma2A)
  sigmaB <- sqrt(sigma2B)

  B.LoAM <- c(-1, 1) * z * sqrt((SSB + SSE) / N)

  ICC <- sigma2A / (sigma2A + sigma2B + sigma2E)

  A         <- b * ICC / (a * (1 - ICC))
  B         <- 1 + b * ICC * (a - 1) / (a * (1 - ICC))
  v         <- (A * MSB + B * MSE)^2 / ((A * MSB)^2 / vB + (B * MSE)^2 / vE)
  FL        <- qf(up, a - 1, v)
  FU        <- qf(up, v, a - 1)
  low_num   <- a * (MSA - FL * MSE)
  low_denom <- FL * (b * MSB + (a * b - a - b) * MSE) + a * MSA
  upp_num   <- a * (FU * MSA - MSE)
  upp_denom <- b * MSB + (a * b - a - b) * MSE + a * FU * MSA

  ICC_CI <- c(low_num / low_denom, upp_num / upp_denom)

  sigmaB_CI <- sigmaB + (c(-1, 1) * ((z2/a)*sqrt((1/(2*sigmaB))*(((a*sigma2B + sigma2E)^2/vB))+(sigma2E^2/vE))))

  SE <- z2 * z * sqrt(((SSB^2 / vB) + (SSE^2 / vE)) / (2 * N * (SSB + SSE)))

  lB <- 1 - 1 / qf(up,           vB, Inf)
  hB <- 1     / qf(lo,           vB, Inf) - 1
  le <- 1 - 1 / qf(up, (a - 1) * vB, Inf)
  he <- 1     / qf(lo, (a - 1) * vB, Inf) - 1

  H <- sqrt(hB^2 * SSB^2 + he^2 * SSE^2)
  L <- sqrt(lB^2 * SSB^2 + le^2 * SSE^2)

  estimates <- data.frame(name     = c("Symmetric", "Asymmetric"),
                          upper    = c(B.LoAM[2],       B.LoAM[2]),
                          uupper   = c(B.LoAM[2] + SE, (z * sqrt((SSB + SSE + H) / N))),
                          lupper   = c(B.LoAM[2] - SE, (z * sqrt((SSB + SSE - L) / N))),
                          lower    = c(B.LoAM[1],       B.LoAM[1]),
                          llower   = c(B.LoAM[1] - SE, (-z * sqrt((SSB + SSE + H) / N))),
                          ulower   = c(B.LoAM[1] + SE, (-z * sqrt((SSB + SSE - L) / N))),

                          outliers = c(round((sum(abs(da$value - da$subjectMean) > B.LoAM[2]) / N) * 100,2),
                                       round((sum(abs(da$value - da$subjectMean) > B.LoAM[2]) / N) * 100,2)))

  parts <- data.frame(sigmaE, sigma2E, sigmaA, sigma2A, sigmaB, sigma2B,
                      SE, SSE, SSA, SSB, L, H, CI, ICC,
                      low_num, low_denom, upp_num, upp_denom)

  intervals <- data.frame(B.LoAM, ICC_CI, sigmaB_CI)

  result <- list(data      = da,
                 estimates = estimates,
                 parts     = parts,
                 intervals = intervals)

  class(result) <- "loamobject"

  return(result)

}
