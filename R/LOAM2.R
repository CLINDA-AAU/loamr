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

LOAM2 <- function(data, CI = 0.95, CICI = 0.95) {

  z  <- abs(qnorm((1 - CI) / 2))
  up <- 1 - (1 - CICI) / 2
  lo <-     (1 - CICI) / 2

  b <- length(unique(data$observer))
  a <- length(unique(data$subject))
  N <- a * b

  vE <- (a - 1) * (b - 1)
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
  SSA <- b * sum((da$observerMean - da$valueMean))
  SSB <- a * sum((da$subjectMean - da$valueMean))

  MSE <- SSE / vE
  MSA <- SSA / vA
  MSB <- SSB / vB

  sigma2E <-  MSE
  sigma2A <- (MSA - MSE) / b
  sigma2B <- (MSB - MSE) / a

  J.LoAM <- c(-1, 1) * z * sqrt(SSE / vE)
  B.LoAM <- c(-1, 1) * z * sqrt((SSB + SSE) / N)

  ICC <- sigma2A / (sigma2A + sigma2B + sigma2E)

  A         <- b * ICC / (a * (1 - ICC))
  B         <- 1 + b * ICC * (a - 1) / (a * (1 - ICC))
  v         <- (A * MSB + B * MSE)^2 / ((A * MSB)^2 / vB + (B * MSE)^2 / vE)
  FL        <- qf(lo, a - 1, v)
  FU        <- qf(up, v, a - 1)
  low_num   <- a * (MSA - FL * MSE)
  low_denom <- FL * (b * MSB + (a * b - a - b) * MSE) + a * MSA
  upp_num   <- a * (FU * MSA - MSE)
  upp_denom <- b * MSB + (a * b - a - b) * MSE + a * FU * MSA

  ICC_CI <- c(low_num / low_denom, upp_num / upp_denom)

  SE1 <- (a - 1) * (sigma2E + sigma2A) / a
  SE2 <- sum((da$value - da$subjectMean)^2) / N
  SE3 <- z^2 * sqrt(((SSB^2 / vB) + (SSE^2 / vE)) / (2 * N * (SSB + SSE)))

  lB <- 1 - 1 / qf(up,           vB, Inf)
  hB <- 1     / qf(lo,           vB, Inf) - 1
  le <- 1 - 1 / qf(up, (a - 1) * vB, Inf)
  he <- 1     / qf(lo, (a - 1) * vB, Inf) - 1

  H <- sqrt(hB^2 * SSB^2 + he^2 * SSE^2)
  L <- sqrt(lB^2 * SSB^2 + le^2 * SSE^2)

  SE4u <- (z * sqrt((SSB + SSE + H) / N)) - J.LoAM[2]
  SE4l <- (z * sqrt((SSB + SSE - L) / N)) - J.LoAM[2]

  estimates <- data.frame(name     = c("Jones", "Borgbjerg"),
                          upper    = c(J.LoAM[2],       B.LoAM[2]),
                          uupper   = c(J.LoAM[2] + SE1, (z * sqrt((SSB + SSE + H) / N))),
                          lupper   = c(J.LoAM[2] - SE1, (z * sqrt((SSB + SSE - L) / N))),
                          lower    = c(J.LoAM[1],       B.LoAM[1]),
                          llower   = c(J.LoAM[1] - SE1, (-z * sqrt((SSB + SSE + H) / N))),
                          ulower   = c(J.LoAM[1] + SE1, (-z * sqrt((SSB + SSE - L) / N))),

                          outliers = c(round((sum(abs(da$value - da$subjectMean) > J.LoAM[2]) / N) * 100,2),
                                       round((sum(abs(da$value - da$subjectMean) > B.LoAM[2]) / N) * 100,2)))

  parts <- data.frame(sigma2E, sigma2A, sigma2B, SE1, SE2, SE3, SE4u, SE4l, SSE, SSA, SSB,
                      L, H, CI, CICI, ICC, low_num, low_denom, upp_num, upp_denom)

  intervals <- data.frame(J.LoAM, B.LoAM, ICC_CI)

  result <- list(data      = da,
                 estimates = estimates,
                 parts     = parts,
                 intervals = intervals)

  class(result) <- "loamobject"

  return(result)

}

LOAM2(ITI)$intervals
LOAM2(ITI)$parts
LOAM2(ITI)$estimates
plot(LOAM2(OTO), CI="sym")
plot(LOAM2(OTO), CI="asym")


LOAM2(simMD())$intervals
LOAM2(simMD())$parts
LOAM2(simMD())$estimates
plot(LOAM2(simMD()), CI="asym")
