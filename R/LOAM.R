#' Function LOAM 13-01-20
#'
#' @description This function calculates an estimate and confidence interval for the
#' 95\% limits of agreement with the mean presented by \insertCite{christensen;textual}{loamr}.
#'
#' @details The data argument requires data in long/narrow format with
#' the following columns:
#'
#' - subject: a unique id for each subject
#'
#' - observer: a unique id of the observer/reader
#'
#' - value: value of the measurement
#'
#' - measurement: a unique id 1,..., k indicating the measurement number, when each observer
#' has performed k measurements on each subject. If only one measurement per observer per subject,
#' this column is not required.
#'
#'
#' The procedure requires balanced data, meaning that all observers must have measured
#' all subjects the same number of times.
#'
#' The function outputs both an approximate asymmetric and symmetric CI for the LOAM.
#' Note that the asymmetric CI is prefered, especially when the number of observers is small/moderate
#' as discussed in \insertCite{christensen;textual}{loamr}.
#' Further, the function outputs estimates and CIs related to the variance components for the
#' underlying random model (see \insertCite{christensen;textual}{loamr}) and to the intraclass correlation coeffecient.
#'
#'
#' @param data a data frame containing measurement data in long format (see 'Details')
#' @param CI coverage probability for the confidence interval on the LOAM.
#' @param observer_effect If set to false removes the observer effect, see \insertCite{christensen;textual}{loamr}
#'
#' @references
#' \insertRef{christensen}{loamr}
#'
#' @return An object of class "loamobject".
#'
#' @examples
#' data(Borgbjerg)
#' L <- LOAM(Borgbjerg)
#' L
#'
#' # Point estimates of LOAM, sigmaA, sigmaB, sigmaE, and ICC(A, 1):
#' L$estimates
#'
#' # CIs for the LOAM, sigmaB, sigmaE, and ICC(A, 1):
#' L$intervals
#'
#' @export
#' @import dplyr magrittr tibble
#' @importFrom stats qnorm qf
#' @importFrom rlang .data


LOAM <- function(data, CI = 0.95, observer_effect = T) {

  if (!(tibble::has_name(data, "measurement"))) {
    data$measurement <- as.integer(1)
  }

  LOAM_perc <- 0.95
  z  <- abs(qnorm((1 - LOAM_perc) / 2))
  z2 <- abs(qnorm((1 - CI) / 2))
  up <- 1 - (1 - CI) / 2
  lo <-     (1 - CI) / 2

  a <- length(unique(data$subject))
  b <- length(unique(data$observer))
  c <- length(unique(data$measurement))
  N <- a * b * c

  vE <- a * b * c - a - b + 1
  vA <- a - 1
  vB <- b - 1
  vW <- (a * ((b * c) - 1))

  da <- data %>%
    group_by(.data$observer) %>%
    mutate(observerMean = mean(.data$value)) %>%
    ungroup() %>%
    group_by(.data$subject) %>%
    mutate(subjectMean = mean(.data$value)) %>%
    ungroup() %>%
    mutate(valueMean = mean(.data$value))

  SSE <- sum((da$value - da$subjectMean - da$observerMean + da$valueMean)^2)
  SSW <- sum((da$value - da$subjectMean)^2)
  SSA <- sum((da$subjectMean - da$valueMean)^2)
  SSB <- sum((da$observerMean - da$valueMean)^2)

  MSE <- SSE / vE
  MSA <- SSA / vA
  MSB <- SSB / vB
  MSW <- SSW / vW

  sigma2E <-  MSE
  sigma2W <-  MSW
  sigma2A <- (MSA - MSE) / (b * c)
  sigma2B <- (MSB - MSE) / (a * c)

  sigmaE <- sqrt(sigma2E)
  sigmaW <- sqrt(sigma2W)
  sigmaA <- sqrt(sigma2A)
  sigmaB <- ifelse(sigma2B >= 0, sqrt(sigma2B), NA)

  if (observer_effect == T) {
    LOAM <- z * sqrt((SSB + SSE) / N)


  if (c == 1 & sigma2A >= 0) {
    ICC       <- sigma2A / (sigma2A + sigma2B + sigma2E)
    A         <- b * ICC / (a * (1 - ICC))
    B         <- 1 + b * ICC * (a - 1) / (a * (1 - ICC))
    v         <- (A * MSB + B * MSE)^2 / ((A * MSB)^2 / vB + (B * MSE)^2 / vE)
    FL        <- qf(up, a - 1, v)
    FU        <- qf(up, v, a - 1)
    low_num   <- a * (MSA - FL * MSE)
    low_denom <- FL * (b * MSB + (a * b - a - b) * MSE) + a * MSA
    upp_num   <- a * (FU * MSA - MSE)
    upp_denom <- b * MSB + (a * b - a - b) * MSE + a * FU * MSA
    ICC_CI    <- c(low_num / low_denom, upp_num / upp_denom)
  } else {
    ICC       <- NA
    ICC_CI    <- NA
  }

  if(sigma2B >= 0){
    sigmaB_CI <- sigmaB + (c(-1, 1) * ((z2 / a) * sqrt((1 / (2 * sigma2B)) * (((a * sigma2B + sigma2E)^2 / vB)) + (sigma2E^2 / vE))))
  } else {
    sigmaB_CI <- NA
    warning("Estimate of sigma2B < 0.")
  }

  sigmaE_CI <- sigmaE + c(-1, 1) * z2 * sigmaE * sqrt(1 / (2 * vE))

  SE <- z2 * z * sqrt(((SSB^2 / vB) + (SSE^2 / vE)) / (2 * N * (SSB + SSE)))

  lB <- 1 - 1 / qf(up, vB, Inf)
  hB <- 1     / qf(lo, vB, Inf) - 1
  le <- 1 - 1 / qf(up, vE, Inf)
  he <- 1     / qf(lo, vE, Inf) - 1

  H <- sqrt(hB^2 * SSB^2 + he^2 * SSE^2)
  L <- sqrt(lB^2 * SSB^2 + le^2 * SSE^2)

  LOAM_CI_sym  <- c(LOAM - SE,
                    LOAM + SE)

  LOAM_CI_asym <- c(z * sqrt((SSB + SSE - L) / N),
                    z * sqrt((SSB + SSE + H) / N))

  result <- list(data      = da,
                 estimates = data.frame(LOAM, sigmaA, sigmaB, sigmaE, ICC),
                 intervals = data.frame(LOAM_CI_sym, LOAM_CI_asym, sigmaB_CI, sigmaE_CI,  ICC_CI),
                 CI        = CI,
                 OE        = observer_effect)


  } else {
    LOAM <- z * sqrt(SSW / N)

    if (c == 1 & sigma2A >= 0) {
      ICC       <- (MSA - MSW) / (MSA + ((b - 1) * MSW))
      Fobs      <- MSA / MSW
      FL        <- Fobs / qf(up, a - 1, a * (b - 1))
      FU        <- Fobs * qf(up, a * (b - 1), a - 1)
      ICC_CI    <- c((FL-1)/ (FL+(b-1)), (FU-1) / (FU+(b-1)))
    } else {
      ICC       <- NA
      ICC_CI    <- NA
    }

    sigmaB    <- NA
    sigmaB_CI <- NA
    sigmaE_CI <- NA


    SE <- z2 * z * sqrt(sigma2E / (2 * a * b * c))

    LOAM_CI_sym  <- c(LOAM - SE,
                      LOAM + SE)

    LOAM_CI_asym <- c(z * sqrt((((b * c) - 1) * SSW) / (b * c * qchisq(lo, df = vW))),
                      z * sqrt((((b * c) - 1) * SSW) / (b * c * qchisq(up, df = vW))))





  result <- list(data      = da,
                 estimates = data.frame(LOAM, sigmaA, sigmaW, ICC),
                 intervals = data.frame(LOAM_CI_sym, LOAM_CI_asym,  ICC_CI),
                 CI        = CI,
                 OE        = observer_effect)

  }

  class(result) <- "loamobject"

  return(result)

}
