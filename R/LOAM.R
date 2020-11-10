#' Function LOAM 13-01-20
#'
#' @description This function calculates estimates and confidence intervals for the
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
#' - measurement: an id indicating the measurement number if each observer
#' has performed multiple measurements on each subject. If only one measurement
#' per observer per subject, this column is not required.
#'
#'
#' The procedure requires balanced data, meaning that all observers must have measured
#' all subjects the same number of times.
#'
#' The function outputs estimates and approximate CIs for the LOAM, the
#' intra-class correlation, \eqn{\sigma_A}, \eqn{\sigma_B}, and \eqn{\sigma_E}, where
#' \eqn{\sigma_A^2}, \eqn{\sigma_B^2}, and \eqn{\sigma_E^2} are the variance components of the underlying
#' two-way random model.  See \insertCite{christensen;textual}{loamr} for details.
#'
#'
#' @param data a data frame containing measurement data in long format (see 'Details')
#' @param CI coverage probability for the confidence interval on the LOAM.
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
#' # CIs for the LOAM, sigmaA, sigmaB, sigmaE, and ICC(A, 1):
#' L$intervals
#'
#' @export
#' @import dplyr magrittr tibble
#' @importFrom stats qnorm qf qchisq
#' @importFrom rlang .data


LOAM <- function(data, CI = 0.95) {

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

  da <- data %>%
    group_by(.data$observer) %>%
    mutate(observerMean = mean(.data$value)) %>%
    ungroup() %>%
    group_by(.data$subject) %>%
    mutate(subjectMean = mean(.data$value)) %>%
    ungroup() %>%
    mutate(valueMean = mean(.data$value))

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
  sigmaA <- ifelse(sigma2A >= 0, sqrt(sigma2A), NA)
  sigmaB <- ifelse(sigma2B >= 0, sqrt(sigma2B), NA)

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

  if(sigma2A >= 0){
    sigmaA_CI <- sigmaA + (c(-1, 1) * ((z2 / b) * sqrt((1 / (2 * sigma2A)) * (((b * sigma2A + sigma2E)^2 / vA)) + (sigma2E^2 / vE))))
  } else {
    sigmaA_CI <- NA
    warning("Estimate of sigma2A < 0.")
  }

  if(sigma2B >= 0){
    sigmaB_CI <- sigmaB + (c(-1, 1) * ((z2 / a) * sqrt((1 / (2 * sigma2B)) * (((a * sigma2B + sigma2E)^2 / vB)) + (sigma2E^2 / vE))))
  } else {
    sigmaB_CI <- NA
    warning("Estimate of sigma2B < 0.")
  }

  sigmaE_CI <- c(sigmaE * sqrt(vE / qchisq(up, vE)),
                 sigmaE * sqrt(vE / qchisq(lo, vE)))


  lB <- 1 - 1 / qf(up, vB, Inf)
  hB <- 1     / qf(lo, vB, Inf) - 1
  lE <- 1 - 1 / qf(up, vE, Inf)
  hE <- 1     / qf(lo, vE, Inf) - 1

  H <- sqrt(hB^2 * SSB^2 + hE^2 * SSE^2)
  L <- sqrt(lB^2 * SSB^2 + lE^2 * SSE^2)

  LOAM_CI <- c(z * sqrt((SSB + SSE - L) / N),
               z * sqrt((SSB + SSE + H) / N))

  result <- list(data      = da,
                 estimates = data.frame(LOAM, sigmaA, sigmaB, sigmaE, ICC),
                 intervals = data.frame(LOAM_CI, sigmaA_CI,
                                        sigmaB_CI, sigmaE_CI,  ICC_CI),
                 CI        = CI)

  class(result) <- "loamobject"

  return(result)

}
