#' Limits of agreement with the mean
#'
#' @description This function calculates estimates and confidence intervals for the
#' 95\% limits of agreement with the mean (LOAM) \insertCite{christensen;textual}{loamr}.
#' It provides both the reproducibility LOAM and the repeatibility LOAM
#' (when > 1 measurement per observer per subject).
##'
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
#' The function outputs estimates and CIs for the reproducibility LOAM under a
#' two-way random effects model with or without interaction. Further, estimate
#' and CIs for \eqn{\sigma_A}, \eqn{\sigma_B}, \eqn{\sigma_AB} (if interaction included)
#' and \eqn{\sigma_E} are supplied, where  \eqn{\sigma_A^2} is the subject variance,
#' \eqn{\sigma_B^2} the observer variance, ' \eqn{\sigma_AB^2} the
#' observer-sujebct interaction variance, and \eqn{\sigma_E^2} the residual variance.
#' If >1 measurement per observer per subject, estimate and CI for repeatibility
#' LOAM is supplied.
#' If only one measurement per observer per subject, estimate and CI for the
#' intra-class correlation ICC(A, 1) is supplied.
#' See \insertCite{christensen;textual}{loamr} for details.
#'
#'
#' @param data a data frame containing measurement data in long format (see 'Details')
#' @param CI.coverage coverage probability for the confidence interval on the LOAM.
#' @param interaction logical, indicates if subject-observer interaction should be included in the two-way random effects model
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


LOAM <- function(data, interaction = F, CI.coverage = 0.95) {

  if (!(tibble::has_name(data, "measurement"))) {
    data$measurement <- as.integer(1)
  }

  LOAM_perc <- 0.95
  z  <- abs(qnorm((1 - LOAM_perc) / 2))
  z2 <- abs(qnorm((1 - CI.coverage) / 2))

  up <- 1 - (1 - CI.coverage) / 2
  lo <-     (1 - CI.coverage) / 2

  a <- length(unique(data$subject))
  b <- length(unique(data$observer))
  h <- length(unique(data$measurement))
  N <- a * b * h

  vA <- a - 1
  vB <- b - 1

  if(interaction){
    if(h == 1) stop("Need >1 measurement per observer per subject when interaction = T")
    vAB <- vA * vB
    vE  <- N - a * b
  } else{
    vE <- N - a - b + 1
  }

  # Sums of squares
  da <- data %>%
    group_by(.data$observer) %>%
    mutate(observerMean = mean(.data$value)) %>%
    ungroup() %>%
    group_by(.data$subject) %>%
    mutate(subjectMean = mean(.data$value)) %>%
    ungroup() %>%
    group_by(.data$subject, .data$observer) %>%
    mutate(subjectobserverMean = mean(.data$value)) %>%
    ungroup %>%
    mutate(valueMean = mean(.data$value))

  SSA <- sum((da$subjectMean - da$valueMean)^2)
  SSB <- sum((da$observerMean - da$valueMean)^2)
  if(interaction){
    SSAB <- sum((da$subjectobserverMean - da$valueMean)^2) - SSA - SSB
    MSAB <- SSAB / vAB  #

    SSE  <- sum((da$value - da$subjectobserverMean)^2)
  } else{
    SSE <- sum((da$value - da$subjectMean - da$observerMean + da$valueMean)^2)
  }

  MSE  <- SSE / vE
  MSA  <- SSA / vA
  MSB  <- SSB / vB

  # Variance estimates
  if(interaction){
    sigma2A  <- (MSA - MSAB) / (b * h)
    sigma2B  <- (MSB - MSAB) / (a * h)

    sigma2AB <- (MSAB - MSE) / h
    sigmaAB <- ifelse(sigma2AB >= 0, sqrt(sigma2AB), NA)

  } else{
    sigma2A <- (MSA - MSE) / (b * h)
    sigma2B <- (MSB - MSE) / (a * h)
    sigmaAB <- ""
  }

  sigmaA <- ifelse(sigma2A >= 0, sqrt(sigma2A), NA)
  sigmaB <- ifelse(sigma2B >= 0, sqrt(sigma2B), NA)

  sigma2E <-  MSE
  sigmaE <- sqrt(sigma2E)

  # LOAM estimates
  if(h > 1){
    LOAM_repeat <- z * sqrt((h - 1) / h * sigma2E)
  } else{
    LOAM_repeat <- ""
  }

  if(interaction){
    LOAM_reprod <- z * sqrt((SSB + SSAB + SSE) / N)
  } else{
    LOAM_reprod <- z * sqrt((SSB + SSE) / N)
  }

  # Reproducibility LOAM CI
  lB <- 1 - 1 / qf(up, vB, Inf)
  hB <- 1     / qf(lo, vB, Inf) - 1
  lE <- 1 - 1 / qf(up, vE, Inf)
  hE <- 1     / qf(lo, vE, Inf) - 1

  if(interaction){
    lAB <- 1 - 1 / qf(up, vAB, Inf)
    hAB <- 1     / qf(lo, vAB, Inf) - 1
    H <- sqrt(hB^2 * SSB^2 + hAB^2 * SSAB^2 + hE^2 * SSE^2)
    L <- sqrt(lB^2 * SSB^2 + lAB^2 * SSAB^2 + lE^2 * SSE^2)

    reprod_CI <- c(z * sqrt((SSB + SSAB + SSE - L) / N),
                   z * sqrt((SSB + SSAB + SSE + H) / N))

  } else{
    H <- sqrt(hB^2 * SSB^2 + hE^2 * SSE^2)
    L <- sqrt(lB^2 * SSB^2 + lE^2 * SSE^2)

    reprod_CI <- c(z * sqrt((SSB + SSE - L) / N),
                   z * sqrt((SSB + SSE + H) / N))
  }

  # Repeatibility LOAM CI
  if(h > 1){
    repeat_CI <- c(z * sqrt( (h - 1) * SSE / (h * qchisq(up, vE))),
                   z * sqrt( (h - 1) * SSE / (h * qchisq(lo, vE))))
  } else{
    repeat_CI <- NA
  }

  # CI for variance components
  if(interaction){
    var1 <- h * sigma2AB + sigma2E
    v <- vAB
  } else{
    var1 <- sigma2E
    v <- vE
  }

  if(sigma2A >= 0){
    sigmaA_CI <- sigmaA + c(-1, 1) * z2 / (b * h * sigmaA) * sqrt((b * h * sigma2A + var1)^2 / (2 * vA) + var1^2 / (2 * v))
  } else {
    sigmaA_CI <- NA
    warning("Estimate of sigma2A < 0.")
  }

  if(sigma2B >= 0){
    sigmaB_CI <- sigmaB + c(-1, 1) * z2 / (a * h * sigmaB) * sqrt((a * h * sigma2B + var1)^2 / (2 * vB) + var1^2 / (2 * v))
  } else {
    sigmaB_CI <- NA
    warning("Estimate of sigma2B < 0.")
  }

  if(interaction){
    if(sigma2AB >= 0){
      sigmaAB_CI <- sigmaAB + c(-1, 1) * z2 / (h * sigmaAB) * sqrt(var1^2 / (2 * vAB) + (sigma2E ^ 2 / (2 * vE)))
    } else {
      sigmaAB_CI <- NA
      warning("Estimate of sigma2AB < 0.")
    }
  } else{
    sigmaAB_CI <- NA
  }

  sigmaE_CI <- c(sigmaE * sqrt(vE / qchisq(up, vE)),
                 sigmaE * sqrt(vE / qchisq(lo, vE)))


  # Intra-class correlation coefficient estimate and CI
  if (h == 1 & sigma2A >= 0) {
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
    ICC       <- ""
    ICC_CI    <- ""
  }

  result <- list(data        = da,
                 estimates   = data.frame(LOAM_reprod, LOAM_repeat, sigmaA,
                                          sigmaB, sigmaAB, sigmaE, ICC),
                 intervals   = data.frame(reprod_CI,  repeat_CI,
                                          sigmaA_CI,  sigmaB_CI,
                                          sigmaAB_CI, sigmaE_CI,
                                          ICC_CI),
                 CI.coverage = CI.coverage)

  class(result) <- "loamobject"
  return(result)

}