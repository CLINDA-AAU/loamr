#' Width of CI for LOAM (for sample size calculation)
#'
#' @description Width of the confidence intervals for the
#' 95\% limits of agreement with the mean for specified parameter values (\insertCite{christensen;textual}{loamr}).
#'
#' @details Given initial estimates of the observer, subject-observer
#' (if interaction included), and residual variance components
#' (\code{sigma2B_0}, \code{sigma2AB_0}, and \code{sigma2E_0}, respectively)
#' we can determine the width of the CI for the 95\% reproducibility LOAM for
#' specified numbers of subjects, observers, and measurements per observer per subject
#' (\code{n_subjects}, \code{n_observers}, and \code{n_measurements}, respectively)
#' (\insertCite{christensen;textual}{loamr}).
#'
#' One of \code{n_subjects}, \code{n_observers}, and \code{n_measurements} is
#' allowed to be a vector of length > 1. Usually, \code{n_subjects} and
#' \code{n_measurements} are fixed, while we want to
#' determine how many observers, \code{b}, we need to obtain a certain width of
#' the CI for the reproducibility LOAM.
#'
#' @param n_subjects number of subjects
#' @param n_observers number of observers
#' @param n_measurements number of measurements per observer per subject
#' @param sigma2B_0 initial estimate of observer variance
#' @param sigma2E_0 initial estimate of residual variance
#' @param sigma2AB_0 initial estimate of subject-observer interaction variance, only used if 'interaction = T'
#' @param interaction logical, ogical, indicates if subject-observer interaction should be included in the two-way random effects model
#' @param CI.coverage coverage probability for the confidence interval on the reproducibility LOAM.
#'
#' @references
#' \insertRef{christensen}{loamr}
#'
#' @return Width of the CI for the 95\% reproducbility LOAM for the selected parameter values.
#'
#' @examples
#' LOAM_ci_width(n_subjects = 20,  n_observers = 4:10, n_measurements = 2,
#' sigma2B_0 = 0.5, sigma2E_0 = 1)
#'
#'
#' @export
#' @importFrom stats qf


LOAM_ci_width <- function(n_subjects, n_observers, n_measurements,
                          sigma2B_0, sigma2E_0,
                          sigma2AB_0 = NULL, interaction = F,
                          CI.coverage = 0.95){

  stopifnot(length(sigma2B_0) == 1,
            length(sigma2E_0) == 1,
            n_observers > 1)

  if(interaction){
    stopifnot(n_subjects > 1, n_measurements > 1)
    if (is.null(sigma2AB_0)) stop("sigma2AB_0 must be supplied when 'interaction = T'")
  } else{
    if (!is.null(sigma2AB_0)) warning("sigma2AB_0 will not be used as 'interaction = F'")
  }

  a <- n_subjects
  b <- n_observers
  h <- n_measurements
  N <- a * b * h

  if(sum(c(length(a), length(b), length(h)) > 1) > 1){
    stop("Only one of 'n_subjects', 'n_observers', and 'n_measurements' can be a vector of length > 1")

  }

  up <- 1 - (1 - CI.coverage) / 2
  lo <-     (1 - CI.coverage) / 2
  vB <- b - 1

  if(interaction){
    vAB <- (a - 1) * (b - 1)
    lAB <- 1 - 1 / qf(up, vAB, Inf)
    hAB <- 1 / qf(lo, vAB, Inf) - 1

    vE  <-  N - a * b
  } else{
    vE <- N - a - b + 1
    stopifnot(vE > 0)
  }

  lB <- 1 - 1 / qf(up, vB, Inf)
  lE <- 1 - 1 / qf(up, vE, Inf)
  hB <- 1 / qf(lo, vB, Inf) - 1
  hE <- 1 / qf(lo, vE, Inf) - 1
  cE <- vE  * sigma2E_0

  if(interaction){
    cB  <- vB  * (a * h * sigma2B_0 + h * sigma2AB_0 + sigma2E_0)
    cAB <- vAB * (h * sigma2AB_0 + sigma2E_0)
    cT <-  cB + cAB + cE
    L0  <- sqrt(lB^2 * cB^2 + lAB^2 * cAB^2 + lE^2 * cE^2)
    H0  <- sqrt(hB^2 * cB^2 + hAB^2 * cAB^2 + hE^2 * cE^2)
  } else{
    cB <- vB * (a * h * sigma2B_0 + sigma2E_0)
    L0 <- sqrt(lB^2 * cB^2 + lE^2 * cE^2)
    H0 <- sqrt(hB^2 * cB^2 + hE^2 * cE^2)
    cT <- cB + cE
  }
  (1.96 / sqrt(N)) * (sqrt(cT + H0) - sqrt(cT - L0))
}
