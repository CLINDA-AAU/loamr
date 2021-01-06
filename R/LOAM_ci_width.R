#' Width of CI for LOAM (for sample size calculation)
#'
#' @description Width of the confidence intervals for the
#' 95\% limits of agreement with the mean for specified parameter values (\insertCite{christensen;textual}{loamr}).
#'
#' @details Given initial estimates of the observer and residual variance
#' components (\code{sigma2B_0} and \code{sigma2E_0}, respectively) we can determine the width
#' of the CI for the 95\% LOAM for specified numbers of subjects, observers, and
#' measurements per observer per subject (\code{a}, \code{b}, and \code{c}, respectively)
#' (\insertCite{christensen;textual}{loamr}).
#'
#' One of \code{a}, \code{b}, and \code{c} is allowed to be a vector of length
#' greater than one. Usually, \code{a} and \code{c} are fixed, while we want to
#' determine how many observers, \code{b}, we need to obtain a certain width of
#' the CI for the LOAM.
#'
#' @param a number of subjects
#' @param b number of observers
#' @param c number of measurements per observer per subject
#' @param sigma2B_0 observer variance
#' @param sigma2E_0 residual variance
#' @param CI coverage probability for the confidence interval on the LOAM.
#'
#' @references
#' \insertRef{christensen}{loamr}
#'
#' @return Width of the CI for the 95\% LOAM for the selected parameter values.
#'
#' @examples
#' LOAM_ci_width(a = 20, b = 4:10, c = 2, sigma2B_0 = 0.5, sigma2E_0 = 1)
#'
#'
#' @export
#' @importFrom stats qf


LOAM_ci_width <- function(a, b, c, sigma2B_0, sigma2E_0, CI = 0.95){
  stopifnot(length(sigma2B_0) == 1,
            length(sigma2E_0) == 1,
            b > 1)
  if(sum(c(length(a), length(b), length(c)) > 1) > 1) stop("Only one of 'a', 'b', and 'c' can be a vector of length > 1")
  up <- 1 - (1 - CI) / 2
  lo <-     (1 - CI) / 2
  N <- a * b * c
  vB <- b - 1
  vE <- a * b * c - a - b + 1
  stopifnot(vE > 0)
  lB <- 1 - 1 / qf(up, vB, Inf)
  lE <- 1 - 1 / qf(up, vE, Inf)
  hB <- 1 / qf(lo, vB, Inf) - 1
  hE <- 1 / qf(lo, vE, Inf) - 1
  cB <- vB * (a * c * sigma2B_0 + sigma2E_0)
  cE <- vE * sigma2E_0
  L0 <- sqrt(lB^2 * cB^2 + lE^2 * cE^2)
  H0 <- sqrt(hB^2 * cB^2 + hE^2 * cE^2)
  (1.96 / sqrt(N)) * (sqrt(cB + cE + H0) - sqrt(cB + cE -L0))
}
