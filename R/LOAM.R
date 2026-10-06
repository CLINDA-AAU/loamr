#' Limits of agreement with the mean
#'
#' @description This function calculates estimates and confidence intervals
#' for the 95\% limits of agreement with the mean (LOAM). Reproducibility
#' LOAM is computed under a two-way random-effects model, either without
#' subject-observer interaction as described in
#' \insertCite{christensen;textual}{loamr}, or with subject-observer
#' interaction as described in \insertCite{christensen2025;textual}{loamr}.
#' When more than one measurement per observer per subject is available,
#' the function additionally provides the repeatability LOAM introduced in
#' \insertCite{christensen2025;textual}{loamr}
#'
#'
#' @details The input data must be in long format with the following columns:
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
#' The procedure requires balanced data, meaning that all observers must have
#' measured all subjects the same number of times.
#'
#' The function outputs estimates and CIs for the reproducibility LOAM under a
#' two-way random effects model with or without interaction. Estimates and
#' confidence intervals are also provided for the standard deviation components
#' sigma_A, sigma_B, sigma_AB (when interaction = TRUE), and sigma_E,
#' corresponding to the subject, observer, interaction, and residual variance
#' components.
#'
#' If more than one measurement per observer per subject is available, estimate
#' and CI for the repeatability LOAM are also provided.
#'
#' If only one measurement per observer per subject is available, estimate and
#' CI for the intra-class correlation ICC(A, 1) are also supplied.
#'
#' See \insertCite{christensen;textual}{loamr} and
#' \insertCite{christensen2025}{loamr} for details.
#'
#'
#' @param data a data frame containing measurement data in long format (see 'Details')
#' @param CI.coverage coverage probability for the confidence interval on the LOAM.
#' @param interaction logical, indicates if subject-observer interaction should be included in the two-way random effects model
#' @param residual.plot logical, indicates if a QQ-plot of the residuals should be produced (for checking normality)
#'
#' @references
#' \insertRef{christensen}{loamr}
#' \insertRef{christensen2025}{loamr}
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
#' @import dplyr magrittr tibble ggplot2
#' @importFrom stats qnorm qf qchisq residuals
#' @importFrom rlang .data


LOAM <- function(data, interaction = F, CI.coverage = 0.95, residual.plot = F) {

  LOAM_perc <- 0.95
  z  <- abs(qnorm((1 - LOAM_perc) / 2))
  z2 <- abs(qnorm((1 - CI.coverage) / 2))

  up <- 1 - (1 - CI.coverage) / 2
  lo <-     (1 - CI.coverage) / 2

  # ANOVA sums of squares, degrees of freedom, mean squares, and LOAM point
  # estimates: computed by .loam_components() (see LOAM_utils.R), which
  # is also used internally by LOAM_diff_boot() when comparing two methods
  comp <- .loam_components(data, interaction = interaction)

  da <- comp$data
  a <- comp$a; b <- comp$b; h <- comp$h; N <- comp$N
  vA <- comp$vA; vB <- comp$vB; vAB <- comp$vAB; vE <- comp$vE
  SSA <- comp$SSA; SSB <- comp$SSB; SSAB <- comp$SSAB; SSE <- comp$SSE
  MSA <- comp$MSA; MSB <- comp$MSB; MSAB <- comp$MSAB; MSE <- comp$MSE

  # Variance estimates
  if(interaction){
    sigma2A  <- (MSA - MSAB) / (b * h)
    sigma2B  <- (MSB - MSAB) / (a * h)

    sigma2AB <- (MSAB - MSE) / h
    sigmaAB <- ifelse(sigma2AB >= 0, sqrt(sigma2AB), NA)

  } else{
    sigma2A <- (MSA - MSE) / (b * h)
    sigma2B <- (MSB - MSE) / (a * h)
    sigmaAB <- NULL
  }

  sigmaA <- ifelse(sigma2A >= 0, sqrt(sigma2A), NA)
  sigmaB <- ifelse(sigma2B >= 0, sqrt(sigma2B), NA)

  sigma2E <-  MSE
  sigmaE <- sqrt(sigma2E)

  # LOAM estimates
  # NB: repeatability (Var(Y_ijk - Y_ij.)) only needs h > 1 replicate
  # measurements per cell - cell-mean centering removes any effect constant
  # within a cell (subject, observer, and any true interaction) regardless
  # of whether interaction is separately modelled. It is therefore keyed off
  # h > 1 alone, not `interaction`. See the note above .loam_components() in
  # LOAM_diff_utils.R for the full argument (and why LOAM_reprod's point
  # estimate is, perhaps surprisingly, provably identical either way).
  if(h > 1){
    LOAM_repeat <- comp$LOAM_repeat
  } else{
    LOAM_repeat <- NULL
  }

  LOAM_reprod <- comp$LOAM_reprod

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

  # Repeatability LOAM CI: uses the same (branch-appropriate) SSE, vE as
  # LOAM_repeat's point estimate above - the cell-based residual when
  # interaction = TRUE, the pooled/reduced residual when interaction =
  # FALSE (see the note above .loam_components() in LOAM_diff_utils.R).
  # Only needs h > 1.
  if(h > 1){
    repeat_CI <- c(z * sqrt( (h - 1) * SSE / (h * qchisq(up, vE))),
                   z * sqrt( (h - 1) * SSE / (h * qchisq(lo, vE))))
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
    ICC       <- NULL
  }

  # QQ-plot
  p <- NULL
  if(residual.plot){
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package 'lme4' is required for this function. Please install it first.")
    }

    if(interaction){
      fit <- lme4::lmer(value ~ 1 + (1 | subject * observer), data = data)
    } else{
      fit <- lme4::lmer(value ~ 1 + (1 | subject) + (1 | observer), data = data)
    }
    resid <- residuals(fit, type = "pearson")

    p <-
      ggplot2::ggplot(data.frame(resid), ggplot2::aes(sample = resid)) +
      ggplot2::stat_qq() + ggplot2::stat_qq_line() +
      ggplot2::labs(x = "Theoretical quantiles", y = "Sample quantiles")

    print(p)
  }

  estimates <- tibble(LOAM_reprod, LOAM_repeat,
                      sigmaA, sigmaB, sigmaAB, sigmaE,
                      ICC)

  intervals <- tibble(reprod_CI,
                      repeat_CI = if(!is.null(LOAM_repeat)) repeat_CI else NULL,
                      sigmaA_CI,  sigmaB_CI,
                      sigmaAB_CI = if(!is.null(sigmaAB)) sigmaAB_CI else NULL,
                      sigmaE_CI,
                      ICC_CI = if(!is.null(ICC)) ICC_CI else NULL)

  result <- list(data        = da,
                 estimates   = estimates,
                 intervals   = intervals,
                 CI.coverage = CI.coverage,
                 qq.plot     = p)

  class(result) <- "loamobject"
  return(result)

}
