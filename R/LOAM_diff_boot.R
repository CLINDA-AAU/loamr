#' Parametric bootstrap to compare two LOAMs obtained by different methods
#' (same subjects, observers, and number of repeated measurements)
#'
#' @description Compares the 95\% limits of agreement with the mean (LOAM)
#' obtained from two measurement methods (or devices, protocols, etc.)
#' applied to the *same* subjects, by the *same* observers, with the *same*
#' number of repeated measurements. Using a parametric bootstrap, the function
#' constructs a confidence interval for the difference
#' \eqn{\mathrm{LOAM}_1 - \mathrm{LOAM}_2} and a p-value for testing the null
#' hypothesis of no difference (\eqn{H_0}: LOAM1 = LOAM2) against a two-sided
#' or one-sided alternative, as specified by \code{alternative}. Both the
#' reproducibility LOAM and, whenever both data sets contain more than one
#' measurement per subject-observer cell, the repeatability LOAM are compared.
#'
#' @details Comparing two LOAMs computed on paired data requires accounting
#' for the correlation between the two LOAM estimates, since they are based
#' on the same subjects and observers. \code{LOAM_diff_boot} therefore uses a
#' *parametric* bootstrap based on a bivariate extension of the two-way random
#' effects model underlying \code{\link{LOAM}}, in which the subject, observer,
#' (interaction,) and residual effects for the two methods are modelled as
#' correlated pairs with unrestricted \eqn{2 \times 2} covariance matrices.
#' These covariance matrices are estimated from the paired data (a MANOVA-type
#' cross-product generalisation of the moment estimators underlying
#' \code{\link{LOAM}}), under the model specified by \code{interaction} (with
#' or without a subject-observer interaction term). New realisations of the
#' subject, observer, (interaction,) and residual effects for both methods are
#' then simulated from this fitted model in each bootstrap replicate, ensuring
#' that the bootstrap data are generated under the same assumptions used to
#' estimate the LOAMs. The confidence interval and p-value are constructed
#' from the resulting bootstrap replicates using the "basic bootstrap"
#' (reflection) method.As p-values are based on R bootstrap replicates, they
#' have resolution 1/R; a reported p-value of 0 should be read as p < 1/R.
#' See details in \insertCite{christensen2025;textual}{loamr}.
#'
#' Both \code{data1} and \code{data2} must be in the same long-format shape
#' required by \code{\link{LOAM}} (columns \code{subject}, \code{observer},
#' \code{value}, and optionally \code{measurement}), and must contain
#' measurements for exactly the same subject/observer/measurement
#' combinations, since the design is required to be paired.
#'
#' As for \code{\link{LOAM}}, \code{interaction = TRUE} requires more than
#' one measurement per subject-observer cell.
#' Repeatability is compared whenever both data sets contain replicated
#' measurements. Note that the
#' repeatability LOAM depends on \code{interaction}: it is based on the
#' cell-based residual variance when \code{interaction = TRUE}, and on the
#' pooled (reduced-model) residual variance when \code{interaction = FALSE},
#' which also absorbs any true subject-observer interaction variance. The
#' reproducibility LOAM point estimate is identical under both settings, but
#' its bootstrap distribution is generated under the model specified by
#' \code{interaction}.
#'
#'#' If any variance component estimated from the observed data is negative
#' (which can happen in finite samples when a true component is close to
#' zero), a warning is issued. The estimated covariance matrices are
#' projected onto the nearest positive semi-definite matrix before the
#' bootstrap data are simulated; the observed LOAM estimates are not
#' affected. Interpret the results with caution when this warning occurs.
#'
#' @param data1 a data frame with measurements from method 1, in the same
#'   long-format shape required by \code{\link{LOAM}} (see 'Details')
#' @param data2 a data frame with measurements from method 2, in the same
#'   long-format shape required by \code{\link{LOAM}}, on the same
#'   subject/observer/measurement combinations as \code{data1}
#' @param interaction logical, indicates if subject-observer interaction
#'   should be modelled as its own variance component, both when fitting
#'   the covariance matrices and when simulating the bootstrap replicates.
#'   Requires repeated measurements whenever \code{TRUE} (see 'Details').
#' @param alternative a character string specifying the alternative
#'   hypothesis for \eqn{H_0}: LOAM1 = LOAM2, matching the convention used
#'   by e.g. \code{\link[stats]{t.test}}: \code{"two.sided"} (the default),
#'   \code{"less"} (LOAM1 < LOAM2), or \code{"greater"} (LOAM1 > LOAM2). For
#'   \code{"less"}, \code{intervals} reports a one-sided upper confidence
#'   bound (the lower bound is \code{-Inf}); for \code{"greater"}, a
#'   one-sided lower bound (\code{Inf} upper bound); for \code{"two.sided"},
#'   both bounds as usual.
#' @param CI.coverage coverage probability for the confidence interval on
#'   the LOAM difference(s).
#' @param R number of parametric bootstrap replicates.
#' @param seed optional integer used to seed the random number generator
#'   before simulating the bootstrap replicates, for reproducibility.
#'
#' @return An object of class \code{"loamdiffobject"}, a list with elements:
#' \describe{
#'   \item{estimates}{a tibble with the observed point estimates of the
#'     reproducibility LOAM (and, if repeated measurements, repeatability
#'     LOAM) for each method, and their observed difference
#'     (\code{diff_reprod}, \code{diff_repeat}) -- i.e. the difference
#'     computed directly from \code{data1} and \code{data2}, not a bootstrap
#'     average.}
#'   \item{intervals}{a 2-row tibble with the lower (row 1) and upper
#'     (row 2) bootstrap confidence limits for the LOAM difference(s).}
#'   \item{p_values}{a tibble with the bootstrap p-value(s) for
#'     \eqn{H_0}: LOAM1 = LOAM2 against the requested \code{alternative},
#'     constructed to be consistent (up to Monte Carlo resolution) with
#'     \code{intervals} (\eqn{p < \alpha
#'     \iff} 0 lies outside the \eqn{100(1-\alpha)\%} confidence region).}
#'   \item{interaction, alternative, CI.coverage, R}{the arguments used.}
#'   \item{has_repeat}{logical, whether repeatability was compared.}
#'   \item{boot_diffs_reprod, boot_diffs_repeat}{the raw bootstrap
#'     replicates of the LOAM difference(s), for diagnostic use (e.g.
#'     inspecting the bootstrap distribution directly).}
#' }
#'
#' @references
#' \insertRef{christensen}{loamr}
#' \insertRef{christensen2025}{loamr}
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' data1 <- simMD(sigma2B = 1,   sigma2E = 0.5, n_subjects = 20,
#'                n_observers = 8, n_measurements = 3)
#' data2 <- simMD(sigma2B = 2.5, sigma2E = 0.5, n_subjects = 20,
#'                n_observers = 8, n_measurements = 3)
#' # data1 and data2 already share identical subject/observer/measurement
#' # columns here, since simMD() assigns these deterministically from
#' # n_subjects/n_observers/n_measurements alone; for a real analysis they
#' # would instead be two methods measured on the same subjects/observers.
#'
#' res <- LOAM_diff_boot(data1, data2, R = 200)
#' res
#' res$estimates
#' res$intervals
#' }
#'
#' @export
#' @importFrom stats qnorm quantile
#' @importFrom tibble tibble
#' @seealso \code{\link{LOAM}}, \code{\link{simMD}}
LOAM_diff_boot <- function(data1, data2, interaction = FALSE,
                           alternative = c("two.sided", "less", "greater"),
                           CI.coverage = 0.95, R = 1000, seed = NULL) {

  alternative <- match.arg(alternative)
  alpha <- 1 - CI.coverage

  comp <- .estimate_bivariate_components(data1, data2, interaction = interaction)

  # Repeatability is bootstrapped whenever h > 1
  # .loam_components() returns a LOAM_repeat that is consistent with the
  # requested model, and the bootstrap DGP is fitted under that same model
  has_repeat <- comp$h > 1

  obs_diff_reprod <- comp$loam1$LOAM_reprod - comp$loam2$LOAM_reprod
  diffs_reprod <- numeric(R)

  if (has_repeat) {
    obs_diff_repeat <- comp$loam1$LOAM_repeat - comp$loam2$LOAM_repeat
    diffs_repeat <- numeric(R)
  }

  if (!is.null(seed)) set.seed(seed)

  for (r in seq_len(R)) {
    sim <- .simMD_bivariate(comp$a, comp$b, comp$h,
                            comp$SigmaA, comp$SigmaB, comp$SigmaAB, comp$SigmaE,
                            interaction = interaction)

    dat <- sim[c("subject", "observer", "measurement")]

    dat$value <- sim$value1
    l1 <- .loam_components(dat, interaction = interaction)

    dat$value <- sim$value2
    l2 <- .loam_components(dat, interaction = interaction)

    diffs_reprod[r] <- l1$LOAM_reprod - l2$LOAM_reprod
    if (has_repeat) diffs_repeat[r] <- l1$LOAM_repeat - l2$LOAM_repeat
  }

  # Bootstrap p-value for H0: LOAM1 = LOAM2, constructed to be consistent
  # with the CI below (p < alpha  <=>  0 lies outside the CI/one-sided
  # bound). Uses the "basic bootstrap" (reflection) construction
  pval <- function(diffs, obs_diff) {
    switch(alternative,
          two.sided = 2 * min(mean(diffs <= 2 * obs_diff), mean(diffs >= 2 * obs_diff)),
          less      = mean(diffs <= 2 * obs_diff),
          greater   = mean(diffs >= 2 * obs_diff))
  }

  # Confidence region for the difference: two-sided as before, or a
  # one-sided bound matching `alternative` (the other bound is left
  # unbounded, following the stats::t.test() convention).
  region <- function(diffs, obs_diff) {
    switch(alternative,
          two.sided = c(2 * obs_diff - quantile(diffs, 1 - alpha / 2),
                        2 * obs_diff - quantile(diffs, alpha / 2)),
          less      = c(-Inf, 2 * obs_diff - quantile(diffs, alpha)),
          greater   = c(2 * obs_diff - quantile(diffs, 1 - alpha), Inf))
  }

  ci_reprod <- unname(region(diffs_reprod, obs_diff_reprod))
  p_reprod  <- pval(diffs_reprod, obs_diff_reprod)

  if (has_repeat) {
    ci_repeat <- unname(region(diffs_repeat, obs_diff_repeat))
    p_repeat  <- pval(diffs_repeat, obs_diff_repeat)
  } else {
    obs_diff_repeat <- NA_real_
    ci_repeat <- c(NA_real_, NA_real_)
    p_repeat  <- NA_real_
  }

  estimates <- tibble(
    LOAM1_reprod = comp$loam1$LOAM_reprod, LOAM2_reprod = comp$loam2$LOAM_reprod,
    diff_reprod  = obs_diff_reprod,
    LOAM1_repeat = comp$loam1$LOAM_repeat, LOAM2_repeat = comp$loam2$LOAM_repeat,
    diff_repeat  = obs_diff_repeat
  )

  intervals <- tibble(diff_reprod_CI = ci_reprod, diff_repeat_CI = ci_repeat)

  p_values <- tibble(p_reprod = p_reprod, p_repeat = p_repeat)

  result <- list(
    estimates   = estimates,
    intervals   = intervals,
    p_values    = p_values,
    interaction = interaction,
    alternative = alternative,
    has_repeat  = has_repeat,
    CI.coverage = CI.coverage,
    R = R,
    boot_diffs_reprod = diffs_reprod,
    boot_diffs_repeat = if (has_repeat) diffs_repeat else NULL
  )

  class(result) <- "loamdiffobject"
  result
}

#' @export
print.loamdiffobject <- function(x, ...) {

  pr <- function(label, l1, l2, diff, ci, p) {
    ci_text <- switch(x$alternative,
      two.sided = sprintf("%g%% CI for diff: (%.4f, %.4f)", 100 * x$CI.coverage, ci[1], ci[2]),
      less      = sprintf("%g%% upper bound for diff: %.4f", 100 * x$CI.coverage, ci[2]),
      greater   = sprintf("%g%% lower bound for diff: %.4f", 100 * x$CI.coverage, ci[1]))
    h1_text <- switch(x$alternative,
      two.sided = "H0: LOAM1 = LOAM2",
      less      = "H0: LOAM1 >= LOAM2, H1: LOAM1 < LOAM2",
      greater   = "H0: LOAM1 <= LOAM2, H1: LOAM1 > LOAM2")
    cat(sprintf(
      "%s:\n  LOAM1 = %.4f, LOAM2 = %.4f, diff = %.4f\n  %s\n  bootstrap p-value (%s): %.4f\n\n",
      label, l1, l2, diff, ci_text, h1_text, p))
  }

  e <- x$estimates; i <- x$intervals; p <- x$p_values

  pr("Reproducibility LOAM", e$LOAM1_reprod, e$LOAM2_reprod, e$diff_reprod,
     i$diff_reprod_CI, p$p_reprod)

  if (x$has_repeat) {
    pr("Repeatability LOAM", e$LOAM1_repeat, e$LOAM2_repeat, e$diff_repeat,
       i$diff_repeat_CI, p$p_repeat)
  } else {
    cat("Repeatability LOAM: not available (requires > 1 measurement per ",
        "subject-observer cell)\n\n", sep = "")
  }

  invisible(x)
}
