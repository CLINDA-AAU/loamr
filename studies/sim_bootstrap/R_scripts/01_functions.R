## ---------------------------------------------------------------
## Simulation study: coverage and p-value distribution for
## LOAM_diff_boot()'s parametric bootstrap
##
## This version:
##  - Uses a vectorised, dplyr-free re-implementation of the bootstrap loop
##    (fast_bivariate_boot(), below) for the *inner* R-replicate loop
##    (.loam_components()'s dplyr pipeline was slower).
##    LOAM_diff_boot() itself (the package's real, dplyr-based function) is
##    still used once per simulated dataset to fit the observed-data
##    covariance components (.estimate_bivariate_components()), since that
##    part is cheap (called M times, not M*R times).
##  - Parallelises the outer loop over the M simulated "real" datasets
##  - For each simulated dataset, draws ONE set of R bootstrap replicates
##    and computes THREE confidence-interval constructions from the SAME
##    replicates:
##      - "recentred": the percentile interval recentred on the empirical
##        bootstrap mean (obs_diff + (quantile(diffs) - mean(diffs))),
##        i.e. what LOAM_diff_boot() itself currently uses. Not a
##        standard name in the literature - it's our own construction.
##      - "basic": the classic "basic bootstrap" / reflection interval
##        (Efron & Tibshirani 1993), 2*obs_diff -
##        quantile(diffs, 1-alpha/2) to 2*obs_diff - quantile(diffs, alpha/2).
##      - "bca": bias-corrected and accelerated (Efron 1987). Adjusts
##        which percentiles of the raw bootstrap distribution are read
##        off, correcting for both median bias (z0) and skewness (a_hat,
##        estimated via a subject-level jackknife of the point estimate).
##    Allows direct comparison of coverage/type-I error across all three,
##    as an empirical check of the choice made in LOAM_diff_boot().
##  - Reproducibility LOAM's point estimate (and hence its "true" value
##    below) does not depend on `interaction`. Repeatability's DOES: under
##    interaction = TRUE it targets sigma2E directly; under interaction =
##    FALSE it targets the *pooled* residual E[MSE_reduced] = sigma2E +
##    h*vAB/(vAB+vE_cell)*sigma2AB - see true_repeat_pooled() below, used
##    as the ground truth when interaction = FALSE.
## ---------------------------------------------------------------

library(loamr)
library(parallel)
run_demo <- F

## -----------------------------------------------------------------
## Fast vectorised bootstrap: computes R replicates of the LOAM
## difference(s) in one pass, without dplyr or a per-replicate loop.
## Mathematically equivalent to calling .simMD_bivariate() +
## .loam_components() R times (as LOAM_diff_boot() itself does), given a
## fitted `comp` object from .estimate_bivariate_components().
## -----------------------------------------------------------------

# minimal, fast bivariate normal generator (Cholesky-based) for a 2x2
# Sigma; avoids MASS::mvrnorm's per-call eigen-decomposition and argument
# checks, which matter when called with very large n (a*R, b*R, N*R draws).
.rmvn2_fast <- function(n, Sigma) {
  L <- chol(Sigma)             # upper-triangular, t(L) %*% L = Sigma
  Z <- matrix(rnorm(n * 2), n, 2)
  Z %*% L
}

fast_bivariate_boot <- function(comp, R, interaction) {
  a <- comp$a; b <- comp$b; h <- comp$h; N <- a * b * h
  z <- abs(qnorm(0.025))

  psd <- function(S) {
    e <- eigen((S + t(S)) / 2, symmetric = TRUE)
    e$vectors %*% diag(pmax(e$values, 0), 2) %*% t(e$vectors)
  }
  SigmaA <- psd(comp$SigmaA); SigmaB <- psd(comp$SigmaB); SigmaE <- psd(comp$SigmaE)

  subj <- rep(1:a, each = b * h)
  obs  <- rep(rep(1:b, each = h), times = a)

  # draw ALL R replicates' worth of each effect in one shot
  A_draw <- .rmvn2_fast(a * R, SigmaA)
  B_draw <- .rmvn2_fast(b * R, SigmaB)
  E_draw <- .rmvn2_fast(N * R, SigmaE)

  A1 <- matrix(A_draw[,1], a, R); A2 <- matrix(A_draw[,2], a, R)
  B1 <- matrix(B_draw[,1], b, R); B2 <- matrix(B_draw[,2], b, R)
  E1 <- matrix(E_draw[,1], N, R); E2 <- matrix(E_draw[,2], N, R)

  V1 <- A1[subj, , drop = FALSE] + B1[obs, , drop = FALSE] + E1
  V2 <- A2[subj, , drop = FALSE] + B2[obs, , drop = FALSE] + E2

  if (interaction) {
    SigmaAB <- psd(comp$SigmaAB)
    AB_draw <- .rmvn2_fast(a * b * R, SigmaAB)
    ab_id <- (subj - 1) * b + obs
    AB1 <- matrix(AB_draw[,1], a * b, R); AB2 <- matrix(AB_draw[,2], a * b, R)
    V1 <- V1 + AB1[ab_id, , drop = FALSE]
    V2 <- V2 + AB2[ab_id, , drop = FALSE]
  }

  bsub <- function(M, v) sweep(M, 2, v, "-")   # column-wise subtraction

  loam_batch <- function(V) {
    grand_mean <- colMeans(V)
    subj_mean  <- rowsum(V, subj) / (b * h)
    obs_mean   <- rowsum(V, obs)  / (a * h)

    SSA <- colSums(bsub(subj_mean[subj, , drop = FALSE], grand_mean)^2)
    SSB <- colSums(bsub(obs_mean[obs, , drop = FALSE],  grand_mean)^2)

    if (interaction) {
      cell_id   <- (subj - 1) * b + obs
      cell_mean <- rowsum(V, cell_id) / h
      SSAB <- colSums(bsub(cell_mean[cell_id, , drop = FALSE], grand_mean)^2) - SSA - SSB
      SSE  <- colSums((V - cell_mean[cell_id, , drop = FALSE])^2)
      vE   <- N - a * b
      LOAM_reprod <- z * sqrt((SSB + SSAB + SSE) / N)
    } else {
      SSE <- colSums((V - subj_mean[subj, , drop = FALSE] - obs_mean[obs, , drop = FALSE] +
                        matrix(grand_mean, N, R, byrow = TRUE))^2)
      vE  <- N - a - b + 1
      LOAM_reprod <- z * sqrt((SSB + SSE) / N)
    }

    LOAM_repeat <- if (h > 1) z * sqrt((h - 1) / h * (SSE / vE)) else rep(NA_real_, R)
    list(LOAM_reprod = LOAM_reprod, LOAM_repeat = LOAM_repeat)
  }

  l1 <- loam_batch(V1); l2 <- loam_batch(V2)

  list(diffs_reprod = l1$LOAM_reprod - l2$LOAM_reprod,
       diffs_repeat = if (h > 1) l1$LOAM_repeat - l2$LOAM_repeat else NULL)
}

## Validated against LOAM_diff_boot() (the package's own dplyr-based
## implementation) by comparing the two bootstrap replicate distributions
## over R = 3000 replicates (t-test on means, F-test on variances): both
## gave p > 0.15 in every comparison across interaction = TRUE/FALSE and
## h = 1/h > 1, i.e. no evidence of a systematic difference. Re-run that
## check yourself after any change to this function or to
## .loam_components()/.simMD_bivariate() in the package, since nothing
## here enforces the equivalence automatically.

## -----------------------------------------------------------------
## Three confidence-interval / p-value constructions from the same
## bootstrap replicates `diffs` (see the file header for definitions).
## Named to match established bootstrap-literature terminology
## (Efron & Tibshirani 1993; Efron 1987) except for "recentred", which does
## not have a standard name in the literature.
## -----------------------------------------------------------------

# "recentred": the percentile interval recentred on the empirical
# bootstrap mean (obs_diff + (quantile(diffs) - mean(diffs)))
ci_recentred <- function(diffs, obs_diff, alpha) {
  centred <- diffs - mean(diffs)
  t0 <- 0 - obs_diff
  list(ci = obs_diff + c(quantile(centred, alpha / 2), quantile(centred, 1 - alpha / 2)),
       p  = 2 * min(mean(centred <= t0), mean(centred >= t0)))
}

# "basic": the classic "basic bootstrap" / reflection interval
# (Efron & Tibshirani 1993), 2*obs_diff - quantile(diffs,
# 1-alpha/2) to 2*obs_diff - quantile(diffs, alpha/2).
ci_basic <- function(diffs, obs_diff, alpha) {
  lo <- 2 * obs_diff - quantile(diffs, 1 - alpha / 2)
  hi <- 2 * obs_diff - quantile(diffs, alpha / 2)
  # p-value consistent with this CI (p < alpha <=> 0 outside (lo, hi)):
  p <- 2 * min(mean(diffs <= 2 * obs_diff), mean(diffs >= 2 * obs_diff))
  list(ci = unname(c(lo, hi)), p = p)
}

# "bca": bias-corrected and accelerated (Efron 1987). Adjusts which
# percentiles of the RAW bootstrap distribution are read off (rather than
# shifting/reflecting the values), correcting both for the bootstrap
# distribution's median bias relative to obs_diff (z0) and for its
# skewness (the acceleration a_hat, estimated here via a subject-level
# jackknife of the point estimate - see .jackknife_diffs()).
ci_bca <- function(diffs, obs_diff, jack_vals, alpha) {
  R <- length(diffs)
  # clamp away from the boundary to avoid +/-Inf from qnorm(0) / qnorm(1)
  eps <- 1 / (2 * R)
  clamp <- function(p) pmin(pmax(p, eps), 1 - eps)

  z0 <- qnorm(clamp(mean(diffs < obs_diff)))

  jack_vals <- jack_vals[is.finite(jack_vals)]
  jm  <- mean(jack_vals)
  num <- sum((jm - jack_vals)^3)
  den <- 6 * (sum((jm - jack_vals)^2))^1.5
  a_hat <- if (den == 0 || length(jack_vals) < 3) 0 else num / den

  adj_p <- function(z) {
    w <- z0 + z
    denom <- 1 - a_hat * w
    if (denom == 0) return(NA_real_)
    pnorm(z0 + w / denom)
  }

  p_lo <- clamp(adj_p(qnorm(alpha / 2)))
  p_hi <- clamp(adj_p(qnorm(1 - alpha / 2)))
  ci <- unname(quantile(diffs, c(p_lo, p_hi), na.rm = TRUE, type = 7))

  # p-value: invert the BCa percentile transform to find the significance
  # level at which 0 sits exactly on the boundary (reduces exactly to the
  # plain-percentile p-value when z0 = 0, a_hat = 0 - see derivation notes
  # in the package chat history / project notes).
  q0 <- clamp(mean(diffs <= 0))
  u  <- qnorm(q0)
  denom <- 1 + a_hat * (u - z0)
  z <- if (denom == 0) sign(u - z0) * 20 else (u - z0) / denom - z0
  p <- 2 * min(pnorm(z), 1 - pnorm(z))                                          # OBS: CI-inverted BCa p-value (not standard)

  list(ci = ci, p = p, z0 = z0, a_hat = a_hat)
}

# Subject-level jackknife of the observed LOAM difference(s): leave one
# subject out at a time, refit .estimate_bivariate_components() on the
# remaining data, and record the resulting point-estimate difference(s).
# Used only for BCa's acceleration constant. Subjects (rather than
# observers, or a fully crossed jackknife) are used as the resampling
# unit, since each subject contributes a correlated block of
# observer x measurement rows - the natural "independent unit" in this
# design; a fully crossed jackknife would be a reasonable extension.
.jackknife_diffs <- function(data1, data2, interaction) {
  subjects <- unique(data1$subject)
  a <- length(subjects)
  jack_reprod <- numeric(a)
  jack_repeat <- rep(NA_real_, a)

  for (i in seq_along(subjects)) {
    keep <- data1$subject != subjects[i]
    ci <- .estimate_bivariate_components(data1[keep, ], data2[keep, ], interaction = interaction)
    jack_reprod[i] <- ci$loam1$LOAM_reprod - ci$loam2$LOAM_reprod
    if (ci$h > 1) jack_repeat[i] <- ci$loam1$LOAM_repeat - ci$loam2$LOAM_repeat
  }

  list(reprod = jack_reprod, repeat_ = jack_repeat)
}

## -----------------------------------------------------------------
## One simulated dataset -> one bootstrap analysis, all CI types.
## Factored out of run_sim_study() so it can be handed to parLapply().
## -----------------------------------------------------------------


.one_sim_rep <- function(m, a, b, h, SigmaA, SigmaB, SigmaAB, SigmaE,
                         R, alpha, mu, interaction, base_seed) {

  set.seed(base_seed + m)

  # DGP always draws from the full (with-interaction, AB) model structure;
  # whether a *genuine* interaction exists depends on SigmaAB itself (set it to
  # a zero matrix if no interaction wanted)
  sim <- .simMD_bivariate(a, b, h, SigmaA, SigmaB, SigmaAB, SigmaE, mu = mu,
                          interaction = TRUE)

  data1 <- data.frame(subject = sim$subject, observer = sim$observer,
                      measurement = sim$measurement, value = sim$value1)
  data2 <- data.frame(subject = sim$subject, observer = sim$observer,
                      measurement = sim$measurement, value = sim$value2)

  comp <- .estimate_bivariate_components(data1, data2, interaction = interaction)
  obs_diff_reprod <- comp$loam1$LOAM_reprod - comp$loam2$LOAM_reprod
  has_repeat <- comp$h > 1
  obs_diff_repeat <- if (has_repeat) comp$loam1$LOAM_repeat - comp$loam2$LOAM_repeat else NA_real_

  fb <- fast_bivariate_boot(comp, R, interaction)
  jk <- .jackknife_diffs(data1, data2, interaction)

  r_repro <- ci_recentred(fb$diffs_reprod, obs_diff_reprod, alpha)
  b_repro <- ci_basic(fb$diffs_reprod, obs_diff_reprod, alpha)
  c_repro <- ci_bca(fb$diffs_reprod, obs_diff_reprod, jk$reprod, alpha)

  row <- data.frame(
    diff_repro = obs_diff_reprod,
    lo_repro_recentred = r_repro$ci[1], hi_repro_recentred = r_repro$ci[2], p_repro_recentred = r_repro$p,
    lo_repro_basic = b_repro$ci[1], hi_repro_basic = b_repro$ci[2], p_repro_basic = b_repro$p,
    lo_repro_bca = c_repro$ci[1], hi_repro_bca = c_repro$ci[2], p_repro_bca = c_repro$p,
    diff_repeat = NA_real_,
    lo_repeat_recentred = NA_real_, hi_repeat_recentred = NA_real_, p_repeat_recentred = NA_real_,
    lo_repeat_basic = NA_real_, hi_repeat_basic = NA_real_, p_repeat_basic = NA_real_,
    lo_repeat_bca = NA_real_, hi_repeat_bca = NA_real_, p_repeat_bca = NA_real_
  )

  if (has_repeat) {
    r_rep <- ci_recentred(fb$diffs_repeat, obs_diff_repeat, alpha)
    b_rep <- ci_basic(fb$diffs_repeat, obs_diff_repeat, alpha)
    c_rep <- ci_bca(fb$diffs_repeat, obs_diff_repeat, jk$repeat_, alpha)
    row$diff_repeat <- obs_diff_repeat
    row$lo_repeat_recentred <- r_rep$ci[1]; row$hi_repeat_recentred <- r_rep$ci[2]; row$p_repeat_recentred <- r_rep$p
    row$lo_repeat_basic <- b_rep$ci[1]; row$hi_repeat_basic <- b_rep$ci[2]; row$p_repeat_basic <- b_rep$p
    row$lo_repeat_bca <- c_rep$ci[1]; row$hi_repeat_bca <- c_rep$ci[2]; row$p_repeat_bca <- c_rep$p
  }

  row$has_repeat <- has_repeat
  row
}

## -----------------------------------------------------------------
## Main entry point: run M "real datasets" -> M bootstrap analyses,
## in parallel across `n_cores`. The DGP always includes a true
## interaction term (SigmaAB's diagonal), regardless of the `interaction`
## argument, which instead controls what LOAM_diff_boot()/the fast
## bootstrap is asked to fit/test - exactly mirroring how an analyst would
## choose interaction = TRUE or FALSE without knowing whether a true
## interaction exists. Set SigmaAB to a zero matrix to instead study the
## well-specified case (see the demo below for both).
## -----------------------------------------------------------------
run_sim_study <- function(a, b, h,
                          SigmaA, SigmaB, SigmaAB, SigmaE,
                          M = 200,      # number of simulated "real" datasets
                          R = 500,      # number of bootstrap replications per dataset
                          CI.coverage = 0.95,
                          mu = c(0, 0),
                          seed = 1,
                          interaction = FALSE,
                          n_cores = max(1, parallel::detectCores() - 1)) {

  alpha <- 1 - CI.coverage
  z <- abs(qnorm(alpha / 2))

  ## True reproducibility LOAM (identical regardless of `interaction`)
  true_repro <- function(Sb, Sab, Se) z * sqrt((b - 1) / b * (Sb + Sab) + (b*h - 1) / (b*h) * Se)
  true_diff_repro <-
    true_repro(SigmaB[1,1], SigmaAB[1,1], SigmaE[1,1]) -
    true_repro(SigmaB[2,2], SigmaAB[2,2], SigmaE[2,2])

  ## True repeatability LOAM: depends on `interaction`
  vAB <- (a - 1) * (b - 1)
  vE_cell <- a * b * (h - 1)

  if (interaction) {
    true_repeat <- function(Se) z * sqrt((h - 1) / h * Se)
    true_diff_repeat <- true_repeat(SigmaE[1,1]) - true_repeat(SigmaE[2,2])
  } else {
    true_repeat_pooled <- function(Se, Sab) {
      MSE_reduced <- Se + h * vAB / (vAB + vE_cell) * Sab
      z * sqrt((h - 1) / h * MSE_reduced)
    }
    true_diff_repeat <- true_repeat_pooled(SigmaE[1,1], SigmaAB[1,1]) -
      true_repeat_pooled(SigmaE[2,2], SigmaAB[2,2])
  }

  ## Dispatch across a PSOCK cluster (works on Windows/macOS/Linux alike).
  ## n_cores = 1 runs serially (still via parLapply, on a 1-node cluster)
  ## so the same code path is exercised regardless of machine.
  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  parallel::clusterExport(cl, varlist = c(
    ".loam_components", ".estimate_bivariate_components", ".simMD_bivariate",
    ".pair_loam_data", "fast_bivariate_boot", ".rmvn2_fast", ".jackknife_diffs",
    "ci_recentred", "ci_basic", "ci_bca", ".one_sim_rep"
  ), envir = globalenv())
  parallel::clusterEvalQ(cl, { library(dplyr); library(tibble); library(magrittr); library(rlang); library(MASS) })

  rows <- parallel::parLapply(cl, 1:M, .one_sim_rep,
                              a = a, b = b, h = h, SigmaA = SigmaA, SigmaB = SigmaB,
                              SigmaAB = SigmaAB, SigmaE = SigmaE, R = R, alpha = alpha,
                              mu = mu, interaction = interaction, base_seed = seed * 100000)

  out <- do.call(rbind, rows)
  has_repeat <- any(out$has_repeat)

  list(out = out,
       true_diff_repro  = true_diff_repro,
       true_diff_repeat = true_diff_repeat,
       has_repeat = has_repeat)
}

## -----------------------------------------------------------------
## Summarising: coverage + calibration of p-values, all three CI types
## side by side
## -----------------------------------------------------------------
summarise_sim <- function(sim, alpha = 0.05) {
  out <- sim$out

  report_one <- function(label, true_val,
                         lo_r, hi_r, p_r, lo_b, hi_b, p_b, lo_c, hi_c, p_c) {
    one_line <- function(name, lo, hi, p) {
      cover <- mean(lo <= true_val & true_val <= hi, na.rm = TRUE)
      width <- mean(hi - lo, na.rm = TRUE)
      cat(sprintf("  %-12s coverage (nominal %.0f%%) = %.3f   mean width = %.4f   P(p < %.2f) = %.3f\n",
                  name, 100 * (1 - alpha), cover, width, alpha, mean(p < alpha, na.rm = TRUE)))
    }
    cat(sprintf("--- %s (true diff = %.4f) ---\n", label, true_val))
    one_line("recentred:", lo_r, hi_r, p_r)
    one_line("basic:",     lo_b, hi_b, p_b)
    one_line("bca:",       lo_c, hi_c, p_c)
    cat("  (P(p < alpha) is the type-I error rate under H0, or power under H1)\n\n")
  }

  report_one("Reproducibility", sim$true_diff_repro,
             out$lo_repro_recentred, out$hi_repro_recentred, out$p_repro_recentred,
             out$lo_repro_basic, out$hi_repro_basic, out$p_repro_basic,
             out$lo_repro_bca, out$hi_repro_bca, out$p_repro_bca)

  if (sim$has_repeat) {
    report_one("Repeatability", sim$true_diff_repeat,
               out$lo_repeat_recentred, out$hi_repeat_recentred, out$p_repeat_recentred,
               out$lo_repeat_basic, out$hi_repeat_basic, out$p_repeat_basic,
               out$lo_repeat_bca, out$hi_repeat_bca, out$p_repeat_bca)
  } else {
    cat("Repeatability: not available (h = 1)\n")
  }

  invisible(out)
}

## -----------------------------------------------------------------
## Plot: p-value histograms and CI coverage, all three CI types
## -----------------------------------------------------------------
plot_sim <- function(sim, alpha = 0.05, file = NULL) {
  out <- sim$out
  if (!is.null(file)) pdf(file, width = 16, height = 8.5)

  n_rows <- if (sim$has_repeat) 2 else 1
  par(mfrow = c(n_rows, 6))

  one_block <- function(diff, lo_r, hi_r, p_r, lo_b, hi_b, p_b, lo_c, hi_c, p_c, true_val, label) {

    hist_ci <- function(lo, hi, p, name) {
      hist(p, breaks = seq(0, 1, 0.05), col = "grey80",
           main = paste(label, "-", name), xlab = "p-value")
      abline(h = length(p) * 0.05, col = "red", lty = 2)
    }
    strip_ci <- function(lo, hi, name) {
      ord <- order(diff)
      covered <- lo[ord] <= true_val & true_val <= hi[ord]
      plot(NA, xlim = range(c(lo, hi), na.rm = TRUE), ylim = c(1, length(diff)),
           xlab = "diff", ylab = "dataset (sorted)", main = paste(label, "CIs -", name))
      segments(lo[ord], seq_along(diff), hi[ord], seq_along(diff),
               col = ifelse(covered, "grey60", "red"))
      abline(v = true_val, col = "blue", lwd = 2)
    }

    hist_ci(lo_r, hi_r, p_r, "recentred"); strip_ci(lo_r, hi_r, "recentred")
    hist_ci(lo_b, hi_b, p_b, "basic");     strip_ci(lo_b, hi_b, "basic")
    hist_ci(lo_c, hi_c, p_c, "bca");       strip_ci(lo_c, hi_c, "bca")
  }

  one_block(out$diff_repro, out$lo_repro_recentred, out$hi_repro_recentred, out$p_repro_recentred,
            out$lo_repro_basic, out$hi_repro_basic, out$p_repro_basic,
            out$lo_repro_bca, out$hi_repro_bca, out$p_repro_bca,
            sim$true_diff_repro, "Reproducibility")

  if (sim$has_repeat) {
    one_block(out$diff_repeat, out$lo_repeat_recentred, out$hi_repeat_recentred, out$p_repeat_recentred,
              out$lo_repeat_basic, out$hi_repeat_basic, out$p_repeat_basic,
              out$lo_repeat_bca, out$hi_repeat_bca, out$p_repeat_bca,
              sim$true_diff_repeat, "Repeatability")
  }

  if (!is.null(file)) dev.off()
}


## ===================================================================
## Demo. Set run_demo <- FALSE before source()-ing this file to load
## only the functions above.
## ===================================================================
if (!exists("run_demo")) run_demo <- TRUE

if (run_demo) {

  a <- 15; b <- 10; h <- 3

  SigmaA_H0  <- matrix(c(2, 0.5, 0.5, 2), 2, 2)
  SigmaB_H0  <- matrix(c(1, 0.2, 0.2, 1), 2, 2)
  SigmaE_H0  <- matrix(c(0.5, 0, 0, 0.5), 2, 2)

  ## Misspecified case: a genuine (non-zero) true interaction, tested under
  ## interaction = FALSE - the "worst case" for that model choice.
  SigmaAB_misspec <- matrix(c(0.3, 0.15, 0.15, 0.3), 2, 2)
  ## Well-specified case: no true interaction at all.
  SigmaAB_null <- matrix(0, 2, 2)

  ## OBS: M and R are lowered here for a quick demo. For a real study use
  ## e.g. M >= 300 and R >= 2000. Set n_cores explicitly if you don't want
  ## run_sim_study() to use (detectCores() - 1) by default.
  cat("=== H0, interaction = TRUE (true interaction present) ===\n")
  sim_H0_int <- run_sim_study(a, b, h, SigmaA_H0, SigmaB_H0, SigmaAB_misspec, SigmaE_H0,
                              M = 100, R = 1000, seed = 1, interaction = TRUE)
  summarise_sim(sim_H0_int)
  plot_sim(sim_H0_int, file = "sim_H0_interaction.pdf")

  cat("=== H0, interaction = FALSE, MISSPECIFIED (true interaction present but not modelled) ===\n")
  sim_H0_mis <- run_sim_study(a, b, h, SigmaA_H0, SigmaB_H0, SigmaAB_misspec, SigmaE_H0,
                              M = 100, R = 1000, seed = 1, interaction = FALSE)
  summarise_sim(sim_H0_mis)
  plot_sim(sim_H0_mis, file = "sim_H0_no_interaction_misspecified.pdf")

  cat("=== H0, interaction = FALSE, WELL-SPECIFIED (no true interaction) ===\n")
  sim_H0_wellspec <- run_sim_study(a, b, h, SigmaA_H0, SigmaB_H0, SigmaAB_null, SigmaE_H0,
                                   M = 100, R = 1000, seed = 1, interaction = FALSE)
  summarise_sim(sim_H0_wellspec)
  plot_sim(sim_H0_wellspec, file = "sim_H0_no_interaction_wellspecified.pdf")

  ## Under an alternative: method 2 has larger sigma_B^2
  SigmaB_H1 <- matrix(c(1, 0.5, 0.5, 2.5), 2, 2)

  cat("=== H1 (difference in sigma_B), interaction = TRUE ===\n")
  sim_H1 <- run_sim_study(a, b, h, SigmaA_H0, SigmaB_H1, SigmaAB_misspec, SigmaE_H0,
                          M = 100, R = 1000, seed = 2, interaction = TRUE)
  summarise_sim(sim_H1)
  plot_sim(sim_H1, file = "sim_H1.pdf")

  ## h = 1: repeatability not available.
  cat("=== h = 1 (single measurement per cell) ===\n")
  sim_h1 <- run_sim_study(15, 10, 1, SigmaA_H0, SigmaB_H0, SigmaAB_null, SigmaE_H0,
                          M = 60, R = 1000, seed = 3, interaction = FALSE)
  summarise_sim(sim_h1)

}  # end if (run_demo)
