# Internal helper functions for LOAM_diff_boot(), also used internally by
# LOAM() itself (see .loam_components() below).
# None of these are exported; not intended to be called by package users.

#' @importFrom stats qnorm quantile
#' @importFrom MASS mvrnorm
#' @importFrom tibble has_name tibble
#' @importFrom dplyr group_by mutate ungroup inner_join
#' @importFrom rlang .data
NULL

# Compute the two-way random-effects ANOVA quantities and LOAM estimates.
# This function is the shared implementation used by both
# LOAM() and LOAM_diff_boot(), ensuring consistent ANOVA calculations.
#
# Returns the augmented data frame `data` with group means attached,
# together with the ANOVA quantities and LOAM estimates.
#
# LOAM_repeat is based on Var(Y_ijk - Y_ij.) = (h - 1)/h * sigma_E^2.
# The estimator of sigma_E^2 depends on the chosen model (cell residual
# for interaction = TRUE; pooled residual for interaction = FALSE). It is NA
# when h == 1, since residual variance cannot be estimated without
# replication.
#
# LOAM_reprod is computed using the formula corresponding to the chosen
# model. Although the interaction and non-interaction formulations are
# numerically identical when h > 1, both are retained for traceability to
# the underlying model.

.loam_components <- function(data, interaction = FALSE) {

  if (!tibble::has_name(data, "measurement")) {
    data$measurement <- as.integer(1)
  }

  a <- length(unique(data$subject))
  b <- length(unique(data$observer))
  h <- length(unique(data$measurement))
  N <- nrow(data)

  if (interaction && h == 1) {
    stop("interaction = TRUE requires > 1 measurement per observer per ",
         "subject (sigma2AB cannot be separated from the residual when ",
         "there is only h = 1 measurement per cell). Use interaction = FALSE ",
         "instead.")
  }

  vA <- a - 1
  vB <- b - 1

  da <- data %>%
    group_by(.data$observer) %>%
    mutate(observerMean = mean(.data$value)) %>%
    ungroup() %>%
    group_by(.data$subject) %>%
    mutate(subjectMean = mean(.data$value)) %>%
    ungroup() %>%
    group_by(.data$subject, .data$observer) %>%
    mutate(subjectobserverMean = mean(.data$value)) %>%
    ungroup() %>%
    mutate(valueMean = mean(.data$value))

  SSA <- sum((da$subjectMean - da$valueMean)^2)
  SSB <- sum((da$observerMean - da$valueMean)^2)

  z <- abs(qnorm(0.025))

  # Reproducibility LOAM, and the SS/MS used for the sigma2A/B/(AB)/E
  # decomposition
  if (interaction) {

    vAB  <- vA * vB
    SSAB <- sum((da$subjectobserverMean - da$valueMean)^2) - SSA - SSB
    MSAB <- SSAB / vAB

    SSE <- sum((da$value - da$subjectobserverMean)^2)
    vE  <- N - a * b

    LOAM_reprod <- z * sqrt((SSB + SSAB + SSE) / N)

  } else {

    vAB  <- NULL
    SSAB <- NULL
    MSAB <- NULL

    SSE <- sum((da$value - da$subjectMean - da$observerMean + da$valueMean)^2)
    vE  <- N - a - b + 1

    LOAM_reprod <- z * sqrt((SSB + SSE) / N)
  }

  MSA <- SSA / vA
  MSB <- SSB / vB
  MSE <- SSE / vE

  # Repeatability LOAM (needs h > 1 to be estimable)
  if (h > 1) {
    LOAM_repeat <- z * sqrt((h - 1) / h * MSE)
  } else {
    LOAM_repeat <- NA_real_
  }

  list(data = da, a = a, b = b, h = h, N = N, interaction = interaction,
       SSA = SSA, SSB = SSB, SSAB = SSAB, SSE = SSE,
       MSA = MSA, MSB = MSB, MSAB = MSAB, MSE = MSE,
       vA = vA, vB = vB, vAB = vAB, vE = vE,
       LOAM_reprod = LOAM_reprod, LOAM_repeat = LOAM_repeat)
}

# Match two single-method long-format data sets (as required by LOAM()) by
# subject/observer/measurement, and return a single data frame with aligned
# value1/value2 columns, i.e. the paired design that
# .estimate_bivariate_components() needs for its cross-product terms.
.pair_loam_data <- function(data1, data2) {

  if (!tibble::has_name(data1, "measurement")) data1$measurement <- as.integer(1)
  if (!tibble::has_name(data2, "measurement")) data2$measurement <- as.integer(1)

  needed <- c("subject", "observer", "measurement", "value")
  missing1 <- setdiff(needed, names(data1))
  missing2 <- setdiff(needed, names(data2))
  if (length(missing1) > 0) stop("data1 is missing column(s): ", paste(missing1, collapse = ", "))
  if (length(missing2) > 0) stop("data2 is missing column(s): ", paste(missing2, collapse = ", "))

  d1 <- data1[needed]; names(d1)[names(d1) == "value"] <- "value1"
  d2 <- data2[needed]; names(d2)[names(d2) == "value"] <- "value2"

  merged <- inner_join(d1, d2, by = c("subject", "observer", "measurement"))

  if (nrow(merged) != nrow(data1) || nrow(merged) != nrow(data2)) {
    stop("data1 and data2 must contain measurements for exactly the same ",
         "subject/observer/measurement combinations (a paired design on ",
         "the same subjects and observers is required).")
  }

  merged
}

# Estimate the 2x2 covariance matrices for the A, B, (AB,) and E random
# effects across two methods from data in the long format required by
# LOAM(). This is the bivariate (MANOVA-style) extension of the variance
# component estimators used by LOAM().
# Cross-product terms are computed from internally paired observations
# (see .pair_loam_data()), while the per-method ANOVA quantities are
# obtained directly from data1 and data2 via .loam_components().

# The covariance matrices are always estimated under the model specified
# by `interaction`, and bootstrap samples are generated from that same
# fitted model.

.estimate_bivariate_components <- function(data1, data2, interaction = FALSE) {

  paired <- .pair_loam_data(data1, data2)

  a <- length(unique(paired$subject))
  b <- length(unique(paired$observer))
  h <- length(unique(paired$measurement))
  N <- nrow(paired)

  if (interaction && h == 1) {
    stop("interaction = TRUE requires > 1 measurement per observer per ",
         "subject (sigma2AB cannot be separated from the residual when ",
         "there is only h = 1 measurement per cell). Use interaction = ",
         "FALSE instead.")
  }

  paired <- paired %>%
    group_by(.data$subject) %>%
    mutate(subjMean1 = mean(.data$value1), subjMean2 = mean(.data$value2)) %>%
    ungroup() %>%
    group_by(.data$observer) %>%
    mutate(obsMean1 = mean(.data$value1), obsMean2 = mean(.data$value2)) %>%
    ungroup() %>%
    mutate(gm1 = mean(.data$value1), gm2 = mean(.data$value2))

  vA <- a - 1
  vB <- b - 1
  SSA_x <- sum((paired$subjMean1 - paired$gm1) * (paired$subjMean2 - paired$gm2))
  SSB_x <- sum((paired$obsMean1  - paired$gm1) * (paired$obsMean2  - paired$gm2))

  u1 <- .loam_components(data1, interaction = interaction)
  u2 <- .loam_components(data2, interaction = interaction)

  if (interaction) {

    paired <- paired %>%
      group_by(.data$subject, .data$observer) %>%
      mutate(soMean1 = mean(.data$value1), soMean2 = mean(.data$value2)) %>%
      ungroup()

    vAB <- vA * vB
    vE  <- N - a * b

    SSAB_x <- sum((paired$soMean1 - paired$gm1) * (paired$soMean2 - paired$gm2)) - SSA_x - SSB_x
    SSE_x  <- sum((paired$value1 - paired$soMean1) * (paired$value2 - paired$soMean2))

    MSA_x  <- SSA_x  / vA
    MSB_x  <- SSB_x  / vB
    MSAB_x <- SSAB_x / vAB
    MSE_x  <- SSE_x  / vE

    covA  <- (MSA_x  - MSAB_x) / (b * h)
    covB  <- (MSB_x  - MSAB_x) / (a * h)
    covAB <- (MSAB_x - MSE_x)  / h
    covE  <- MSE_x

    var2 <- function(u) {
      sigma2A  <- (u$MSA  - u$MSAB) / (u$b * u$h)
      sigma2B  <- (u$MSB  - u$MSAB) / (u$a * u$h)
      sigma2AB <- (u$MSAB - u$MSE)  / u$h
      sigma2E  <- u$MSE
      c(A = sigma2A, B = sigma2B, AB = sigma2AB, E = sigma2E)
    }

  } else {

    vAB <- NULL

    SSE_x <- sum((paired$value1 - paired$subjMean1 - paired$obsMean1 + paired$gm1) *
                 (paired$value2 - paired$subjMean2 - paired$obsMean2 + paired$gm2))

    MSA_x <- SSA_x / vA
    MSB_x <- SSB_x / vB
    MSE_x <- SSE_x / (N - a - b + 1)

    covA  <- (MSA_x - MSE_x) / (b * h)
    covB  <- (MSB_x - MSE_x) / (a * h)
    covAB <- NULL
    covE  <- MSE_x

    var2 <- function(u) {
      sigma2A <- (u$MSA - u$MSE) / (u$b * u$h)
      sigma2B <- (u$MSB - u$MSE) / (u$a * u$h)
      sigma2E <- u$MSE
      c(A = sigma2A, B = sigma2B, E = sigma2E)
    }
  }

  v1 <- var2(u1)
  v2 <- var2(u2)

  # Warn if any observed-data diagonal variance estimate is negative (the
  # bootstrap projects the resulting 2x2 matrix onto the nearest PSD matrix
  # regardless, but the user should know the point estimate itself was
  # negative, same spirit as the per-component warnings in LOAM()).
  neg <- c(A1 = unname(v1["A"]), B1 = unname(v1["B"]),
           A2 = unname(v2["A"]), B2 = unname(v2["B"]))
  if (interaction) neg <- c(neg, AB1 = unname(v1["AB"]), AB2 = unname(v2["AB"]))
  neg <- neg[neg < 0]
  if (length(neg) > 0) {
    warning("Negative variance-component estimate(s) for the observed data: ",
            paste(sprintf("%s = %.4g", names(neg), neg), collapse = ", "),
            ". Each 2x2 covariance matrix containing a negative diagonal ",
            "entry is projected to the nearest positive semi-definite matrix ",
            "before simulating the bootstrap replicates; consider checking ",
            "the estimates in this direction with LOAM().", call. = FALSE)
  }

  mk <- function(s1, s2, cov) matrix(c(s1, cov, cov, s2), 2, 2)

  list(a = a, b = b, h = h, interaction = interaction,
       SigmaA  = mk(v1["A"], v2["A"], covA),
       SigmaB  = mk(v1["B"], v2["B"], covB),
       SigmaAB = if (interaction) mk(v1["AB"], v2["AB"], covAB) else NULL,
       SigmaE  = mk(v1["E"], v2["E"], covE),
       loam1 = u1, loam2 = u2)
}

# Simulate a new paired data set (bivariate extension of simMD()),
# drawing new subject, observer, (interaction,) and residual effects from
# the fitted 2x2 (co)variance matrices. When interaction = FALSE no
# subject-observer interaction effects are simulated.
.simMD_bivariate <- function(a, b, h, SigmaA, SigmaB, SigmaAB = NULL, SigmaE,
                             mu = c(0, 0), interaction = FALSE) {

  # Project to nearest positive semidefinite matrix by truncating negative
  # eigenvalues to zero
  psd <- function(S) {
    e <- eigen((S + t(S)) / 2, symmetric = TRUE)
    e$vectors %*% diag(pmax(e$values, 0), 2) %*% t(e$vectors)
  }
  SigmaA <- psd(SigmaA); SigmaB <- psd(SigmaB); SigmaE <- psd(SigmaE)

  A_eff <- mvrnorm(a, mu = c(0, 0), Sigma = SigmaA)
  B_eff <- mvrnorm(b, mu = c(0, 0), Sigma = SigmaB)
  E_eff <- mvrnorm(a * b * h, mu = c(0, 0), Sigma = SigmaE)

  subj <- rep(1:a, each = b * h)
  obs  <- rep(rep(1:b, each = h), times = a)

  value1 <- mu[1] + A_eff[subj, 1] + B_eff[obs, 1] + E_eff[, 1]
  value2 <- mu[2] + A_eff[subj, 2] + B_eff[obs, 2] + E_eff[, 2]

  if (interaction) {
    if (is.null(SigmaAB)) stop("interaction = TRUE requires SigmaAB.")
    SigmaAB <- psd(SigmaAB)
    AB_eff  <- mvrnorm(a * b, mu = c(0, 0), Sigma = SigmaAB)
    ab_id   <- (subj - 1) * b + obs
    value1  <- value1 + AB_eff[ab_id, 1]
    value2  <- value2 + AB_eff[ab_id, 2]
  }

  data.frame(subject = subj, observer = obs, measurement = rep(1:h, times = a * b),
             value1 = value1, value2 = value2)
}
