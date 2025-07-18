#' Simulates data from a two-way random effect model
#'
#' @description Simulates data from a two-way random effect model given by the formula A + B, where A denotes subject and B observer, as in \insertCite{christensen;textual}{loamr}.
#' An interaction term can also be included, i.e. A + B + AB.
#'
#'
#' @param mu overall mean
#' @param sigma2A inter-subject variance
#' @param sigma2B inter-observer variance
#' @param sigma2E residual variance
#' @param sigma2AB subject-observer interaction variance, only used if 'interaction = T'
#' @param interaction logical, indicates if subject-observer interaction should be included in the two-way random effects model
#' @param n_subjects number of subjects
#' @param n_observers number of observers
#' @param n_measurements number of measurements per subject-observer pair
#' @param n_sim number of simulated datasets
#'
#'
#' @return A tibble of simulated measurements. The tibble is in the format
#' required for the 'LOAM'-function when n_sim = 1, i.e. in long format with
#' columns 'subject', 'observer', 'measurement' (if h > 1), and 'value' (= the
#' simulate data). When n_sim > 1, the outputted tibble contains a column for
#' each of the simulated data sets, named 'value1', 'value2', etc.
#'
#' @references
#' \insertRef{christensen}{loamr}
#'
#' @examples
#' simMD()
#'
#' LOAM(simMD())
#'
#' @export
#' @importFrom MASS mvrnorm
#' @importFrom tibble tibble
#'

simMD <- function(mu = 0,
                  sigma2A = 2, sigma2B = 1, sigma2E = 0.5,
                  sigma2AB = NULL, interaction = F,
                  n_subjects = 15, n_observers = 20, n_measurements = 1,
                  n_sim = 1){

  stopifnot(length(sigma2A) == 1,
            length(sigma2B) == 1,
            length(sigma2E) == 1,
            sigma2A > 0,
            sigma2B > 0,
            sigma2E > 0,
            length(n_subjects)     == 1,
            length(n_observers)    == 1,
            length(n_measurements) == 1)

  if(interaction){
    if (is.null(sigma2AB)){
      stop("sigma2AB must be supplied when 'interaction = T'")
    }

    stopifnot(
      length(sigma2AB) == 1,
      sigma2AB > 0
    )
  } else{
    if (!is.null(sigma2AB)){
      warning("sigma2AB will not be used as 'interaction = F'")
    }
  }

  # Construct variance-covariance matrix:
  a <- n_subjects
  b <- n_observers
  h <- n_measurements

  SigmaA <-
    sigma2A * kronecker(diag(a), matrix(1, nrow = b * h, ncol = b * h))

  SigmaB <-
    sigma2B * kronecker(matrix(1, nrow = a, ncol = a),
                        kronecker(diag(b), matrix(1, nrow = h, ncol = h)))

  SigmaE <- sigma2E * diag(a * b * h)


  if(interaction){
    SigmaAB <-
      sigma2AB * kronecker(diag(a * b), matrix(1, ncol = h, nrow = h))

    Sigma <-  SigmaA + SigmaB + SigmaAB + SigmaE

  } else{
    Sigma <-  SigmaA + SigmaB + SigmaE
  }

  # Simulate
  sims <- mvrnorm(n = n_sim, mu = rep(mu, a * b * h), Sigma = Sigma)

  dat <- tibble(subject     = rep(1:a, each = b * h),
                observer    = rep(rep(1:b, each = h), times = a),
                measurement = if(!(h == 1)) rep(1:h, times = a * b) else NULL)

  if(n_sim == 1){
    dat$value <- sims
  } else{
    sim_names <- paste0("value", 1:n_sim)
    dat[sim_names] <- as_tibble(t(sims))
  }

  return(dat)
}



