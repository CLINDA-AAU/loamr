#' simMD function 18-12-19
#'
#' @description Simulates data from the random effect models described in
#' \insertCite{christensen;textual}{loamr}
#'
#' @details The function returns a dataframe
#'
#' @param subjects number of subjects
#' @param observers number of observers
#' @param measurements number of measurements
#' @param mu overall mean
#' @param sigma2A inter-subject variance
#' @param sigma2B inter-observer variance
#' @param sigma2E residual variance
#'
#' @return A dataframe of simulated measurements. The data frame is in the format
#' required for the 'LOAM'-function.
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
#' @importFrom dplyr bind_rows
#'

simMD <- function(subjects = 15, observers = 20, measurements = 1, mu = 0, sigma2A = 0.1, sigma2B = 0.1, sigma2E = 0.5) {

    SigmaA <- sigma2A * kronecker(kronecker(diag(subjects), matrix(1, nrow = observers, ncol = observers)),
                                  matrix(1, nrow = measurements, ncol = measurements))
    SigmaB <- sigma2B * kronecker(kronecker(matrix(1, nrow = subjects, ncol = subjects), diag(observers)),
                                  matrix(1, nrow = measurements, ncol = measurements))
    SigmaE <- sigma2E * diag(subjects * observers * measurements)

    Sigma  <- SigmaA + SigmaB + SigmaE
    values <- mvrnorm(1, mu = rep(mu, subjects * observers * measurements), Sigma = Sigma)

    dat <- tibble(subject     = rep(1:subjects, each = observers * measurements),
                  observer    = rep(rep(1:observers, each = measurements), times = subjects),
                  measurement = rep(1:measurements, times = subjects * observers),
                  value       = values)

    if (measurements == 1) {
      dat$measurement <- NULL
      }

    return(dat)
}


