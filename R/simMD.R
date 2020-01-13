#' simMD function 18-12-19
#'
#' @description Simulates measurement data to test that can be
#' used to test coverage of the LOAM function.
#'
#' @details The function returns a dataframe
#'
#' @param subjects Number of subjects to simulate
#' @param observers Number of observers to simulate
#' @param measurements Number of measurements to simulate
#' @param mu Mean of simulation
#' @param sigma2A parameter
#' @param sigma2B parameter
#' @param sigma2E parameter
#'
#' @return A dataframe of simulated measurements
#'
#' @examples
#' simMD()
#'
#' @export
#' @importFrom MASS mvrnorm
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#'

simMD <- function(subjects = 15, observers = 20, measurements = 1, mu = 0, sigma2A = 0.1, sigma2B = 0.1, sigma2E = 0.5) {

    SigmaA <- sigma2A * kronecker(kronecker(diag(subjects), matrix(1, nrow = observers, ncol = observers)),
                                  matrix(1, nrow = measurements, ncol = measurements))
    SigmaB <- sigma2B * kronecker(kronecker(diag(observers), matrix(1, nrow = subjects, ncol = subjects)),
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


