#' Simulates measurement data
#'
#' @description This function simulates measurement data
#'
#' @details The function returns a dataframe
#'
#' @param subjects A dataframe,
#' @param observers A dataframe,
#' @param measurements A dataframe,
#' @param mu A dataframe,
#' @param sigma2A A dataframe,
#' @param sigma2B A dataframe,
#' @param sigma2E A dataframe,
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


