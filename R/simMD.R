#' Simulates measurement data
#'
#' @description This function simulates measurement data
#'
#' @details The function returns a dataframe
#'
#' @param subjects A dataframe,
#' @param readers A dataframe,
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

simMD <- function(subjects = 15, readers = 20, mu = 0, sigma2A = 0.1, sigma2B = 0.1, sigma2E = 0.5) {

  SigmaA <- sigma2A * kronecker(diag(subjects), matrix(1, nrow = readers, ncol = readers))
  SigmaB <- sigma2B * kronecker(diag(readers), matrix(1, nrow = subjects, ncol = subjects))
  SigmaE <- sigma2E * diag(subjects * readers)

  Sigma  <- SigmaA + SigmaB + SigmaE

  values <- MASS::mvrnorm(1, mu = rep(mu, subjects * readers), Sigma = Sigma)

  dat <- tibble(subject = rep(1:subjects, each = readers),
                reader  = rep(1:readers, times = subjects),
                value   = values)
  return(dat)
}

