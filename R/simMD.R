#' Simulates measurement data
#'
#' @description This function simulates measurement data
#'
#' @details The function returns a dataframe
#'
#' @param subjects A dataframe,
#' @param observers A dataframe,
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

simMD <- function(subjects = 15, observers = 20, mu = 0, sigma2A = 0.1, sigma2B = 0.1, sigma2E = 0.5) {

  SigmaA <- sigma2A * kronecker(diag(subjects), matrix(1, nrow = observers, ncol = observers))
  SigmaB <- sigma2B * kronecker(diag(observers), matrix(1, nrow = subjects, ncol = subjects))
  SigmaE <- sigma2E * diag(subjects * observers)

  Sigma  <- SigmaA + SigmaB + SigmaE

  values <- MASS::mvrnorm(1, mu = rep(mu, subjects * observers), Sigma = Sigma)

  dat <- tibble(subject = rep(1:subjects, each = observers),
                observer  = rep(1:observers, times = subjects),
                value   = values)
  return(dat)
}



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
#' simMD2()
#'
#' @export
#' @importFrom MASS mvrnorm
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#'
simMD2 <- function(subjects = 15, observers = 20, measurements = 3, mu = 0, sigma2A = 0.1, sigma2B = 0.1, sigma2E = 0.5) {

  l <- list(NA)

  for (i in 1:measurements) {

  SigmaA <- sigma2A * kronecker(diag(subjects), matrix(1, nrow = observers, ncol = observers))
  SigmaB <- sigma2B * kronecker(diag(observers), matrix(1, nrow = subjects, ncol = subjects))
  SigmaE <- sigma2E * diag(subjects * observers)

  Sigma  <- SigmaA + SigmaB + SigmaE

  values <- MASS::mvrnorm(1, mu = rep(mu, subjects * observers), Sigma = Sigma)

  dat <- tibble(subject     = rep(1:subjects, each = observers),
                observer    = rep(1:observers, times = subjects),
                value       = values)
  l[[i]] <- dat
  }

  data <- bind_rows(l, .id = "measurement")
  return(data)
}

simMD2()













