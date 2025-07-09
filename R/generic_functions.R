#' Print output from 'LOAM' function
#'
#' @param x an object of class 'loamobject'
#' @param digits number of digits shown in printed output
#' @param ... passthrough
#'
#' @export
#'

print.loamobject <- function(x, digits = 3, ...) {
  fm <- function(x) {format(round(x, digits), nsmall = digits)}

  cat("Limits of agreement with the mean for multiple observers", sep = "")
  cat("\n\n")
  cat("The data has", nrow(x$data),"observations from", length(unique(x$data$subject)),
      "individuals by", length(unique(x[[1]]$observer)), "observers with", length(unique(x[[1]]$measurement)),"repeated measurements")
  cat("\n\n")
  cat("95% reproducibility LOAM:  +/- ", fm(x$estimates$LOAM_reprod), " (", fm(x$intervals$reprod_CI[1]), ", ", fm(x$intervals$reprod_CI[2]), ")", sep = "")
  if(!(x$estimates$LOAM_repeat == "-")){
    cat("\n")
    cat("95% repeability LOAM:      +/- ", fm(x$estimates$LOAM_repeat), " (", fm(x$intervals$repeat_CI[1]), ", ", fm(x$intervals$repeat_CI[2]), ")", sep = "")
  }
  cat("\n\n")
  cat("sigmaA:    ", fm(x$estimates$sigmaA),  " (", fm(x$intervals$sigmaA_CI[1]),  ", ", fm(x$intervals$sigmaA_CI[2]),  ")", sep = "")
  cat("\n")
  cat("sigmaB:    ", fm(x$estimates$sigmaB),  " (", fm(x$intervals$sigmaB_CI[1]),  ", ", fm(x$intervals$sigmaB_CI[2]),  ")", sep = "")
  if(!(x$estimates$sigmaAB == "-")){
    cat("\n")
    cat("sigmaAB:   ", fm(x$estimates$sigmaAB), " (", fm(x$intervals$sigmaAB_CI[1]), ", ", fm(x$intervals$sigmaAB_CI[2]), ")", sep = "")
  }
  cat("\n")
  cat("sigmaE:    ", fm(x$estimates$sigmaE),  " (", fm(x$intervals$sigmaE_CI[1]),  ", ", fm(x$intervals$sigmaE_CI[2]),  ")", sep = "")
  cat("\n")
  if (!(x$estimates$ICC == "-")) {
    cat("ICC(A,1):  ", fm(x$estimates$ICC), " (", fm(x$intervals$ICC_CI[1]), ", ", fm(x$intervals$ICC_CI[2]), ")", sep = "")
  }
  cat("\n\n")
  cat("Coverage probability for the above CIs: ", x$CI.coverage * 100,"%", sep="")
  cat("\n")
}

#' Plot output from 'LOAM' function
#'
#' @param x an object of class 'loamobject'
#' @param ... passthrough

#' @return An agreement plot with an estimate and CI for the 95\% reproducibility limits of agreement with the mean
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#'

plot.loamobject <- function(x, ...) {

  ci  <- x$intervals$reprod_CI
  est <- x$estimates$LOAM_reprod

  fm <- function(x) {format(round(x, 3), nsmall = 3)}
  k <- max(x$data$measurement)
  b <- length(unique(x$data$observer))

  if (b <= 6) {

    x[[1]] %>%
      mutate(centered = .data$value - .data$subjectMean) %>%
      ggplot(aes(x = .data$subjectMean, y = .data$centered, shape = as.factor(.data$observer))) +
      scale_shape_manual(values = 0:(b-1)) +
      geom_vline(aes(xintercept = .data$subjectMean), alpha = 0.1) +
      geom_hline(yintercept =  est, color = "#1f78b4", linetype = "dashed")+
      geom_hline(yintercept = -est, color = "#1f78b4", linetype = "dashed")+
      annotate("rect", ymin =  ci[1], ymax =  ci[2], xmin = -Inf, xmax = Inf, alpha = 0.2, fill = "#1f78b4") +
      annotate("rect", ymin = -ci[1], ymax = -ci[2], xmin = -Inf, xmax = Inf, alpha = 0.2, fill = "#1f78b4") +
      geom_point(size = 2, ...) +
      theme_bw() + theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank()) +
      labs(x = ifelse(k > 1, expression(italic(bar(y)[i..])), expression(italic(bar(y)[i.]))),
           y = ifelse(k > 1, expression(italic(y[ijk] - bar(y)[i..])), expression(italic(y[ij] - bar(y)[i.]))),
           title = "Agreement plot",
           subtitle = paste0("95% reproducbility LOAM +/- ", fm(est), " with ", x$CI.coverage*100, "% CI: (", fm(ci[1]),", ",fm(ci[2]),")"),
           shape = "Observer")

  } else {

#    message("Observers not illustrated as there is more than 6")

    x[[1]] %>%
      mutate(centered = .data$value - .data$subjectMean) %>%
      ggplot(aes(x = .data$subjectMean, y = .data$centered)) +
      geom_vline(aes(xintercept = .data$subjectMean), alpha=0.1) +
      geom_hline(yintercept =  est, color = "#1f78b4", linetype = "dashed")+
      geom_hline(yintercept = -est, color = "#1f78b4", linetype = "dashed")+
      annotate("rect", ymin = ci[1],  ymax =  ci[2], xmin = -Inf, xmax = Inf, alpha = 0.2, fill = "#1f78b4") +
      annotate("rect", ymin = -ci[1], ymax = -ci[2], xmin = -Inf, xmax = Inf, alpha = 0.2, fill = "#1f78b4") +
      geom_point(size = 2, alpha = 0.5, ...) +
      theme_bw() + theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank()) +
      labs(x = ifelse(k > 1, expression(italic(bar(y)[i..])), expression(italic(bar(y)[i.]))),
           y = ifelse(k > 1, expression(italic(y[ijk] - bar(y)[i..])), expression(italic(y[ij] - bar(y)[i.]))),
           title = "Agreement plot",
           subtitle = paste0("95% reproducbility LOAM +/- ", fm(est), " with ", x$CI.coverage*100, "% CI: (", fm(ci[1]),", ",fm(ci[2]),")"),
           shape = "Observer")
  }
}
