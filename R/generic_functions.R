#' Adding to the generic print function
#'
#' @param x The loamobject
#' @param ... passthrough
#'
#' @return a plot
#' @export
#'

print.loamobject <- function(x, ...) {

  fm <- function(x) {format(round(x, 3), nsmall = 3)}

  cat("Limits of agreement with the mean for multiple observers", sep = "")
  cat("\n\n")
  cat("The data has", nrow(x$data),"observations from", length(unique(x$data$subject)),
      "individuals by", length(unique(x[[1]]$observer)), "observers with", length(unique(x[[1]]$measurement)),"measurements")
  cat("\n\n")
  cat("LoAM: +/-      ", fm(x$estimates$LoAM), sep = "")
  cat("\n")
  cat("Symmetric CI:        (", fm(x$intervals$LoAM_CI_sym[1]), ", ", fm(x$intervals$LoAM_CI_sym[2]), ")", sep = "")
  cat("\n")
  cat("Asymmetric CI:       (", fm(x$intervals$LoAM_CI_asym[1]), ", ", fm(x$intervals$LoAM_CI_asym[2]), ")", sep = "")
  cat("\n\n")
  if (!is.na(x$estimates$ICC)) {
  cat("ICC:           ", fm(x$estimates$ICC),           " (", fm(x$intervals$ICC_CI[1]), ", ", fm(x$intervals$ICC_CI[2]), ")", sep = "")
  cat("\n")
  }
  cat("sigmaB:        ", fm(x$estimates$sigmaB),        " (", fm(x$intervals$sigmaB_CI[1]), ", ", fm(x$intervals$sigmaB_CI[2]), ")", sep = "")
  cat("\n")
  cat("sigmaE:        ", fm(x$estimates$sigmaE), sep = "")
  cat("\n\n")
  cat("Confidence interval in brackets:", x$CI * 100,"%")
  cat("\n")
}

#' Adding to the generic plot function
#'
#' @param x The loamobject
#' @param ... passthrough
#' @param CItype confidence interval type: "sym" or "asym"
#'
#' @return a plot
#' @import ggplot2
#' @export
#'

plot.loamobject <- function(x, CItype = "sym", ...) {

  if (CItype == "sym") {
      ci <- x$intervals$LoAM_CI_sym
      name <- "symmetric"
  } else if (CItype == "asym") {
      ci <- x$intervals$LoAM_CI_asym
      name <- "asymmetric"
  } else {
      stop("'CItype' needs to be 'sym' or 'asym'")
  }

  fm <- function(x) {format(round(x, 3), nsmall = 3)}


  if (length(unique(x$data$observer)) <= 6) {

    x[[1]] %>%
    mutate(centered = value - subjectMean) %>%
    ggplot(aes(x = subjectMean, y = centered, shape = as.factor(observer))) +
    geom_vline(aes(xintercept = subjectMean), alpha = 0.1) +
    geom_hline(yintercept =  x$estimates$LoAM, color = "#1f78b4", linetype = "dashed")+
    geom_hline(yintercept = -x$estimates$LoAM, color = "#1f78b4", linetype = "dashed")+
    annotate("rect", ymin = ci[1],  ymax =  ci[2], xmin = -Inf, xmax = Inf, alpha = 0.2, fill = "#1f78b4") +
    annotate("rect", ymin = -ci[1], ymax = -ci[2], xmin = -Inf, xmax = Inf, alpha = 0.2, fill = "#1f78b4") +
    geom_point(size = 2) +
    theme_bw() + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank()) +
    labs(x = expression(italic(bar(y)[.j])),
         y = expression(italic(y[ij] - bar(y)[.j])),
         title = "Agreement plot",
         subtitle = paste0("LoAM +/- ", fm(x$estimates$LoAM), "   ", x$CI*100, "% ", name, " CI (", fm(ci[1])," ",fm(ci[2]),")"),
         shape = "Observer")

  } else {

    message("Observers not illustrated as there is more than 6")

    x[[1]] %>%
      mutate(centered = value - subjectMean) %>%
      ggplot(aes(x = subjectMean, y = centered)) +
      geom_vline(aes(xintercept=subjectMean), alpha=0.1) +
      geom_hline(yintercept =  x$estimates$LoAM, color = "#1f78b4", linetype = "dashed")+
      geom_hline(yintercept = -x$estimates$LoAM, color = "#1f78b4", linetype = "dashed")+
      annotate("rect", ymin = ci[1],  ymax =  ci[2], xmin = -Inf, xmax = Inf, alpha = 0.2, fill = "#1f78b4") +
      annotate("rect", ymin = -ci[1], ymax = -ci[2], xmin = -Inf, xmax = Inf, alpha = 0.2, fill = "#1f78b4") +
      geom_point(size=2, alpha=0.5) +
      theme_bw() + theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank()) +
      labs(x = expression(italic(bar(y)[.j])),
           y = expression(italic(y[ij] - bar(y)[.j])),
           title = "Agreement plot",
           subtitle = paste0("LoAM +/- ", fm(x$estimates$LoAM), "   ", x$CI*100, "% ", name, " CI (", fm(ci[1])," ",fm(ci[2]),")"),
           shape = "Observer")
  }
}
