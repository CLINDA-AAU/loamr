#' Adding to the generic print function
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
      "individuals by", length(unique(x[[1]]$observer)), "observers with", length(unique(x[[1]]$measurement)),"measurements")
  cat("\n\n")
  cat("LOAM: +/-      ", fm(x$estimates$LOAM), sep = "")
  cat("\n")
  cat("Symmetric CI:", strrep(" ",nchar(fm(x$estimates$LOAM))+3),"(", fm(x$intervals$LOAM_CI_sym[1]), ", ", fm(x$intervals$LOAM_CI_sym[2]), ")", sep = "")
  cat("\n")
  cat("Asymmetric CI:", strrep(" ",nchar(fm(x$estimates$LOAM))+2),"(", fm(x$intervals$LOAM_CI_asym[1]), ", ", fm(x$intervals$LOAM_CI_asym[2]), ")", sep = "")
  cat("\n\n")
  cat("sigmaB:        ", fm(x$estimates$sigmaB), " (", fm(x$intervals$sigmaB_CI[1]), ", ", fm(x$intervals$sigmaB_CI[2]), ")", sep = "")
  cat("\n")
  cat("sigmaE:        ", fm(x$estimates$sigmaE), " (", fm(x$intervals$sigmaE_CI[1]), ", ", fm(x$intervals$sigmaE_CI[2]), ")", sep = "")
  cat("\n")
  if (!is.na(x$estimates$ICC)) {
    cat("ICC(A,1):      ", fm(x$estimates$ICC), " (", fm(x$intervals$ICC_CI[1]), ", ", fm(x$intervals$ICC_CI[2]), ")", sep = "")
  }
  cat("\n\n")
  cat("Confidence interval in brackets: ", x$CI * 100,"%", sep="")
  cat("\n")
}

#' Adding to the generic plot function
#'
#' @param x an object of class 'loamobject'
#' @param ... passthrough
#' @param CItype confidence interval type: symmetric ("sym") or asymmetric ("asym")
#'
#' @return an agreement plot with an estimate and CI for the limits of agreement with the mean
#' @import ggplot2
#' @importFrom rlang .data
#' @export
#'

plot.loamobject <- function(x, CItype = "asym", ...) {

  if (CItype == "sym") {
      ci <- x$intervals$LOAM_CI_sym
      name <- "symmetric"
  } else if (CItype == "asym") {
      ci <- x$intervals$LOAM_CI_asym
      name <- "asymmetric"
  } else {
      stop("'CItype' needs to be 'sym' or 'asym'")
  }

  fm <- function(x) {format(round(x, 3), nsmall = 3)}
  k <- max(x$data$measurement)

  if (length(unique(x$data$observer)) <= 6) {

    x[[1]] %>%
    mutate(centered = .data$value - .data$subjectMean) %>%
    ggplot(aes(x = .data$subjectMean, y = .data$centered, shape = as.factor(.data$observer))) +
    geom_vline(aes(xintercept = .data$subjectMean), alpha = 0.1) +
    geom_hline(yintercept =  x$estimates$LOAM, color = "#1f78b4", linetype = "dashed")+
    geom_hline(yintercept = -x$estimates$LOAM, color = "#1f78b4", linetype = "dashed")+
    annotate("rect", ymin = ci[1],  ymax =  ci[2], xmin = -Inf, xmax = Inf, alpha = 0.2, fill = "#1f78b4") +
    annotate("rect", ymin = -ci[1], ymax = -ci[2], xmin = -Inf, xmax = Inf, alpha = 0.2, fill = "#1f78b4") +
    geom_point(size = 2) +
    theme_bw() + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank()) +
    labs(x = ifelse(k > 1, expression(italic(bar(y)[i..])), expression(italic(bar(y)[i.]))),
         y = ifelse(k > 1, expression(italic(y[ijk] - bar(y)[i..])), expression(italic(y[ij] - bar(y)[i.]))),
         title = "Agreement plot",
         subtitle = paste0("LOAM +/- ", fm(x$estimates$LOAM), "   ", x$CI*100, "% ", name, " CI (", fm(ci[1])," ",fm(ci[2]),")"),
         shape = "Observer")

  } else {

    message("Observers not illustrated as there is more than 6")

    x[[1]] %>%
      mutate(centered = .data$value - .data$subjectMean) %>%
      ggplot(aes(x = .data$subjectMean, y = .data$centered)) +
      geom_vline(aes(xintercept = .data$subjectMean), alpha=0.1) +
      geom_hline(yintercept =  x$estimates$LOAM, color = "#1f78b4", linetype = "dashed")+
      geom_hline(yintercept = -x$estimates$LOAM, color = "#1f78b4", linetype = "dashed")+
      annotate("rect", ymin = ci[1],  ymax =  ci[2], xmin = -Inf, xmax = Inf, alpha = 0.2, fill = "#1f78b4") +
      annotate("rect", ymin = -ci[1], ymax = -ci[2], xmin = -Inf, xmax = Inf, alpha = 0.2, fill = "#1f78b4") +
      geom_point(size=2, alpha=0.5) +
      theme_bw() + theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank()) +
      labs(x = ifelse(k > 1, expression(italic(bar(y)[i..])), expression(italic(bar(y)[i.]))),
           y = ifelse(k > 1, expression(italic(y[ijk] - bar(y)[i..])), expression(italic(y[ij] - bar(y)[i.]))),
           title = "Agreement plot",
           subtitle = paste0("LOAM +/- ", fm(x$estimates$LOAM), "   ", x$CI*100, "% ", name, " CI (", fm(ci[1])," ",fm(ci[2]),")"),
           shape = "Observer")
  }
}
