#' Adding to the generic print function
#'
#' @param x The loamobject
#' @param ... passthrough
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
  cat("LoAM +/-: (", x$parts$CI * 100, "% CI)", sep = "")
  cat("\n\n")
  cat("Symmetric CI:  ", fm(x$intervals$B.LoAM[2]), " (", fm(x$estimates$uupper[1]),    ", ", fm(x$estimates$lupper[1]), ")", sep = "")
  cat("\n")
  cat("Asymmetric CI: ", fm(x$intervals$B.LoAM[2]), " (", fm(x$estimates$uupper[2]),    ", ", fm(x$estimates$lupper[2]), ")", sep = "")
  cat("\n\n")
  cat("ICC:           ", fm(x$parts$ICC),           " (", fm(x$intervals$ICC_CI[1]),    ", ", fm(x$intervals$ICC_CI[2]), ")", sep = "")
  cat("\n")
  cat("SigmaB:        ", fm(x$parts$sigmaB),        " (", fm(x$intervals$sigmaB_CI[1]), ", ", fm(x$intervals$sigmaB_CI[2]), ")", sep = "")
  cat("\n")
  cat("SigmaE:        ", fm(x$parts$sigmaE), sep = "")
  cat("\n")
}


#' Adding to the generic plot function
#'
#' @param x The loamobject
#' @param ... passthrough
#' @param CI confidence interval type,
#' @return a plot
#' @import ggplot2
#' @export
#'

plot.loamobject <- function(x, CI = "sym", ...) {

  if (CI == "sym") {
      ci <- x$estimates[1,]
  } else if (CI == "asym") {
      ci <- x$estimates[2,]
  } else {
      stop("use 'sym' og 'asym' in confidence interval")
    }


  if (length(unique(x$data$observer)) <= 6) {

    x[[1]] %>%
    mutate(centered = value - subjectMean) %>%
    ggplot(aes(x = subjectMean, y = centered, shape = as.factor(observer))) +
    geom_vline(aes(xintercept=subjectMean), alpha=0.1) +
    geom_hline(yintercept=ci$upper, color="#1f78b4", linetype="dashed")+
    geom_hline(yintercept=ci$lower, color="#1f78b4", linetype="dashed")+
    annotate("rect",ymin=ci$lupper, ymax=ci$uupper, xmin=-Inf, xmax=Inf, alpha=0.2, fill="#1f78b4") +
    annotate("rect",ymin=ci$llower, ymax=ci$ulower, xmin=-Inf, xmax=Inf, alpha=0.2, fill="#1f78b4") +
    geom_point(size=2) +
    theme_bw() + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank()) +
    labs(x=expression(italic(bar(y)[.j])), y=expression(italic(y[ij] - bar(y)[.j])),
         title="Bland-Altman plot", subtitle=paste0(ci$name, " LoAM: ", round(ci$lower,2),"/",
                                                    round(ci$upper,2)),
         shape = "Observer")

  } else {

    x[[1]] %>%
      mutate(centered = value - subjectMean) %>%
      ggplot(aes(x = subjectMean, y = centered)) +
      geom_vline(aes(xintercept=subjectMean), alpha=0.1) +
      geom_hline(yintercept=ci$upper, color="#1f78b4", linetype="dashed")+
      geom_hline(yintercept=ci$lower, color="#1f78b4", linetype="dashed")+
      annotate("rect",ymin=ci$lupper, ymax=ci$uupper, xmin=-Inf, xmax=Inf, alpha=0.2, fill="#1f78b4") +
      annotate("rect",ymin=ci$llower, ymax=ci$ulower, xmin=-Inf, xmax=Inf, alpha=0.2, fill="#1f78b4") +
      geom_point(size=2, alpha=0.5) +
      theme_bw() + theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank()) +
      labs(x = expression(italic(bar(y)[.j])), y = expression(italic(y[ij] - bar(y)[.j])), title = "Bland-Altman plot",
           subtitle = paste0(ci$name, " LoAM: ", round(ci$lower,2),"/", round(ci$upper,2), " with ",
                           "\n(Observers not illustrated as there is more than 6)"))
  }
}
