#' Adding to the generic print function
#'
#' @param x The loamobject
#' @param ... passthrough
#' @return a plot
#' @export
#'

print.loamobject <- function(x, ...) {

  cat("Output of this type of function... somthing")
  cat("\n\n")
  cat("The data has", nrow(x$data),"measurements from", length(unique(x$data$subject)),
      "individuals by", length(unique(x[[1]]$reader)), "readers")
  cat("\n\n")
  cat("Limits of Agreements 2 methods with", x$parts$CI,"% CI")
  cat("\n\n")
  print(x$estimates)
  cat("\n")
  cat("Standard Error:",x$parts$SE1, " ",x$parts$SE2)
  cat("\n")
  cat("Maybe some text here also...\n")
}


#' Adding to the generic plot function
#'
#' @param x The loamobject
#' @param ... passthrough
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
    labs(x=expression(bar(y)[.j]), y=expression(y[ij] - bar(y)[.j]),
         title="Bland-Altman plot", subtitle=paste0(ci$name, " LoAM: ", round(ci$lower,2),"/",
                                                    round(ci$upper,2), " with ", round(ci$outliers,2), "% outliers"),
         shape = "Observers")

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
      labs(x = expression(bar(y)[.j]), y = expression(y[ij] - bar(y)[.j]), title = "Bland-Altman plot",
           subtitle = paste0(ci$name, " LoAM: ", round(ci$lower,2),"/", round(ci$upper,2), " with ", round(ci$outliers,2),
                           "% outliers\n(Observers not illustrated as there is more than 6)"))
  }
}
