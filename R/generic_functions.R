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
  cat("The data has", nrow(x[[1]]),"measurements from", length(unique(x[[1]]$subject)),
      "individuals by", length(unique(x[[1]]$reader)), "readers")
  cat("\n\n")
  cat("Limits of Agreements 2 methods with", 0.95,"% CI")
  cat("\n\n")
  print(x[[6]])
  cat("\n")
  cat("Standard Error:",x[[4]])
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
      ci <- x[[6]][1,]
  } else if (CI == "asym") {
      ci <- x[[6]][2,]
  } else {
      stop("use 'sym' og 'asym' in confidence interval")
    }


  x[[1]] %>%
    mutate(centered = value - subjectMean) %>%
    ggplot(aes(x=subjectMean, y=centered, shape=as.factor(reader))) +
    geom_vline(aes(xintercept=subjectMean), alpha=0.1) +
    geom_hline(yintercept=ci$upper, color="#1f78b4", linetype="dashed")+
    geom_hline(yintercept=ci$lower, color="#1f78b4", linetype="dashed")+
    annotate("rect",ymin=ci$lupper, ymax=ci$uupper, xmin=-Inf, xmax=Inf, alpha=0.2, fill="#1f78b4") +
    annotate("rect",ymin=ci$llower, ymax=ci$ulower, xmin=-Inf, xmax=Inf, alpha=0.2, fill="#1f78b4") +
    geom_point(size=2) +
    theme_bw() + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank()) +
    labs(x=expression(bar(y)[.j]), y=expression(y[ij] - bar(y)[.j]),
         title="Bland-Altman plot", subtitle=paste0(ci$name, " LoAM: ", round(ci$lower,2),"/", round(ci$upper,2), " with ", round(ci$outliers,2), "% outliers"),
         shape = "Readers")
}
