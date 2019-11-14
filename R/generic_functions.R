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
  cat("Jones' LoAM")
  cat("\n")
  print(x[[2]])
  cat("\n")
  cat("Borgbjerg's LoAM")
  cat("\n")
  print(x[[3]])
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
      CItype <- x[[4]]
      CIname <- "Bjornborg"
  } else if (CI == "asym") {
      CItype <- x[[5]]
      CIname <- "Jones"
  } else {
      stop("use 'sym' og 'asym' in confidence interval")
    }

  x[[1]] %>%
    mutate(centered = value - subjectMean) %>%
    ggplot(aes(x=subjectMean, y=centered, shape=as.factor(reader))) +
    geom_vline(aes(xintercept=subjectMean), alpha=0.1) +
    geom_hline(yintercept=x[[2]][1], color="#1f78b4", linetype="dashed")+
    geom_hline(yintercept=x[[2]][2], color="#1f78b4", linetype="dashed")+
    annotate("rect",ymin=x[[2]][1]-CItype, ymax=x[[2]][1]+CItype, xmin=-Inf, xmax=Inf, alpha=0.2, fill="#1f78b4") +
    annotate("rect",ymin=x[[2]][2]-CItype, ymax=x[[2]][2]+CItype, xmin=-Inf, xmax=Inf, alpha=0.2, fill="#1f78b4") +
    geom_point(size=2) +
    theme_bw() + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank()) +
    labs(x=expression(bar(y)[.j]), y=expression(y[ij] - bar(y)[.j]),
         title="Bland-Altman plot", subtitle=paste0(CIname, " LOAM: ",round(x[[2]],2), "with a 95% CI of ", round(CItype,2)),
         shape = "Readers")
}
