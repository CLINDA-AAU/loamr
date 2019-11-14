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
  cat("The data has", nrow(x[[1]]),"measurements from", length(unique(x[[1]]$lesion)),
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

  x[[1]] %>%
    mutate(centered = value - lesionMean) %>%
    ggplot(aes(x=lesionMean, y=centered, shape=reader)) +
    geom_vline(aes(xintercept=lesionMean), alpha=0.1) +
    geom_hline(yintercept=x[[2]][1], color="red", linetype="dashed")+
    geom_hline(yintercept=x[[2]][2], color="red", linetype="dashed")+
    annotate("rect",ymin=x[[2]][1]-0.34, ymax=x[[2]][1]+0.34, xmin=-Inf, xmax=Inf, alpha=0.1, fill="red") +
    annotate("rect",ymin=x[[2]][2]-0.34, ymax=x[[2]][2]+0.34, xmin=-Inf, xmax=Inf, alpha=0.1, fill="red") +
    geom_point(size=2) +
    theme_bw() + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank()) +
    labs(x=expression(bar(y)[.j]), y=expression(y[ij] - bar(y)[.j]), title="Bland-Altman plot")
}
