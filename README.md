
<!-- README.md is generated from README.Rmd. Please edit that file -->

# loamr

loamr is an R package to …

## Installation

loamr may be installed using the following command

``` r
devtools::install_github("HaemAalborg/loamr")
```

# Overview

The package includes some data from Jones, this is in wide format, so we
transform this to a long format as the ‘LOAM’ function uses:

``` r
library(loamr)
data(Jones)
head(Jones)
#>   Reader1 Reader2 Reader3 Reader4 Reader5
#> 1     3.8     3.8     3.8     3.9     3.9
#> 2     2.2     2.0     2.8     2.4     2.1
#> 3     1.5     1.2     2.2     2.3     2.0
#> 4     3.8     4.0     4.8     3.7     4.2
#> 5     3.5     3.3     4.8     3.6     3.9
#> 6     4.2     4.0     4.8     4.1     4.2

Jones <- Jones %>%
  mutate(subject = 1:nrow(.)) %>%
  gather(reader, value, -subject)
```

Ww can calculate the Limit of Agreements and confidence intervals

``` r
LOAM(Jones)
#> Output of this type of function... somthing
#> 
#> The data has 195 measurements from 39 individuals by 5 readers
#> 
#> Jones' LoAM
#> [1] -1.14755  1.14755
#> 
#> Borgbjerg's LoAM
#> [1] -1.142867  1.142867
#> 
#> Standard Error: 0.34
#> Maybe some text here also...
```

The S3 class includes a generic plotting function made with ggplot2

``` r
plot(LOAM(Jones))
```

![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->

## Simulation of Data

The package also includes a function to simulate data (which is already
in long format)

``` r
sim <- simMD(subjects=80, readers=4)
LOAM(sim)
#> Output of this type of function... somthing
#> 
#> The data has 320 measurements from 80 individuals by 4 readers
#> 
#> Jones' LoAM
#> [1] -1.334906  1.334906
#> 
#> Borgbjerg's LoAM
#> [1] -1.176248  1.176248
#> 
#> Standard Error: 0.3601518
#> Maybe some text here also...
```

We can plot this, and as the plotting is done by ggplot2 it’s easy to
change elements of the plot, like the title:

``` r
plot(LOAM(sim), CI="asym") + labs(title="Simulated Data")
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

## References

1.  Jones’
2.  Borgbjerg’s
