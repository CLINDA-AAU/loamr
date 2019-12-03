
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
  gather(observer, value, -subject)
```

Ww can calculate the Limit of Agreements and confidence intervals

``` r
LOAM(Jones)
#> Limits of agreement with the mean for multiple observers
#> 
#> The data has 195 observations from 39 individuals by 5 observers with 1 measurements
#> 
#> LoAM +/-: (95% CI)
#> 
#> Symmetric CI:  1.143 (1.340, 0.946)
#> Asymmetric CI: 1.143 (1.831, 1.020)
#> 
#> ICC:           0.837 (0.744, 0.904)
#> SigmaB:        0.287 (0.169, 0.404)
#> SigmaE:        0.585
```

The S3 class includes a generic plotting function made with ggplot2

``` r
grid.arrange(plot(LOAM(Jones), CI="sym"), plot(LOAM(Jones), CI="asym"), nrow=1)
```

![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->

## Simulation of Data

The package also includes a function to simulate data (which is already
in long format)

``` r
sim <- simMD(subjects=80, observers=4)
LOAM(sim)
#> Limits of agreement with the mean for multiple observers
#> 
#> The data has 320 observations from 80 individuals by 4 observers with 1 measurements
#> 
#> LoAM +/-: (95% CI)
#> 
#> Symmetric CI:  1.176 (1.286, 1.066)
#> Asymmetric CI: 1.176 (1.500, 1.082)
#> 
#> ICC:           0.252 (0.143, 0.376)
#> SigmaB:        0.128 (0.078, 0.177)
#> SigmaE:        0.681
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
