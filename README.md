
<!-- README.md is generated from README.Rmd. Please edit that file -->

# loamr: limits of agreement with the mean

`loamr` is an `R` package for performing agreement analysis on
continuous measurements made by multiple observers. The package provides
functions for making agreement plots and for calculating the estimate
and CI for the limits of agreement with the mean proposed by Christensen
et al. (2020).

## Installation

`loamr` can be installed using the following command:

``` r
devtools::install_github("HaemAalborg/loamr")
```

## Example

The package includes a function to simulate data from the two-way random
effects model described in Christensen et al. (2020):

``` r
sim <- simMD(mu = 5)
head(sim)
#> # A tibble: 6 × 3
#>   subject observer value
#>     <int>    <int> <dbl>
#> 1       1        1  4.36
#> 2       1        2  4.72
#> 3       1        3  3.99
#> 4       1        4  5.28
#> 5       1        5  5.26
#> 6       1        6  3.67
```

Estimate and CI for the limits of agreements with the mean:

``` r
LOAM(sim)
#> Limits of agreement with the mean for multiple observers
#> 
#> The data has 300 observations from 15 individuals by 20 observers with 1 measurements
#> 
#> LOAM:  +/- 1.387 (1.283, 1.549)
#> 
#> sigmaA:    0.322 (0.176, 0.469)
#> sigmaB:    0.235 (0.117, 0.352)
#> sigmaE:    0.687 (0.633, 0.751)
#> ICC(A,1):  0.164 (0.075, 0.360)
#> 
#> Coverage probability for the above CIs: 95%
```

The S3 class includes a generic plotting function made with `ggplot2`
for making an agreement plot with indication of estimate and CI for the
limits of agreement with the mean:

``` r
plot(LOAM(sim))
```

![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->

Elements of the plot is easily changed using functionalities from
`ggplot2`. For example, changing the title:

``` r
plot(LOAM(sim)) + ggplot2::labs(title = "Simulated Data")
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

## References

1.  Christensen, H. S., Borgbjerg, J., Børty, L., and Bøgsted, M. (2020)
    “On Jones et al.’s method for extending Bland-Altman plots to limits
    of agreement with the mean for multiple observers”. BMC Medical
    Research Methodology. <https://doi.org/10.1186/s12874-020-01182-w>
