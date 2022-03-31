
# mcpwr

<!-- badges: start -->

<!-- badges: end -->

The goal of mcpwr is to perform Monte Carlo power calculations

## Installation

You can install the development version of mcpwr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("TJMurphy/mcpwr")
```

## Example

The poisdm function is a datamaker to simulate groups of
Poisson-distributed count data. The plot argument allows users to print
a histogram and jitterplot of the output.

``` r
library(mcpwr)

# generate data and graphs
# for group 1 (group of 40 measurements with a mean of 10)
# and group 2 (group of 50 with measurements with a mean of 12)

poisdm(c(40, 50), c(10, 12), c("group1", "group2"), TRUE)
```

<img src="man/figures/README-example-1.png" width="100%" /><img src="man/figures/README-example-2.png" width="100%" />

    #> $param
    #> $param$groupnames
    #> [1] "group1" "group2"
    #> 
    #> $param$n
    #> [1] 40 50
    #> 
    #> $param$lambda
    #> [1] 10 12
    #> 
    #> 
    #> $simdata
    #>     group1 group2
    #> V1       8      8
    #> V2      11     10
    #> V3       8     14
    #> V4       9      6
    #> V5      11     13
    #> V6      11     14
    #> V7       8     11
    #> V8       8     13
    #> V9       9      9
    #> V10     11     14
    #> V11      4     19
    #> V12     13     16
    #> V13      7     11
    #> V14     16     12
    #> V15     16     13
    #> V16      8      7
    #> V17     10     15
    #> V18      6     15
    #> V19      8     17
    #> V20      2     16
    #> V21     11     15
    #> V22      6     13
    #> V23     12     11
    #> V24     11     16
    #> V25     12     13
    #> V26     10     14
    #> V27     10     13
    #> V28     12      6
    #> V29     12     16
    #> V30      6     13
    #> V31     10     14
    #> V32      9     16
    #> V33     14      8
    #> V34     13     15
    #> V35      7      9
    #> V36      9      9
    #> V37      9     17
    #> V38      9     11
    #> V39     15     17
    #> V40      7     13
    #> V41     NA      8
    #> V42     NA      9
    #> V43     NA     11
    #> V44     NA      7
    #> V45     NA     15
    #> V46     NA     12
    #> V47     NA     16
    #> V48     NA     12
    #> V49     NA     18
    #> V50     NA      6
    #> 
    #> $distrib
    #> [1] "poisson"

Input parameters are listed as $param and simmulated data as $simdata
