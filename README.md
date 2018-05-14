
<!-- README.md is generated from README.Rmd. Please edit that file -->
portopt
=======

Tools for simple portfolio optimization:

-   Minimum variance portfolio for a given expected return, with no limits on allocations for individual asset classes other than that they must sum to 1. (That is, shorting and leverage are allowed.)

-   Minimum variance portfolio with lower and upper bounds on allocations for asset classes. It can also accommodate more-general linear constraints on asset classes. (For example, stocks plus bonds combined must be between 40% and 60% of the portfolio.)

-   Efficient frontier

`portopt` cannot handle nonlinear constraints on asset allocations, and it cannot find an optimal portfolio that satisfies more-complex objectives, such as value-at-risk criteria. For my purposes, so far, these are not necessary. `portopt` uses `solve.QP` from the package `quadprog` for minimum-variance optimization. Other solvers would be required for more-complex optimization.

I wrote `portopt` because I was looking for basic portfolio optimization tools in `R`. There are many available packages, but the package that looked like it would be most useful, `PortfolioAnalytics`, recently was removed from `CRAN`. At some point I may trade up to one of the more-sophisticated portfolio optimization packages available on `CRAN`.

portopt data sets
-----------------

The package includes several data sets with expected returns, standard deviations, and a correlation matrix for multiple asset classes. These data sets are used in the examples below. Each data set is a list with two elements:

-   `ersd` - a data frame with columns class (the asset class), er (expected return), and sd (standard deviation)
-   `cormat` - a correlation matrix for these assets. Thus, the number of rows and number of columns equal the number of asset classes. The row names and column names are the asset-class names.

The data sets are:

-   `stalebrink`
-   `rvk`
-   `horizon10year2017`

The examples below use these datasets.

Installation
------------

Install as follows:

``` r
devtools::install_github("donboyd5/portopt")
```

Examples
--------

### Get basic information about a dataset

This is a basic example which shows you how to solve a common problem:

``` r
library("portopt")
library("magrittr")
library("tidyverse")
#> -- Attaching packages ----------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --
#> v ggplot2 2.2.1     v purrr   0.2.4
#> v tibble  1.4.2     v dplyr   0.7.4
#> v tidyr   0.8.0     v stringr 1.3.1
#> v readr   1.1.1     v forcats 0.3.0
#> -- Conflicts -------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
#> x tidyr::extract()   masks magrittr::extract()
#> x dplyr::filter()    masks stats::filter()
#> x dplyr::lag()       masks stats::lag()
#> x purrr::set_names() masks magrittr::set_names()

stalebrink
#> $ersd
#> # A tibble: 6 x 3
#>   class          er     sd
#>   <chr>       <dbl>  <dbl>
#> 1 stocks     0.148  0.249 
#> 2 bonds.dom  0.058  0.0751
#> 3 bonds.intl 0.0577 0.116 
#> 4 re         0.103  0.0626
#> 5 alts       0.0738 0.0734
#> 6 cash       0.0377 0.0308
#> 
#> $cormat
#>            stocks bonds.dom bonds.intl    re  alts  cash
#> stocks       1.00      0.05       0.22  0.05  0.13 -0.08
#> bonds.dom    0.05      1.00       0.42 -0.28 -0.04  0.29
#> bonds.intl   0.22      0.42       1.00 -0.37 -0.04  0.22
#> re           0.05     -0.28      -0.37  1.00 -0.06  0.33
#> alts         0.13     -0.04      -0.04 -0.06  1.00 -0.10
#> cash        -0.08      0.29       0.22  0.33 -0.10  1.00
aa.wts <- c(.3, .2, .1, .15, .1, .05) # asset-allocation weights
per(stalebrink$ersd$er, aa.wts) # portfolio expected return with these weights
#> [1] 0.09626111
psd(stalebrink$cormat, stalebrink$ersd$sd, aa.wts) # portfolio standard deviation with these weights
#> [1] 0.09136952
```

### Get the minimum variance portfolio for a given expected return

This is a basic example which shows you how to solve a common problem:

``` r
# Create several minimum-variance portfolios

# no restrictions on asset allocation - shorting and leverage allowed
minvport(.09, stalebrink$ersd, stalebrink$cormat)$portfolio
#> Loading required package: quadprog
#> # A tibble: 6 x 6
#>   class          er     sd asset.weight   per    psd
#>   <chr>       <dbl>  <dbl>        <dbl> <dbl>  <dbl>
#> 1 stocks     0.148  0.249       0.00276  0.09 0.0356
#> 2 bonds.dom  0.058  0.0751      0.209    0.09 0.0356
#> 3 bonds.intl 0.0577 0.116       0.123    0.09 0.0356
#> 4 re         0.103  0.0626      0.569    0.09 0.0356
#> 5 alts       0.0738 0.0734      0.226    0.09 0.0356
#> 6 cash       0.0377 0.0308     -0.130    0.09 0.0356

# shorting and leverage NOT allowed:
minvport(.09, stalebrink$ersd, stalebrink$cormat, 0, 1)$portfolio
#> # A tibble: 6 x 6
#>   class          er     sd asset.weight   per    psd
#>   <chr>       <dbl>  <dbl>        <dbl> <dbl>  <dbl>
#> 1 stocks     0.148  0.249        0.0323  0.09 0.0374
#> 2 bonds.dom  0.058  0.0751       0.119   0.09 0.0374
#> 3 bonds.intl 0.0577 0.116        0.0886  0.09 0.0374
#> 4 re         0.103  0.0626       0.585   0.09 0.0374
#> 5 alts       0.0738 0.0734       0.175   0.09 0.0374
#> 6 cash       0.0377 0.0308       0       0.09 0.0374

# shorting and leverage NOT allowed, 40% upper bound on real estate:
minvport(.09, stalebrink$ersd, stalebrink$cormat, 0, c(1, 1, 1, .4, 1, 1))$portfolio
#> # A tibble: 6 x 6
#>   class          er     sd asset.weight   per    psd
#>   <chr>       <dbl>  <dbl>        <dbl> <dbl>  <dbl>
#> 1 stocks     0.148  0.249        0.101   0.09 0.0437
#> 2 bonds.dom  0.058  0.0751       0.157   0.09 0.0437
#> 3 bonds.intl 0.0577 0.116        0.0330  0.09 0.0437
#> 4 re         0.103  0.0626       0.40    0.09 0.0437
#> 5 alts       0.0738 0.0734       0.310   0.09 0.0437
#> 6 cash       0.0377 0.0308       0       0.09 0.0437
```

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />
