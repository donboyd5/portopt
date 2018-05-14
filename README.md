
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
-   `horizon2017`

The examples below use these datasets.

Installation
------------

Install as follows:

``` r
devtools::install_github("donboyd5/portopt")
```

Examples
--------

### Get the minimum variance portfolio for a given expected return

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub!
