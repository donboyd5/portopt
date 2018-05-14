
#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr

library("hms") # hms, for times.
library("stringr") # stringr, for strings.
library("lubridate") # lubridate, for date/times.
library("forcats") # forcats, for factors.
library("readxl") # readxl, for .xls and .xlsx files.
library("haven") # haven, for SPSS, SAS and Stata files.

library("grDevices")
library("knitr")

library("zoo") # for rollapply

library("btools") # library that I created (install from github)
library("bdata")
library("BEAData")
library("pdata")
library("fof")
library("apitools")


# Note that nearPD in package Matrix will calculate a nearest positive definite matrix
# https://stat.ethz.ch/R-manual/R-devel/library/Matrix/html/nearPD.html
# It implements the algorithm of 
# Higham, Nick (2002) Computing the nearest correlation matrix - a problem from finance;
# IMA Journal of Numerical Analysis 22, 329–343.

library("Matrix")

library("quadprog")
