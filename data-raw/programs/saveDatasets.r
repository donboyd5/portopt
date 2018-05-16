# Don Boyd
# 5/12/2018


#****************************************************************************************************
#       Setup ####
#****************************************************************************************************
library("devtools")

source("./data-raw/programs/libraries.r")
# source("./data-raw/programs/functions_ersd.r") # expected return and standard deviation
# source("./data-raw/programs/functions_optim_portfolio.r")


#****************************************************************************************************
#       Tools for creating datasets ####
#****************************************************************************************************
getcormat.lt <- function(lowtri.m){
  # get correlation matrix from a lower triangle that is already in matrix form

  cormat <- lowtri.m
  cormat[is.na(cormat)] <- 0
  cormat <- cormat + t(cormat) # fill the upper triangle
  diag(cormat) <- 1
  return(cormat)
}


#****************************************************************************************************
#                Get data - create ersd and the correlation matrix cormat ####
#****************************************************************************************************

getData <- function(fn, sheet){
  df <- read_excel(paste0("./data-raw/", fn), sheet=sheet, skip=2)

  ersd <- df %>%
    select(1:3) %>%
    mutate_at(vars(er, sd), funs(as.numeric))

  lowtri <- df %>%
    select(-c(1:4)) %>%
    select(class=1, everything())

  lowtri.m <- as.matrix(lowtri %>% select(-1))
  rownames(lowtri.m) <- lowtri$class
  # rownames(lowtri.m) == colnames(lowtri.m)
  # lowtri.m

  cormat <- getcormat.lt(lowtri.m)

  data <- list()
  data$ersd <- ersd
  data$cormat <- cormat
  return(data)
}

fn <- "CapitalMarketAssumptions(1).xlsx"
cmas <- c("stalebrink", "rvk", "horizon10year2017")

stalebrink <- getData(fn, "stalebrink")
stalebrink
use_data(stalebrink, overwrite = TRUE)

rvk <- getData(fn, "rvk")
rvk
use_data(rvk, overwrite = TRUE)

horizon10year2017 <- getData(fn, "horizon10year2017")
horizon10year2017
use_data(horizon10year2017, overwrite = TRUE)

