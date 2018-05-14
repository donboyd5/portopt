# Don Boyd
# 5/12/2018

usethis::use_build_ignore(c("data-raw"))
usethis::use_build_ignore(c("docs"))


#****************************************************************************************************
#       Setup ####
#****************************************************************************************************
source("./data-raw/programs/libraries.r")
source("./data-raw/programs/functions_ersd.r") # expected return and standard deviation
source("./data-raw/programs/functions_optim_portfolio.r")


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
















# minvport_bounds(.09, ersd, cormat, aa.lb=rep(0, nrow(ersd)), aa.ub=rep(1, nrow(ersd)))
plist <- rvk
minvport_bounds(.09, plist$ersd, plist$cormat)
minvport_bounds(.09, plist$ersd, plist$cormat, aa.lb=0, aa.ub=1)





#****************************************************************************************************
#                Check data ####
#****************************************************************************************************
covmat <- getcovmat(cormat, ersd$sd)
covmat

# Covariance matrix must be positive definite or else a variance can be negative
# Note that "A correlation matrix is a symmetric positive semidefinite matrix with unit diagonal" per
# Higham 2001 (http://eprints.ma.man.ac.uk/232/01/covered/MIMS_ep2006_70.pdf).

# verify that the covmat is positive definite
# if symmetric PD, then all eigen values are positive
eigen(cormat)$values
eigen(covmat)$values
cormat
covmat
chol(covmat)
diag(chol(covmat))
any(diag(chol(covmat)) <= 0) # if true, covmat is NOT PSD


#****************************************************************************************************
#                Adjust to positive definite ONLY IF NEEDED ####
#****************************************************************************************************
cormatadj <- nearPD(cormat, corr=TRUE)$mat

# verify that the adjusted covmat is positive definite
cormatadj
covmatadj <- getcovmat(cormatadj, ersd$sd)
covmatadj

eigen(cormatadj)$values
eigen(covmatadj)$values

# chol(covmatadj)
# diag(chol(covmatadj))
# any(diag(chol(covmatadj)) <= 0)

# compare the unadjusted and adjusted RVK cormats
cormat
cormatadj %>% round(., 2)
(cormatadj %>% round(., 2)) - cormat

cormat <- cormatadj
covmat <- covmatadj

