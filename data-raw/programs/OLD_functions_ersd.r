# ersd:  Functions related to expected return (er) and standard deviation (sd).


#**************************************************************************************************************
#     Functions for correlation and covariance matrices, and portfolio expected returns and standard deviations ####
#**************************************************************************************************************
# note order of arguments: matrix first, then er or sds, then wts
getcormat <- function(corrut){
  # get correlation matrix from an upper triangle
  
  # determine nrow ncol of correlation matrix, use brute force for now
  lengths <- c(1, 3, 6, 10, 15, 21, 28, 36)
  dims <- 2:9
  mdim <- dims[lengths==length(corrut)]
  
  cormat <- matrix(0, nrow=mdim, ncol=mdim)
  cormat[lower.tri(cormat)] <- corrut # put it into lower triangle of symmetric matrix to get the correct row-order
  cormat <- cormat + t(cormat) # fill the upper triangle
  diag(cormat) <- 1
  return(cormat)
}

getcormat.lt <- function(lowtri.m){
  # get correlation matrix from a lower triangle that is already in matrix form
  
  cormat <- lowtri.m
  cormat[is.na(cormat)] <- 0
  cormat <- cormat + t(cormat) # fill the upper triangle
  diag(cormat) <- 1
  return(cormat)
}

getcovmat <- function(cormat, sds){
  # get covariance matrix from correlation and sd
  b <- sds %*% t(sds)
  covmat <- b * cormat
  dimnames(covmat) <- dimnames(cormat)
  return(covmat)
}

psd <- function(cormat, sds, wts){
  # portfolio standard deviation using correlation matrix
  wts <- wts / sum(wts) # force-fit wts, no error checking
  wtdsd <- wts * sds # weighted standard deviations
  sd <- sqrt(t(wtdsd) %*% cormat %*% wtdsd) %>% as.numeric
  return(sd)  
}

psd.cov <- function(covmat, wts){
  # portfolio standard deviation using covariance matrix
  wts <- wts / sum(wts) # force-fit wts, no error checking
  sd <- sqrt(wts %*% covmat %*% wts) %>% as.numeric
  return(sd)
}

per <- function(er, wts){
  # portfolio expected return
  # er expected investment return vector
  # wts asset class weights, must sum to 1
  wts <- wts / sum(wts) # force-fit wts, no error checking
  per <- crossprod(er, wts) %>% as.numeric # same as sum(ir * wts)
  return(per)
}
