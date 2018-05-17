# ersd:  Functions related to expected return (er) and standard deviation (sd).


#**************************************************************************************************************
#     Correlation and covariance matrices, and portfolio expected returns and standard deviations ####
#**************************************************************************************************************
# note order of arguments: matrix first, then er or sds, then wts

#' Get a portfolio's expected return from asset-class expected returns and weights.
#'
#' @param ervec A vector of asset-class expected returns.
#' @param wts A vector of asset-allocation weights. Should sum to 1.
#' @return The portfolio expected return.
#' @examples
#' library("magrittr")
#' per(stalebrink$ersd$er, c(.25, .25, .2, .15, .1, .05))
#' @export
per <- function(ervec, wts){
  # portfolio expected return
  # er expected investment return vector
  # wts asset class weights, must sum to 1
  wts <- wts / sum(wts) # force-fit wts, no error checking
  per <- crossprod(ervec, wts) %>% as.numeric # same as sum(ir * wts)
  return(per)
}


#' Get a portfolio's standard deviation from the correlation matrix, standard deviations, and asset-allocation weights.
#'
#' @param cormat A correlation matrix for asset class returns.
#' @param sdvec A vector of standard deviations.
#' @param wts A vector of asset-allocation weights. Should sum to 1.
#' @return The portfolio standard deviation.
#' @examples
#' library("magrittr")
#' psd(stalebrink$cormat, stalebrink$ersd$sd, c(.25, .25, .2, .15, .1, .05))
#' @export
psd <- function(cormat, sdvec, wts){
  wts <- wts / sum(wts) # force-fit wts, no error checking
  wtdsd <- wts * sdvec # weighted standard deviations
  psd <- sqrt(t(wtdsd) %*% cormat %*% wtdsd) %>% as.numeric
  return(psd)
}


#' Get covariance matrix for asset returns from the correlation matrix and standard deviations.
#'
#' @param cormat A correlation matrix for asset class returns.
#' @param sdvec A vector of standard deviations.
#' @return The covariance matrix.
#' @examples
#' covmat(stalebrink$cormat, stalebrink$ersd$sd)
#' covmat(rvk$cormat, rvk$ersd$sd)
#' covmat(horizon10year2017$cormat, horizon10year2017$ersd$sd)
#' @export
covmat <- function(cormat, sdvec){
  b <- sdvec %*% t(sdvec)
  covmat <- b * cormat
  dimnames(covmat) <- dimnames(cormat)
  return(covmat)
}


#**************************************************************************************************************
#     Diagnostic and correction tools ####
#**************************************************************************************************************

# Covariance and correlation matrix must be positive definite or else a variance can be negative
# Note that "A correlation matrix is a symmetric positive semidefinite matrix with unit diagonal" per
# Higham 2001 (http://eprints.ma.man.ac.uk/232/01/covered/MIMS_ep2006_70.pdf).

#' Check whether covariance or correlation matrix is positive definite.
#'
#' A covariance matrix should be positive definite or else a variance can be negative. See, for example,
#'   https://epublications.bond.edu.au/cgi/viewcontent.cgi?article=1078&context=ejsie
#'
#' There are a variety of available fixes if it is not.
#'
#' @param covmat A covariance matrix for asset class returns.
#' @return TRUE if covariance matrix is positive definite, FALSE if not.
#' @examples
#' is.PD(covmat(stalebrink$cormat, stalebrink$ersd$sd)) # this IS positive definite
#' is.PD(stalebrink$cormat)
#'
#' is.PD(covmat(rvk$cormat, rvk$ersd$sd)) # NOT PD
#' is.PD(rvk$cormat)
#'
#' is.PD(covmat(horizon10year2017$cormat, horizon10year2017$ersd$sd)) # IS PD
#' is.PD(horizon10year2017$cormat)
#' @export
is.PD <- function(covmat){
  # if symmetric PD, then all eigen values are positive
  isnotPD <- any(eigen(covmat)$values <= 0) # if true, covmat is NOT PSD
  isPD <- !isnotPD
  return(isPD)
}


#' Get the nearest positive definite correlation matrix to the matrix we have.
#'
#' @param cormat A covariance matrix for asset class returns.
#' @return cormat if covariance matrix is positive definite, FALSE if not.
#' @examples
#' library("magrittr")
#'
#' is.PD(rvk$cormat)
#' cormat2 <- makePDcorr(rvk$cormat)
#' is.PD(cormat2)
#'
#' # compare:
#' rvk$cormat %>% round(., 2)
#' cormat2 %>% round(., 2)
#' (cormat2 - rvk$cormat) %>% round(., 2)
#'
#' @export
makePDcorr <- function(cormat){
  # require("Matrix")
  cormat.adj <- Matrix::nearPD(cormat, corr=TRUE)$mat
  return(cormat.adj)
}


