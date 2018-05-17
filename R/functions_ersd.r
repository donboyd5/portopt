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

# Covariance and correlation matrix must be positive semi-definite or else a variance can be negative
# Note that "A correlation matrix is a symmetric positive semidefinite matrix with unit diagonal" per
# Higham 2001 (http://eprints.ma.man.ac.uk/232/01/covered/MIMS_ep2006_70.pdf).

#' Check whether covariance or correlation matrix is positive semi-definite.
#'
#' A covariance matrix should be positive semi-definite or else a variance can be negative. See, for example:
#'   https://epublications.bond.edu.au/cgi/viewcontent.cgi?article=1078&context=ejsie
#'   https://stats.stackexchange.com/questions/69114/why-does-correlation-matrix-need-to-be-positive-semi-definite-and-what-does-it-m
#'   https://stats.stackexchange.com/questions/125412/is-every-correlation-matrix-positive-semi-definite
#'   https://blogs.sas.com/content/iml/2012/09/12/when-is-a-correlation-matrix-not-a-correlation-matrix.html
#'
#' A good discussion of the problem and possible solutions
#' is at:
#'
#'   https://nickhigham.wordpress.com/2013/02/13/the-nearest-correlation-matrix/
#'
#' His method is used in makePDcorr.
#'
#' @param cmat A correlation or covariance matrix for asset class returns.
#' @return TRUE if matrix is positive semi-definite, FALSE if not.
#' @examples
#' is.PSD(covmat(stalebrink$cormat, stalebrink$ersd$sd)) # this IS positive semi-definite
#' is.PSD(stalebrink$cormat)
#'
#' is.PSD(covmat(rvk$cormat, rvk$ersd$sd)) # NOT PSD
#' is.PSD(rvk$cormat)
#'
#' is.PSD(covmat(horizon10year2017$cormat, horizon10year2017$ersd$sd)) # IS PSD
#' is.PSD(horizon10year2017$cormat)
#' @export
is.PSD <- function(cmat){
  # if symmetric PSD, then all eigen values are non-negative -- required for a correlation matrix
  isnotPSD <- any(eigen(cmat)$values < 0) # if true, covmat is NOT PSD
  isPSD <- !isnotPSD
  return(isPSD)
}


#' Get the nearest positive definite correlation matrix to the matrix we have.
#'
#' Note that this insists upon returning a positive definite correlation matrix. We would be
#' satisfied if it only returned a positive semi-definite matrix, but that does not seem
#' to be an option.
#'
#' See:
#'   Higham, Nicholas J. (2002) Computing the Nearest Correlation Matrix---A Problem from Finance. IMA Journal of Numerical Analysis, 22 (3). pp. 329-343. ISSN 0272-4979
#'   http://eprints.ma.man.ac.uk/232/
#'
#'   https://www.nag.com/IndustryArticles/fixing-a-broken-correlation-matrix.pdf
#'
#'
#' @param cormat A correlation matrix for asset class returns.
#' @return cormat if correlation matrix is positive definite, FALSE if not.
#' @examples
#' library("magrittr")
#'
#' is.PSD(rvk$cormat)
#' cormat2 <- makePDcorr(rvk$cormat)
#' is.PSD(cormat2)
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


