# ersd:  Functions related to expected return (er) and standard deviation (sd).


#**************************************************************************************************************
#     Functions for correlation and covariance matrices, and portfolio expected returns and standard deviations ####
#**************************************************************************************************************
# note order of arguments: matrix first, then er or sds, then wts

#' Get a portfolio's expected return from asset-class expected returns and weights.
#'
#' @param ervec A vector of asset-class expected returns.
#' @param wts A vector of asset-allocation weights. Should sum to 1.
#' @return The portfolio expected return.
#' @examples
#' per(stalebrink$ersd$er, c(.3, .2, .1, .15, .1, .05))
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
#' psd(stalebrink$cormat, stalebrink$ersd$sd, c(.3, .2, .1, .15, .1, .05))
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





