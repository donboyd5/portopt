
#****************************************************************************************************
#       Functions for minimum variance portfolio ####
#****************************************************************************************************
minvport_bounds <- function(er.target, ersd, cormat, aa.lb=-1e9, aa.ub=1e9) {
  # get the minimum variance portfolio, where:
  #   er.target is the desired return (scalar)
  #   ersd is a dataframe with variables class, er, sd (asset class, expected return, standard deviation)
  #   cormat is the correlation matrix
  #   aa.lb is lower bound of asset allocations
  #   aa.ub is upper bound of asset allocations
  #     aa.lb and aa.ub each can be a single value, in which case that value will be used for ALL asset classes
  #     or each can be vector of length nassets, given a different bound for each class
  #     It is fine for one to be a single value and the other to be a vector.
  # The function forces the asset class weights to sum to 1
  # It returns a portfolio object
  
  # TODO: Add error checking.
  
  # prelinaries
  nassets <- nrow(ersd)
  covmat <- getcovmat(cormat, ersd$sd)
  if(length(aa.lb)==1) aa.lb <- rep(aa.lb, nassets)
  if(length(aa.ub)==1) aa.ub <- rep(aa.ub, nassets)
  
  # 2 equality constraints:
  #   portfolio return must equal the target
  #   weights must sum to 1
  A.equality <- matrix(c(ersd$er, rep(1, nassets)), ncol=2) # equality constraints LHS
  b.equality <- c(er.target, 1) # equality constraints RHS
  
  # inequality constraints for asset allocation requirements
  #   LHS (A) is the constraint value
  #   RHS (b) is the lower bound
  #   quadprog will require that A^T b >= b_0
  minwt <- rep(1, nassets)
  maxwt <- rep(1, nassets)
  A.inequality <- cbind(diag(minwt), -diag(maxwt))
  # A.inequality
  # b.inequality <- c(rep(0, nassets), rep(-1, nassets))
  b.inequality <- c(aa.lb, -aa.ub)
  
  Amat <- cbind(A.equality, A.inequality)
  
  bvec <- c(b.equality, b.inequality)
  # must be equal:
  # ncol(Amat) == length(bvec)
  # rbind(Amat, bvec)
  
  # done with setup, solve the problem
  qp <- solve.QP(Dmat=2*covmat, dvec=rep(0, nassets), Amat=Amat, bvec=bvec, meq=length(b.equality))
  port <- list()
  port$er.target <- er.target
  port$er.port <- per(ersd$er, qp$solution)
  port$sd.port <- psd(cormat, ersd$sd, qp$solution)
  port$portfolio <- ersd %>% mutate(asset.weight=qp$solution, per=port$er.port, psd=port$sd.port)
  port$cormat <- cormat
  port$qp <- qp
  return(port)
}


# TODO: Fix the arguments. Not sure we'll ever need this, thoough
minvport_nobounds <- function(target, port){
  # UNBOUNDED
  # efficient portfolio weights, no bounds on asset weights
  # takes a target return and a portfolio object (list) that has elements names, er, and covmat
  # Based on Zivot
  # http://faculty.washington.edu/ezivot/econ424/portfolioTheoryMatrix.pdf
  ones <- rep(1, length(port$ersd$er))
  top <- cbind(2 * port$covmat, port$ersd$er, ones)
  bot <- cbind(rbind(port$ersd$er, ones), matrix(0, 2, 2))
  A <- rbind(top, bot)
  b.target <- as.matrix(c(rep(0, nrow(port$ersd)), target, 1))
  x <- solve(A, b.target)
  wts <- x[1:nrow(port$ersd)]
  names(wts) <- port$names
  return(wts)
}
