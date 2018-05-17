#' Compute the minimum-variance efficient frontier over a target set of portfolio expected returns.
#'
#' \code{efrontier} returns a list with efficient frontier results, plus solution information.
#'
#' @param er.target.vec Vector of targeted expected returns
#' @param ersd data frame with 1 row per asset class: class (class name), er (expected return), sd (standard deviation)
#' @param cormat Correlation matrix for asset class returns. Assumed to be positive definite.
#' @param aa.lb Asset allocation lower bound. Can be scalar applied to all classes, or vector of length # of asset classes.
#' @param aa.ub Asset allocation upper bound. Can be scalar applied to all classes, or vector of length # of asset classes.
#' @return eflist if covariance matrix is positive definite, FALSE if not.
#' @examples
#' library("magrittr")
#' library("quadprog")
#' library("tidyverse")
#'
#' # Get no-bounds and bounded efficient frontiers for the Stalebrink capital market assumptions
#' ef.nobound <- efrontier(seq(.00, .20, .0025), stalebrink$ersd, stalebrink$cormat)
#' ef.noshort <- efrontier(seq(.00, .20, .0025), stalebrink$ersd, stalebrink$cormat, 0, 1)
#'
#' # create a data frame with just the data we want
#' efdf <- bind_rows(ef.nobound$efrontier %>% mutate(rule="no bounds on allocation"),
#'                   ef.noshort$efrontier %>% mutate(rule="no shorts or leverage")) %>%
#'         filter(type=="high", !is.na(per))
#'
#' # graph the unbounded and bounded frontiers, and the asset-class assumptions
#' ggplot() +
#'   geom_line(data=efdf, aes(psd, per, colour=rule)) +
#'     scale_x_continuous(name="Standard deviation", breaks=seq(0, .3, .025), labels = scales::percent) +
#'     scale_y_continuous(name="Expected return", breaks=seq(0, .2, .01), labels = scales::percent) +
#'     ggtitle("Efficient frontier for the Stalebrink capital market assumptions", subtitle="With and without bounds on asset allocations") +
#'     # now add the asset-class points
#'     geom_point(data=ef.nobound$ersd, aes(x=sd, y=er)) +
#'     geom_text(data=ef.nobound$ersd, aes(x=sd, y=er, label=class), nudge_y = +.003)
#'
#' # Examine asset class weights for different classes
#' ef.noshort$weights %>%
#' filter(class %in% c("stocks", "cash", "bonds.dom")) %>%
#'   ggplot(aes(er.target, asset.weight, colour=class)) +
#'     geom_line()
#'
#' # Compare weights for an asset class, optimized with and without bounds
#' aclass <- "stocks"
#' wts <- bind_rows(ef.nobound$weights %>% mutate(rule="no bounds on allocation"),
#'                 ef.noshort$weights %>% mutate(rule="no shorts or leverage")) %>%
#'       filter(class==aclass)
#'
#' wts %>%
#'       ggplot(aes(er.target, asset.weight, colour=rule)) +
#'       geom_line() +
#'       geom_hline(yintercept=0) +
#'       ggtitle(paste0(aclass, " weights"))
#'
#' @export
efrontier <- function(er.target.vec, ersd, cormat, aa.lb=-1e9, aa.ub=1e9){
  requireNamespace("tidyverse", quietly = TRUE)

  port <- llply(er.target.vec, minvport, ersd, cormat, aa.lb, aa.ub) # temporary list with results

  # retrieve vectors with portfolio expected returns (pervec) and portfolio standard deviations (psdvec)
  pervec <- llply(1:length(port), function(i) return(port[[i]]$er.port)) %>% unlist
  psdvec <- llply(1:length(port), function(i) return(port[[i]]$sd.port)) %>% unlist

  # create data frame with asset weights for each class, at each target return (plus other information)
  weights <- ldply(1:length(port), function(i) return(port[i][[1]]$portfolio))

  # create a data frame with er targets, calculated portfolio er and sd, and identify which are on the high part of the
  # frontier (good) and which are on the low part (bad)
  efrontier1 <- tibble(er.target=er.target.vec, per=pervec, psd=psdvec) # preliminary frontier, without high-low

  # identify high and low
  rn <- which.min(efrontier1$psd)[1] # which record number has the first instance of lowest sd? that is the split-point between high and low
  efrontier2 <- efrontier1 %>%
    mutate(type=ifelse(row_number() <= rn, "low", "high")) # this labels the split-point as low

  # now add a copy of the split-point labeled high, so that we have one labeled high and one low, so they will meet when graphed
  efrontier <- bind_rows(efrontier2,
                         efrontier2 %>% filter(row_number()==rn) %>% mutate(type="high")) %>%
    mutate(type=ifelse(is.na(per), NA, type)) %>%
    arrange(er.target, per, desc(type))

  eflist <- list()
  eflist$ersd <- ersd
  eflist$efrontier <- efrontier
  eflist$weights <- weights
  eflist$port <- port

  return(eflist)
}




