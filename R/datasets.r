#' Horizon 2017 capital market assumptions.
#'
#' Expected returns and standard deviations by asset class, and correlation matrix
#'
#' @source Horizon 2017 report
#' @format List with 2 elements
#' \describe{
#' \item{ersd}{expected returns and standard deviations}
#' \item{cormat}{correlation matrix}
#' }
#' @examples
#'   horizon10year2017
#'   horizon10year2017$cormat
"horizon10year2017"


#' RVK capital market assumptions used for the LAFPP pension plan.
#'
#' Expected returns and standard deviations by asset class, and correlation matrix
#'
#' @source RVK asset-liability studey
#' @format List with 2 elements
#' \describe{
#' \item{ersd}{expected returns and standard deviations}
#' \item{cormat}{correlation matrix}
#' }
#' @examples
#'   rvk
#'   rvk$cormat
"rvk"


#' Stalebrink .
#'
#' Expected returns and standard deviations by asset class, and correlation matrix
#'
#' @source Stalebrink paper
#' @format List with 2 elements
#' \describe{
#' \item{ersd}{expected returns and standard deviations}
#' \item{cormat}{correlation matrix}
#' }
#' @examples
#'   stalebrink
#'   stalebrink$cormat
"stalebrink"
