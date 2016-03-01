#' Simulate fleet in SimFish
#'
#' \code{simfleet} adjusts fishing mortality based on a fleet model
#' @param fleetmodel the form of fleet model to use
#' @param prior_profits profits in the prior year
#' @param msy_effort effort at MSY
#' @param msy_profits profits at MSY
#' @param prior_effort effort in the prior year
#' @param exp_bio exploitable biomass
#' @param q catcheability coefficient
#' @param phi responsiveness of fleet to profits
#'
#' @return new fishing mortality rate
#' @export
simfleet <- function(fleetmodel, prior_profits, msy_effort,msy_profits, prior_effort, exp_bio = 0, q, phi){

if (fleetmodel == 'Open Access'){
# new_effort <- pmax(1e-5,prior_effort * (1 + phi*(prior_profits)))
new_effort <- pmax(1e-6,prior_effort + (phi * (prior_profits/msy_profits)))
# pmin(2*prior_effort,
new_u <- pmin(0.99,pmax(0,new_effort*q))
new_f <- -log(1 - new_u)

}

return(new_f)
}