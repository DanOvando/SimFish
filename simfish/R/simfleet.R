
simfleet <- function(fleetmodel, prior_profits, msy_profits, prior_effort, exp_bio = 0, q, phi){

# The concept here. Have an "if" statement inside the time loop. If the fleet is turned
# on, overwrite "historicF" with an F derived from the fleet model
# Potential fleet models:
# 1 Open access
# 2 Constant catch
# 3 Constant effort
# 5
if (fleetmodel == 'Open Access'){

new_effort <- pmax(1e-3,prior_effort + phi*(prior_profits)) #/msy_profits)
new_u <- pmin(0.99,pmax(0,new_effort*q))
new_f <- -log(1 - new_u)

}

return(new_f)
}