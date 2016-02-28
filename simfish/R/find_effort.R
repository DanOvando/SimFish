find_effort <- function(effort, q ,vuln,f,m,n_at_age){

  catch_by_f <- sum((vuln*f)/(vuln * f + m) * (1 - exp(-(vuln*f + m))) * n_at_age, na.rm = T)

  f_calc <- -log(1 - q * effort)

  catch_by_effort <- sum((vuln * f_calc)/(vuln * f_calc + m) * (1 - exp(-(vuln * f_calc + m))) * n_at_age, na.rm = T)

  catch_diff <- (catch_by_f - catch_by_effort)^2

  return(catch_diff)

}
