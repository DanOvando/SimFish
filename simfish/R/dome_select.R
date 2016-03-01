dome_select <- function(peak_select,min_select,length_caught){

dome_sd <- (peak_select - min_select)/2

dome_shape <- hist(rnorm(10000,peak_select, dome_sd), plot = F)

dome_selectivity <- dome_shape$density/max(dome_shape$density)

b <- approx(x = dome_shape$mids, y = dome_selectivity, xout = seq(0,200))

}


