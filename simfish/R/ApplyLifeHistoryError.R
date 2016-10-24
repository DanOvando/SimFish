ApplyLifeHistoryError<- function(Fish)
{

  NewFish<- Fish

  NewFish$vbk<- Fish$vbk*rlnorm(1,0,Fish$LengthError)

  NewFish$MvK<- runif(1,NewFish$MinMvK,NewFish$MaxMvK)

  NewFish$M<- NewFish$vbk * NewFish$MvK

  NewFish$Linf<- Fish$Linf*rlnorm(1,0,Fish$LengthError)

  NewFish$LengthMatRatio<- runif(1,NewFish$MinLengthMatRatio,NewFish$MaxLengthMatRatio)

  NewFish$Mat50<- NewFish$Linf * NewFish$LengthMatRatio

  NewFish$Mat95<- NewFish$Mat50*1.05

  NewFish$MaxAge<- -log(.01)/NewFish$M

  return(NewFish)
}