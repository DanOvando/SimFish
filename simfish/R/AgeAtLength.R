AgeAtLength<- function(Lengths,VBErrorSlope,Linf,vbk,t0,Error)
{
  # Error<- Fish$LengthError
  Lengths[is.na(Lengths)]<- 0
  # Lengths<- LengthDat$Length
  AgeSD<- Error*(1+ VBErrorSlope*Lengths/Linf)
  #   RawAges<- (log(1-(Lengths)/Fish$Linf)/-Fish$vbk)+Fish$t0
  RawAges<- (log(1-pmin(Lengths,Linf*.99)/Linf)/-vbk)+t0
  #   AgeWithError<- RawAges*rlnorm(length(Lengths),mean=0,sd=AgeSD)
  AgeWithError<- pmax(1,RawAges+rnorm(length(Lengths),mean=0,sd=AgeSD))

  return(AgeWithError)
}