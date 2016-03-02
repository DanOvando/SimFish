#' Run catch curve
#'
#' @param LengthDat numbers at age data
#' @param CatchCurveWeight weight assigned to MPA based catch curve for natural mortaliry
#' @param WeightedRegression 1 or 0 to use weighted regression
#' @param ReserveYr year MPA goes in place
#' @param OutsideBoundYr year of management change outside
#' @param ManualM manually supplied natural mortality
#' @param GroupMPA 1 or 0 to aggregate MPAs
#' @param Iterations number of iterations to run
#' @param BootStrap 1 or 0 to run boostrapping
#' @param LifeError 1 or 0 to run life history error
#' @param HistInterval binwidth of age histograms
#'
#' @return
#' @export
catch_curve<- function(LengthDat,Fish,CatchCurveWeight = 0,
                       WeightedRegression = 1, ReserveYr = NA
                       ,OutsideBoundYr = NA, ManualM = 0.2,GroupMPA = F,
                       Iterations = 1, BootStrap = 0,LifeError = 1,HistInterval = 20,MinSampleSize = 200,
                       length_at_age_key)
{

  SampleSize<- NA

  ################
  #### Process Data ####
  ################

  FindPeaks<- function(Freqs,Fish, PeakTol = 0.001)
  {
    MaxPeak<- max(Freqs)

    TopThree<- sort(Freqs,decreasing=T)[1:3]

    DeltaMax<- (MaxPeak-TopThree)/MaxPeak

    Peaks<- TopThree[DeltaMax<PeakTol]

    ReverseFrequency<-   (Freqs)

    Peak<- ReverseFrequency[which(ReverseFrequency %in% Peaks)[1]]

    WherePeak<- which(Freqs==Peak)[1]
    return(WherePeak)
  }

  if (GroupMPA==T){LengthDat$MPA<- 0}

  CCWeight<- CatchCurveWeight

  #   SampleSizeSummary<- ddply(LengthDat,c('Year'),summarize,SampleSize=length(Length))

  SampleSizeSummary<- LengthDat %>%
    group_by(year) %>%
    summarize(SampleSize=sum(total_numbers))

  SufficientSamples<- SampleSizeSummary$SampleSize>MinSampleSize

  if (sum(SufficientSamples)==0)
  {
    stop('No years have enough length data to run')
  }

  AvailableYears<- SampleSizeSummary$year[SufficientSamples]

  WhereAvailableYears<- LengthDat$year %in% AvailableYears

  LengthDat<- LengthDat[WhereAvailableYears,]

  Years<- sort(unique(LengthDat$year))

  MCOutput<- as.data.frame(matrix(NA,nrow=length(Years)*Iterations,ncol=7))

  colnames(MCOutput)<- c('Iteration','Year','Method','SampleSize','Value','Metric','Flag')

  MCDetails<- as.data.frame(matrix(NA,nrow=length(Years)*Iterations,ncol=6))

  colnames(MCDetails)<- c('Iteration','Year','TotalMortality','FishingMortality','NaturalMortality','SampleSize')

  Output<- as.data.frame(matrix(NA,nrow=length(Years),ncol=9))

  colnames(Output)<- c('Year','Method','SampleSize','Value','LowerCI','UpperCI','SD','Metric','Flag')

  Details<- as.data.frame(matrix(NA,nrow=length(Years)*Iterations,ncol=9))

  colnames(Details)<- c('Year','FishingMortality','LowerCI','UpperCI','SD','NaturalMortality','LowerCI','UpperCI','SD')

  BaseFish<- Fish

  c<- 0

  LengthDat$MPA <- 0

  it_results <- list()

  for (i in 1:Iterations) #Loop over Monte Carlo run
  {

    if (i>1 & LifeError==1) #Apply life history errors
    {
      Fish<- BaseFish

      Fish<- ApplyLifeHistoryError(Fish)
    }

    if (sum(LengthDat$MPA)>0)
    {

      AllAgeHist<- pmax(.01,AgeAtLength(subset(LengthDat,MPA==1)$Length,Fish,Fish$AgeSD  )) #Calculate age at length

      AgeHist <- hist(AllAgeHist,plot=F)

      MaxSelectedAge<- max(AgeHist$counts)[1]
      FullySelectedAge<- ceiling(AgeHist$mids[AgeHist$counts == MaxSelectedAge])

    }

    if (sum(LengthDat$MPA)==0)
    {

      LengthDat$age <- round(AgeAtLength(LengthDat$LengthBinsMid, VBErrorSlope = .1, Linf = Fish$Linf,
                                         vbk = Fish$vbk, t0 = Fish$t0, Error = 0.25))

#       check <- LengthDat %>%
#         group_by(age) %>%
#           summarise(mean_pred_length = mean(LengthBinsMid)) %>%
#             left_join(length_at_age_key, by = 'age')

#       browser()

      #       AllAgeHist<- pmax(.01,AgeAtLength(filter(LengthDat,MPA==0)$Length,Fish,Fish$LengthError  )) #Calculate age at length

      #       AgeHist <- hist(AllAgeHist,plot=F)
      #
      AgeHist <- LengthDat %>%
        ungroup() %>%
        group_by(year,age) %>%
        summarise(counts = round(sum(total_numbers, na.rm = T))) %>%
          mutate(log_counts = log(counts))

      MaxSelectedAge<- which.max(AgeHist$counts)[1]

      FullySelectedAge<- floor(AgeHist$age[MaxSelectedAge])

    }

    reg_results <- list()
    for (y in 1:length(Years)) #Loop over years
    {


      Flag<- 'None'

      TempAgeHist <- AgeHist[AgeHist$year==Years[y],] %>%
        filter(counts > 0)

      #       TempLengthDat<- TempLengthDat[is.na(TempLengthDat$Length)==FALSE,]
      #
      #       SampleSize[y]<- dim(TempLengthDat)[1]

      c<- c+1

      if (i>1 & BootStrap==1) #Resample length data inside and outside of MPAs
      {

        FishedDat<- TempAgeHist[TempAgeHist$MPA==0,]

        NumPoints<- 1:dim(FishedDat)[1]

        BootSample<- sample(NumPoints,length(NumPoints),replace=T)

        TempFishedDat<- FishedDat[BootSample,]

        MPADat<- TempAgeHist[TempAgeHist$MPA==1,]

        NumPoints<- 1:dim(MPADat)[1]

        BootSample<- sample(NumPoints,length(NumPoints),replace=T)

        TempMPADat<- MPADat[BootSample,]

        TempAgeDat<- rbind(TempFishedDat,TempMPADat)


      }

      # TempLengthDat$Length[TempLengthDat$Length>=Fish$Linf]<- Fish$Linf*.98

      #       TempLengthDat<- TempLengthDat[TempLengthDat$Length<Fish$Linf,] #Only use length data less than Linf (might want to fix this)

      #       TempLengthDat$Age<- pmax(.01,AgeAtLength(TempLengthDat$Length,Fish,Fish$AgeSD	)) #Calculate age at length

      #       BinBreaks<- seq(from=0,to=ceiling(max(TempLengthDat$Age,na.rm=T)+1),by=HistInterval)

      #       AllHist<- DanHist(TempLengthDat$Age,BinBreaks)

      TotalPeak<- which.max(TempAgeHist$counts)[1]

      # BinBreaks<- 0:(Fish$MaxAge)

      # BinBreaks<- seq(from=0,to=(Fish$MaxAge),by=HistInterval)

      #       AgeDistFished<- DanHist(TempLengthDat$Age[TempLengthDat$MPA==0],BinBreaks) #Create histogram of age data outside MPA

      AgeDistFished <- TempAgeHist
      AgeDistFished$MPA<- 'Fished Area'

      #       FishedPeak<- which(AgeDistFished$Frequency ==max(AgeDistFished$Frequency))[1] #Identidy the mode of the fished age hist.

      #       Freqs<- AgeDistFished$Frequency

      FishedPeak<- FindPeaks(AgeDistFished$counts,Fish)
      #       FishedPeak<- FullySelectedAge
      # FishedPeak<- TotalPeak

      FishedAllObserved<- (which(AgeDistFished$counts>0))

      #       AgeDistMPA <- DanHist(TempLengthDat$Age[TempLengthDat$MPA==1],BinBreaks) #Create histogram of age data inside MPA
      #
      #       AgeDistMPA$MPA<- 'MPA'
      #
      #       Freqs<- AgeDistMPA$Frequency
      #
      #       MPAPeak<- FindPeaks(Freqs,Fish)

      #       if ((Years[y]-ReserveYr)>20)
      #       {
      #         MPAPeak<- FullySelectedAge
      #       }

      # MPAPeak<- TotalPeak

      #       MPAAllObserved<- (which(AgeDistMPA$Frequency>0))
      # if ((length(MPAAllObserved)>1 & length(FishedAllObserved)>1) & (is.na(AgeDistMPA$LogFrequency[MPAPeak])==F & is.na(AgeDistFished$LogFrequency[FishedPeak])==F)) #If you've got any real histogram to work with
      if (length(FishedAllObserved)>1) # & is.na(AgeDistFished$LogFrequency[FishedPeak])==F) #If you've got any real histogram to work with

      {

        #         if (sum(TempLengthDat$MPA,na.rm=T)>0)
        #         {
        #
        #
        #           MPALastObserved<- MPAAllObserved[length(MPAAllObserved)-1] #Find second to last observed age in the MPA
        #
        #
        #           if (WeightedRegression==1)
        #           {
        #             MPALastObserved<- MPALastObserved+1
        #           }
        #
        #           if (is.na(ReserveYr)==F & Years[y]>ReserveYr) # Bound the last observed age by species young enough to only be impacted by the MPA
        #           {
        #             BoundedLastObserved<- MPAPeak+(Years[y]-ReserveYr)
        #
        #             if (BoundedLastObserved<MPALastObserved)
        #             {
        #               MPALastObserved<- BoundedLastObserved #Adjust last observed age by MPA age
        #             }
        #           }
        #
        #           if (MPAPeak==MPALastObserved & length(MPAAllObserved)!=1) #Stupid fix
        #           {
        #             MPAPeak<- MPAAllObserved[length(MPAAllObserved)-1]
        #           }
        #
        #           if (length(MPAAllObserved)==1)
        #           {
        #             Flag<- 'Catch Curve cannot run - Only 1 Age group observed'
        #           }
        #
        #           NumAgeGroups<- length(AgeDistMPA$Frequency)
        #
        #           MPANumPoints<- length(MPAPeak:MPALastObserved)
        #
        #           MPACatchCurve<- lm((AgeDistMPA$LogFrequency[MPAPeak:MPALastObserved]) ~ (AgeDistMPA$Age[MPAPeak:MPALastObserved]),na.action='na.omit') #Fit catch curve between peak and final point
        #
        #           PredictedMPAValues<- predict(MPACatchCurve,data.frame(Ages=seq(from=BinBreaks[MPAPeak],to=BinBreaks[MPALastObserved],length.out=MPANumPoints)))
        #
        #           if (WeightedRegression==1)
        #           {
        #
        #             RegWeights<- pmax(0,PredictedMPAValues)/sum(pmax(0,PredictedMPAValues))
        #
        #             MPACatchCurve<- lm((AgeDistMPA$LogFrequency[MPAPeak:MPALastObserved]) ~ (AgeDistMPA$Age[MPAPeak:MPALastObserved]),na.action='na.omit',weights= RegWeights) #Fit catch curve between peak and final point
        #
        #             PredictedMPAValues<- predict(MPACatchCurve,data.frame(Ages=seq(from=BinBreaks[MPAPeak],to=BinBreaks[MPALastObserved],length.out=MPANumPoints)))
        #
        #           }
        #
        #
        #           CatchCurveNaturalMortality<- -MPACatchCurve$coefficients[2] #Natural mortality: slope of MPA catch curve
        #
        #
        #           if (CCWeight=='AgeBased') #Allows the weight of the MPA based M to increase the older the MPA is
        #           {
        #             CatchCurveWeight<- Years[y]-ReserveYr
        #           }
        #
        #           MortalityWeight<- c(CatchCurveWeight,1,1)
        #
        #           MortalityWeight<- MortalityWeight/sum(MortalityWeight)
        #           if(is.na(CatchCurveNaturalMortality))
        #           {
        #             Flag<- 'Catch Curve could not estimate M'
        #           }
        #
        #           NaturalMortality<-  sum(MortalityWeight*
        #                                     c(CatchCurveNaturalMortality,(Fish$MvK*Fish$vbk),(4.899*Fish$MaxAge^-.916)),na.rm=T)/sum(MortalityWeight)
        #
        #           #           NaturalMortality<-  CatchCurveNaturalMortality
        #         }

        if (ManualM == 1 ) #| sum(TempLengthDat$MPA,na.rm=T)==0 | (is.na(ReserveYr)==F & (Years[y]-ReserveYr)<2)) #Use lit or LHI based M if set that way, if there is no MPA, or if the MPA is less than 2 years old
        {

          MortalityWeight<- c(1,1)

          MortalityWeight<- MortalityWeight/sum(MortalityWeight)

          #           NaturalMortality<-  sum(MortalityWeight*c(Fish$M,Fish$MvK*Fish$vbk),na.rm=T)/sum(MortalityWeight) #Calcualte weighted natural mortality

          NaturalMortality<-  sum(MortalityWeight*
                                    c((Fish$MvK*Fish$vbk),(4.899*Fish$MaxAge^-.916)),na.rm=T)/sum(MortalityWeight)
          Flag<- 'No MPA based M possible - derived from LHI and Lit'
        }

        FishedLastObserved<- FishedAllObserved[length(FishedAllObserved)-1]

        if (WeightedRegression==1)
        {

          FishedLastObserved <-  FishedLastObserved+1

        }

        if (is.na(OutsideBoundYr)==F & Years[y]>OutsideBoundYr) # Allows bounding if there was a selectivity intervention outside the MPA
        {
          BoundedLastObserved<- FishedPeak+(Years[y]-OutsideBoundYr)

          if (BoundedLastObserved<FishedLastObserved)
          {
            FishedLastObserved<- BoundedLastObserved
          }
        }

        FishedNumPoints<- length(FishedPeak:FishedLastObserved)

        if (FishedPeak==FishedLastObserved & length(FishedAllObserved)!=1  )
        {

          Flag<- 'Catch Curve cannot run, no righthand side of age distribution'
          # FishedPeak<- FishedAllObserved[length(FishedAllObserved)-1]

        }

        if (length(FishedAllObserved)==1)
        {
          Flag<- 'Catch Curve cannot run - Only 1 Age group observed'
        }

        if (FishedPeak<FishedLastObserved)
        {
          FishedNumPoints<- length(FishedPeak:FishedLastObserved)

          FishedCatchCurve<- lm(AgeDistFished$log_counts[FishedPeak:FishedLastObserved] ~ AgeDistFished$age [FishedPeak:FishedLastObserved], na.action = na.exclude)#Fit catch curve between peak and final point

          PredictedFishedValues<- predict(FishedCatchCurve)


          if (WeightedRegression==1)
          {

            RegWeights<- pmax(0, PredictedFishedValues)/sum(pmax(0, PredictedFishedValues))

            FishedCatchCurve<- lm(AgeDistFished$log_counts[FishedPeak:FishedLastObserved] ~ AgeDistFished$age [FishedPeak:FishedLastObserved], na.action='na.omit',weights=RegWeights)#Fit catch curve between peak and final point

            predicted_log_count <- predict(FishedCatchCurve)
            PredictedFishedValues<- data_frame(age = AgeDistFished$age [FishedPeak:FishedLastObserved], predicted_log_count = predicted_log_count)
          }

          CatchCurveTotalMortality<- -FishedCatchCurve$coefficients[2]

          reg_results[[y]] <- data_frame(year = Years[y], age = AgeDistFished$age,
                                         log_counts = AgeDistFished$log_counts) %>%
            left_join(PredictedFishedValues, by = 'age') %>%
                                         mutate(z = CatchCurveTotalMortality, f =  CatchCurveTotalMortality-NaturalMortality,
                                         m = NaturalMortality)

        }
        else
        {
          CatchCurveTotalMortality<- NA
        }

        TotalMortality<- CatchCurveTotalMortality

        FishingMortality<- TotalMortality-NaturalMortality

        if (FishingMortality<0 & is.na(FishingMortality)==F)
        {
          Flag<- 'Catch-Curve not working-Negative Fishing Mortality'
        }

        AgeDist<- rbind(AgeDistFished)

        #         if (i==1) #Plot catch curves analysis on first iteration
        #         {
        #
        #           if (sum(TempLengthDat$MPA,na.rm=T)>0)
        #           {
        #             pdf(file=paste(FigureFolder,i,Years[y],' Catch Curve Analysis.pdf'))
        #             LayoutMatrix<- matrix(c(1:2),nrow=2,ncol=1)
        #             PlotLayout<- layout(mat=LayoutMatrix)
        #             plot(LogFrequency ~ Age,data=AgeDistMPA,xlab='Age',ylab='ln Frequency',bty='n',col=3,cex=2,pch=16,main='Inside Reserve',xlim=c(0,Fish$MaxAge+1),ylim=c(0,ceiling(max(AllHist$LogFrequency,na.rm=T))))
        #             lines(seq(from=BinBreaks[MPAPeak],to=BinBreaks[MPALastObserved],length.out=MPANumPoints),PredictedMPAValues,lty=2,lwd=2)
        #             text(ceiling(max(TempLengthDat$Age+1))*.65,.95*max(PredictedMPAValues),labels=paste('M=',round(NaturalMortality,2) ))
        #             if (FishedPeak<FishedLastObserved){
        #               plot(LogFrequency ~ Age,data=AgeDistFished,xlab='Age',ylab='ln Frequency',bty='n',col=2,cex=2,pch=16,main='Outside Reserve',xlim=c(0,Fish$MaxAge+1),ylim=c(0,ceiling(max(AllHist$LogFrequency,na.rm=T))))
        #               lines(seq(from=BinBreaks[FishedPeak],to=BinBreaks[FishedLastObserved],length.out=FishedNumPoints),PredictedFishedValues,lty=2,lwd=2)
        #               text(ceiling(max(TempLengthDat$Age+1))*.65,max(PredictedFishedValues)*.95,labels=paste('Z=',round(TotalMortality,2), '; F=', round(FishingMortality,2) ))
        #             }
        #             dev.off()
        #           }
        #           else
        #           {
        #             pdf(file=paste(FigureFolder,i,Years[y],' Catch Curve Analysis.pdf'))
        #             plot(LogFrequency ~ Age,data=AgeDistFished,xlab='Age',ylab='ln Frequency',bty='n',col=2,cex=2,pch=16,main='Outside Reserve',xlim=c(0,max(TempLengthDat$Age+1)+1),ylim=c(0,ceiling(max(AllHist$LogFrequency,na.rm=T))))
        #             lines(seq(from=BinBreaks[FishedPeak],to=BinBreaks[FishedLastObserved],length.out=FishedNumPoints),PredictedFishedValues,lty=2,lwd=2)
        #             text(ceiling(max(TempLengthDat$Age+1))*.65,max(PredictedFishedValues)*.95,labels=paste('Z=',round(TotalMortality,2), '; F=', round(FishingMortality,2) ))
        #             dev.off()
        #
        #           }
        #
        #         }

        # MCOutput[c,]<- c(i,Years[y],'CatchCurve',FishingMortality/NaturalMortality,'FvFmsy',Flag)
        # MCDetails[c,]<- c(i,Years[y],FishingMortality,NaturalMortality)
        MCOutput$Iteration[c]<- i
        MCOutput$Year[c]<- Years[y]
        MCOutput$Method[c]<- 'CatchCurve'
        #   MCOutput$Value[c]<- FishingMortality/NaturalMortality
        MCOutput$Metric[c]<- 'FvM'
        MCOutput$Flag[c]<- Flag
        MCOutput$SampleSize[c]<- SampleSize[y]
        MCDetails$Iteration[c]<- i
        MCDetails$Year[c]<- Years[y]
        MCDetails$TotalMortality[c]<- TotalMortality
        MCDetails$FishingMortality[c]<- FishingMortality
        MCDetails$NaturalMortality[c]<- NaturalMortality
        MCDetails$SampleSize[c]<-  sum(AgeDist$counts, na.rm = T)
      }


      #       if  (length(MPAAllObserved)==1 | length(FishedAllObserved)==1 )
      #       {
      #         Flag<- 'Catch Curve cannot run - Only 1 Age group observed'
      #         MCOutput$Flag[c]<- Flag
      #         MCOutput$Iteration[c]<- i
      #         MCOutput$Year[c]<- Years[y]
      #         MCOutput$Method[c]<- 'CatchCurve'
      #         #   MCOutput$Value[c]<- NA
      #         MCOutput$Metric[c]<- 'FvM'
      #         MCOutput$Flag[c]<- Flag
      #
      #
      #         MCDetails$Iteration[c]<- i
      #         MCDetails$Year[c]<- Years[y]
      #         MCDetails$TotalMortality[c]<- NA
      #         MCDetails$FishingMortality[c]<- NA
      #         MCDetails$NaturalMortality[c]<- NA
      #         MCDetails$SampleSize[c]<- NA
      #
      #
      #
      #       }
      MCOutput$Iteration[c]<- i

      #Close Minimum Sample Size Loop
    } #Close Years loop

    it_results[[i]] <- ldply(reg_results)

  } #Close monte carlo loop
  if (sum(is.na(MCDetails$Iteration)==F)>0)
  {

    MCDetails$WeightedYear<- Fish$Alpha*(((MCDetails$Year-min(MCDetails$Year,na.rm=T))+1)/length(unique(MCDetails$Year)))

    MCDetails$WeightedSample<- (1-Fish$Alpha)*MCDetails$SampleSize/max(MCDetails$SampleSize,na.rm=T)

    #     MeanNaturalMort <- ddply(MCDetails,c('Iteration'),summarize,
    #                           MeanNaturalMort=sum(NaturalMortality*(WeightedYear+WeightedSample),na.rm=T)/sum(WeightedYear+WeightedSample,na.rm=T))
    #
    MeanNaturalMort <- MCDetails %>%
      group_by(Iteration) %>%
      summarize(MeanNaturalMort=sum(NaturalMortality*(WeightedYear+WeightedSample),na.rm=T)/sum(WeightedYear+WeightedSample,na.rm=T))

    MCDetails<- join(MCDetails,MeanNaturalMort,by='Iteration')

    MCDetails$FishingMortality<- MCDetails$TotalMortality-MCDetails$MeanNaturalMort

    MCDetails$FvM<- MCDetails$FishingMortality / MCDetails$MeanNaturalMort

    MCOutput$Value<- MCDetails$FvM

    MCOutput$Flag[MCOutput$Flag=='Catch-Curve not working-Negative Fishing Mortality']<- 'None'

    MCOutput$Flag[MCOutput$Value<0]<- 'warning-MPA slope steeper than fished slope'

    #     if (length(unique(MCOutput$Year))>1)
    #     {
    #
    #       pdf(file=paste(FigureFolder,' Catch Curve FvM Boxplots.pdf',sep=''))
    #
    #       p=ggplot(MCDetails,aes(factor(Year),FvM,fill=SampleSize))+geom_boxplot()+xlab('Year')+ylab('F/M')
    #       print(p)
    #       dev.off()
    #
    #
    #       pdf(file=paste(FigureFolder,' Catch Curve F Boxplots.pdf',sep=''))
    #
    #       p=ggplot(MCDetails,aes(factor(Year),FishingMortality,fill=SampleSize))+geom_boxplot()+xlab('Year')+ylab('F')
    #       print(p)
    #       #+scale_y_continuous(limits = quantile(MCDetails$FishingMortality, c(0.1, 0.9),na.rm=T)))
    #
    #       dev.off()
    #
    #       pdf(file=paste(FigureFolder,' Catch Curve M Boxplots.pdf',sep=''))
    #
    #       p=ggplot(MCDetails,aes(factor(Year),MeanNaturalMort,fill=SampleSize))+geom_boxplot()+xlab('Year')+ylab('M')
    #       #     print(p+scale_y_continuous(limits = quantile(MCDetails$MeanNaturalMort, c(0.1, 0.9),na.rm=T)))
    #       print(p)
    #
    #       dev.off()
    #     }
    #     if (length(unique(MCOutput$Year))==1)
    #     {
    #
    #       pdf(file=paste(FigureFolder,' Catch Curve FvM Boxplots.pdf',sep=''))
    #
    #       p=ggplot(MCDetails,aes(factor(Year),FvM))+geom_boxplot()+xlab('Year')+ylab('F/M')
    #       print(p)
    #       dev.off()
    #
    #
    #       pdf(file=paste(FigureFolder,' Catch Curve F Boxplots.pdf',sep=''))
    #
    #       p=ggplot(MCDetails,aes(factor(Year),FishingMortality))+geom_boxplot()+xlab('Year')+ylab('F')
    #       print(p)
    #       #+scale_y_continuous(limits = quantile(MCDetails$FishingMortality, c(0.1, 0.9),na.rm=T)))
    #
    #       dev.off()
    #
    #       pdf(file=paste(FigureFolder,' Catch Curve M Boxplots.pdf',sep=''))
    #
    #       p=ggplot(MCDetails,aes(factor(Year),MeanNaturalMort))+geom_boxplot()+xlab('Year')+ylab('M')
    #       #     print(p+scale_y_continuous(limits = quantile(MCDetails$MeanNaturalMort, c(0.1, 0.9),na.rm=T)))
    #       print(p)
    #
    #       dev.off()
    #     }

  }

  #####################################
  ##### Proces Monte Carlo Data #######
  #####################################

  TrueIteration<- MCOutput$Iteration==1

  TrueOutput<- MCOutput[TrueIteration,]

  TrueDetails<- MCDetails[TrueIteration,]

  MCOutput<- MCOutput[TrueIteration==F,]

  Output$Year<- Years

  Output$Method<- 'CatchCurve'

  Output$Value<- TrueOutput$Value

  Output$LowerCI<- NA

  Output$UpperCI<- NA

  Output$SD<- NA

  Output$Metric<- 'FvM'

  Output$Flag<-TrueOutput$Flag

  Output$SampleSize<- SampleSize

  if (Iterations>1)
  {

    for (y in 1:length(Years))
    {
      Where<- MCOutput$Year==Years[y]

      Temp<- MCOutput[Where,]

      if(sum(Temp$Value,na.rm=T)>0)
      {
        TempValue<- sort(as.numeric(Temp$Value))

        Bottom<- ceiling(.025*length(TempValue))

        Top<- ceiling(.975*length(TempValue))

        MeanMetric<- mean(as.numeric(Temp$Value),na.rm=T)

        LowerCI<- TempValue[Bottom]

        UpperCI<- TempValue[Top]

        SD<- sd(TempValue[Bottom:Top],na.rm=T)

        # Output[y,]<- c(Years[y],'CatchCurve',TrueOutput$Value[y],LowerCI,UpperCI,SD,'FvFmsy',TrueOutput$Flag[y])

        Output$Year[y]<- Years[y]

        Output$Method[y]<- 'CatchCurve'

        Output$Value[y]<- TrueOutput$Value[y]

        Output$LowerCI[y]<- LowerCI

        Output$UpperCI[y]<- UpperCI

        Output$SD[y]<- SD

        Output$Metric[y]<- 'FvM'

        Output$Flag[y]<-TrueOutput$Flag[y]
      }

    }
  }

  #####################################
  ##### Plot outputs #######
  #####################################


  Fish<- BaseFish


  return(list(Output=Output,Details=MCDetails,MonteCarlo=MCOutput, it_results = ldply(it_results)))
}
