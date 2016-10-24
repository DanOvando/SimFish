#' \code{simfish} simulates codys fishery
#'
#' @param Nsim number of simulations
#' @param SimYear number of years to run simulation
#' @param depletion target depletion
#' @param CatchShareN share of the catch in the north parth
#' @param CalcFMSY 1 or 0 to calculate Fmsy doesn't work now
#' @param SmallNum constant to add to zeros
#' @param InitSmooth no idea
#' @param FmortPen no idea
#' @param RecruitPen no idea
#' @param CatchCVn CV of the catch in the north
#' @param CatchCVs CV of the catch in the south
#' @param IndexCVn CV of the survey index in the north
#' @param IndexCVs CV of the survey index in the south
#' @param LenSampleN number of lengths samples in the north
#' @param LenSampleS number of length samples in the south
#' @param GrowthSDn growth standard deviation in the north
#' @param GrowthSDs growth standard deviation in the south
#' @param surv50n lower survey selectivity in the north
#' @param surv95n upper survey selectivity in the north
#' @param surv50s lower survey selectivity in the south
#' @param surv95s upper survey selectivity in the south
#' @param MaxAge maximum age of the species
#' @param NatMn natural mortality in the north
#' @param NatMs natural mortality in the south
#' @param VonKn von bert k in the north
#' @param VonKs von bert k in the south
#' @param LinfN linfinity in the north
#' @param LinfS linfinity in the south
#' @param t0n t0 in the north
#' @param t0s t0 in the south
#' @param mat50n lower maturity in the north
#' @param mat50s lower maturity in the south
#' @param mat95n upper maturity in the north
#' @param mat95s upper maturity in the north
#' @param alphaN alpha of weight function in the north
#' @param betaN beta of weight function in the north
#' @param alphaS alpha of weight function in the south
#' @param betaS beta of weight function in the south
#' @param MaxMovingN maximum moving numbers I think in the north
#' @param MaxMovingS maximum moving numbers I think in the south
#' @param Move50n lower movement ogive north
#' @param Move50s lower movement ogive south
#' @param Move95n upper movement ogive north
#' @param Move95s upper movement ogive south
#' @param steepnessN recruitment steepness in the north
#' @param steepnessS recruitment steepness in the south
#' @param sigmaRn recruitment deviations in the north
#' @param sigmaRs recruitment deviations in the south
#' @param RzeroN eq recruitment in the north
#' @param RzeroS eq recruitment in the south
#' @param sel50n lower fishing selectivity ogive in the north
#' @param sel50s lower fishing selectivity ogive in the south
#' @param sel95n upper fishing selectivity ogive in the north
#' @param sel95s upper fishing selectivity ogive in the south
#' @param HarvestControl no idea
#' @param HistoricalF historical pattern of fishing mortality
#' @param ConstantCatch no idea
#' @param ConstantF no idea
#' @param HCalpha no idea
#' @param HCbeta mo idea
#' @param GrowthCV constant coefficient of variane of length at age
#'
#' @return list of real and observed data and plots
#' @export
simfish <-
  function(Nsim = 1,
           SimYear = 50,
           depletion = 0,
           CatchShareN = 1,
           CalcFMSY = 1,
           SmallNum = 1e-4,
           InitSmooth = 3,
           FmortPen = 3,
           RecruitPen = 3,
           CatchCVn = 0.01,
           CatchCVs = 0.01,
           IndexCVn = 0.05,
           IndexCVs = 0.05,
           LenSampleN = 200,
           LenSampleS = 200,
           GrowthCV = 0.05,
           surv50n = 90,
           surv95n =  150,
           surv50s = 90,
           surv95s = 150,
           MaxAge = 30,
           NatMn = 0.2,
           NatMs = 0.2,
           VonKn = 0.2,
           VonKs = 0.2,
           LinfN = 300,
           LinfS = 300,
           t0n = 0.9,
           t0s = 0.9,
           mat50n = 6,
           mat50s = 6,
           mat95n = 8,
           mat95s = 8,
           alphaN = 1.7e-06,
           betaN = 3,
           alphaS = 1.7e-06,
           betaS = 3,
           MaxMovingN = 0,
           MaxMovingS = 0,
           Move50n = 6,
           Move50s = 6,
           Move95n = 10,
           Move95s = 10,
           steepnessN = 0.7,
           steepnessS = 0.7,
           sigmaRn = 0.001,
           sigmaRs =  0.001,
           recruit_ac = 0,
           RzeroN = 1e5,
           RzeroS = 1e5,
           sel50n = 190,
           sel50s = 190,
           sel95n = 225,
           sel95s = 225,
           HarvestControl = 3,
           HistoricalF = 0.3,
           ConstantCatch = 0,
           ConstantF = 0.1,
           HCalpha = 0.05,
           HCbeta = 0.5,
           price = 1,
           cr_ratio = 0.6,
           cost_beta = 1.1,
           q = 1e-5,
           tech_rate = 0,
           use_fleetmodel = T,
           fleetmodel = 'Open Access',
           phi = 0.025,
           select_model = 'logistic',
           p_sampled = 0.2,
           sd_sample = 0,
           fm_ratio = 0.6,
           zoom_year = 1)
  {
    #=======================
    #==simulation controls==
    #=======================

    plot_theme <-
      theme_economist() + theme(text = element_text(size = 24))



    #   Nsim <- Nsim   #out$OM$Nsim			# number of simulations to do in the MSE
    #   SimYear <- SimYear #  out$OM$SimYear			# total number of years in simulation
    InitYear <-
      SimYear #out$OM$InitYear			# year in which MSE starts (i.e. the number of years of data available)
    #   depletion	 <- depletion		# model is conditioned to a specific depletion given a trajectory of effort
    #   CatchShareN	 <- CatchShareN		# the amount of effort allocated to a given area
    CatchShareS	 <- 1 #-CatchShareN
    #   SmallNum <- SmallNum
    #   InitSmooth <- InitSmooth
    #   FmortPen <- FmortPen
    #   RecruitPen <- RecruitPen
    #   CalcFMSY <- CalcFMSY
    #==========================================================
    #=================population dynamics======================
    #=========================================================
    #==When making dynamics time-varying, only make them vary after
    #=='InitYear'.  The idea is that this simulates a relatively steady
    #==environment until climate change, then things change. MEH.
    #===========================================================
    #===========================================================
    #   MaxAge <- MaxAge

    Ages <- seq(1, MaxAge)

    #==natural mortality================
    NatMn	<-  CleanInput(NatMn, SimYear)

    NatMs		<-  CleanInput(NatMs, SimYear)

    #==Length at age====================
    VonKn	<- CleanInput(VonKn, SimYear)
    LinfN	<- CleanInput(LinfN, SimYear)
    t0n		<- CleanInput(t0n, SimYear)
    LenAtAgeN	<- matrix(nrow = SimYear, ncol = MaxAge)
    for (i in 1:SimYear)
      LenAtAgeN[i, ] <- LinfN[i] * (1 - exp(-VonKn[i] * (Ages - t0n[i])))

    VonKs		<-  CleanInput(VonKs, SimYear)
    LinfS		<-  CleanInput(LinfS, SimYear)
    t0s		<-  CleanInput(t0s, SimYear)
    LenAtAgeS	<- matrix(nrow = SimYear, ncol = MaxAge)
    for (i in 1:SimYear) {
      LenAtAgeS[i, ] <- LinfS[i] * (1 - exp(-VonKs[i] * (Ages - t0s[i])))
    }
    #==specify the number of length bins
    #   browser()
    LengthBins <-
      seq(min(c(LenAtAgeN, LenAtAgeS)), max(c(LenAtAgeS, LenAtAgeN)) * 1.1, by = 5)

    LengthBinsMid <-
      (LengthBins[2:length(LengthBins)] + LengthBins[1:(length(LengthBins) - 1)]) /
      2

    #     LengthBins[1:(length(LengthBins)-1)] + mean(LengthBins[1:2])
    #==maturity at age==========================
    mat50n	<- CleanInput(mat50n, SimYear)
    mat95n	<- CleanInput(mat95n, SimYear)

    matureN	<- matrix(nrow = SimYear, ncol = MaxAge)
    for (i in 1:SimYear)
      matureN[i, ] <-
      1 / (1 + exp(-1 * log(19) * (Ages - mat50n[i]) / (mat95n[i] - mat50n[i])))

    mat50s	<- CleanInput(mat50s, SimYear)
    mat95s	<-  CleanInput(mat95s, SimYear)
    matureS	<- matrix(nrow = SimYear, ncol = MaxAge)
    for (i in 1:SimYear)
      matureS[i, ] <-
      1 / (1 + exp(-1 * log(19) * (Ages - mat50s[i]) / (mat95s[i] - mat50s[i])))

    #==weight at age==========================
    alphaN		<-   CleanInput(alphaN, SimYear)
    betaN			<-  CleanInput(betaN, SimYear)
    WeightAtAgeN	<- matrix(nrow = SimYear, ncol = MaxAge)
    for (i in 1:SimYear)
      WeightAtAgeN[i, ] <- alphaN[i] * LenAtAgeN[i, ] ^ betaN[i]

    alphaS <- CleanInput(alphaS, SimYear)
    betaS <-  CleanInput(betaS, SimYear)
    WeightAtAgeS	<- matrix(nrow = SimYear, ncol = MaxAge)
    for (i in 1:SimYear)
      WeightAtAgeS[i, ]	<- alphaS[i] * LenAtAgeS[i, ] ^ betaS[i]
    #==movement from box to box==
    MaxMovingN	<- CleanInput(MaxMovingN, SimYear)
    Move50n	<- CleanInput(Move50n, SimYear)
    Move95n	<- CleanInput(Move50n, SimYear)
    MovementN	<- matrix(nrow = SimYear, ncol = MaxAge)
    for (i in 1:SimYear)
      MovementN[i, ]	<-
      MaxMovingN[i] / (1 + exp(-1 * log(19) * (Ages - Move50n[i]) / (Move95n[i] -
                                                                       Move50n[i])))

    MaxMovingS <- CleanInput(MaxMovingS, SimYear)
    Move50s	<- CleanInput(Move50s, SimYear)
    Move95s	<- CleanInput(Move95s, SimYear)
    MovementS	<- matrix(nrow = SimYear, ncol = MaxAge)
    for (i in 1:SimYear)
      MovementS[i, ]	<-
      MaxMovingS[i] / (1 + exp(-1 * log(19) * (Ages - Move50s[i]) / (Move95s[i] -
                                                                       Move50s[i])))

    #==recruitment parameters==
    steepnessN	<-  CleanInput(steepnessN, SimYear)
    sigmaRn	<- CleanInput(sigmaRn, SimYear)
    RzeroN	<- CleanInput(RzeroN, SimYear)

    steepnessS	<-  CleanInput(steepnessS, SimYear)
    sigmaRs	<-  CleanInput(sigmaRs, SimYear)
    RzeroS	<- CleanInput(RzeroS , SimYear)
    # Produce potentiall autocorrelated recruitment errors
    RecErrN <- matrix(NA, nrow = Nsim, ncol = SimYear)
    RecErrS <- matrix(NA, nrow = Nsim, ncol = SimYear)
    RecErrN[, 1] <- rnorm(Nsim, 0, sigmaRn[1])
    RecErrS[, 1] <- rnorm(Nsim, 0, sigmaRs[1])

    for (t in 2:SimYear) {
      RecErrN[, t]	<-
        RecErrN[, t - 1] * recruit_ac + rnorm(Nsim, 0, sigmaRn[1])

      RecErrS[, t]	<-
        RecErrS[, t - 1] * recruit_ac + rnorm(Nsim, 0, sigmaRs[1])
    }
    # Fishing Fleet ----

    sel50n	<-  CleanInput(sel50n, SimYear)
    sel95n	<-  CleanInput(sel95n, SimYear)
    vulnN		<- matrix(nrow = SimYear, ncol = MaxAge)

    for (i in 1:SimYear) {
      if (select_model == 'logistic') {
        vulnN[i, ]	<-
          1 / (1 + exp(-1 * log(19) * (LenAtAgeN[i, ] - sel50n[i]) / (sel95n[i] -
                                                                        sel50n[i])))
      }
      if (select_model == 'slot') {
        vulnN[i, ] <-
          as.numeric(LenAtAgeN[i, ] >= sel50n[i] &
                       LenAtAgeN[i, ] <= sel95n[i])
      }
    }
    vulnN[vulnN < 0.01] <- 0
    sel50s	<-  CleanInput(sel50s, SimYear)
    sel95s	<- CleanInput(sel95s, SimYear)
    vulnS		<- matrix(nrow = SimYear, ncol = MaxAge)
    for (i in 1:SimYear) {
      if (select_model == 'logistic') {
        vulnS[i, ]	<-
          1 / (1 + exp(-1 * log(19) * (LenAtAgeN[i, ] - sel50s[i]) / (sel95s[i] -
                                                                        sel50s[i])))
      }
      if (select_model == 'slot') {
        vulnS[i, ] <-
          as.numeric(LenAtAgeS[i, ] >= sel50s[i] &
                       LenAtAgeS[i, ] <= sel95s[i])
      }
    }
    vulnS[vulnS < 0.01] <- 0
    #==historic fishing mortality
    HistoricalF		<-   CleanInput(HistoricalF, SimYear)[1:InitYear]
    HistoricalF[is.na(HistoricalF)] <- mean(HistoricalF, na.rm = T)
    HarvestControl	<- HarvestControl #out$OM$HarvestControl
    ConstantCatch	<- ConstantCatch #out$OM$ConstantCatch
    ConstantF		<- ConstantF #out$OM$ConstantF
    HCalpha		<- HCalpha #out$OM$HCalpha
    HCbeta		<- HCbeta #out$OM$HCbeta

    # Surveys -----------------------------------------------------------------

    #==sampling uncertainty
    CatchCVn	<- CatchCVn
    CatchCVs	<- CatchCVs

    IndexCVn	<- IndexCVn
    IndexCVs	<- IndexCVs

    LenSampleN	<- LenSampleN
    LenSampleS	<- LenSampleS

    GrowthCVn	<- GrowthCV
    GrowthCVs	<- GrowthCV

    #==index selectivity=====================
    surv50n	<- CleanInput(surv50n, SimYear)
    surv95n	<-  CleanInput(surv95n, SimYear)
    survSelN		<- matrix(nrow = SimYear, ncol = MaxAge)
    for (i in 1:SimYear)
      survSelN[i, ]	<-
      1 / (1 + exp(-1 * log(19) * (LenAtAgeN[i, ] - surv50n[i]) / (surv95n[i] -
                                                                     surv50n[i])))


    surv50s	<-  CleanInput(surv50s, SimYear)
    surv95s	<-  CleanInput(surv95s, SimYear)
    survSelS	<- matrix(nrow = SimYear, ncol = MaxAge)
    for (i in 1:SimYear)
      survSelS[i, ]	<-
      1 / (1 + exp(-1 * log(19) * (LenAtAgeN[i, ] - surv50s[i]) / (surv95s[i] -
                                                                     surv50s[i])))


    #===================================================
    #==Virgin numbers at age, biomass, initial depletion, recruitment
    #===================================================
    VirInitN <- initialN(Rzero = RzeroN[1],
                         NatM = NatMn[1],
                         inAge = MaxAge)
    VirInitS <- initialN(Rzero = RzeroS[1],
                         NatM = NatMs[1],
                         inAge = MaxAge)

    q <- CleanInput(1000 / sum(VirInitN), SimYear)

    for (t in 2:SimYear) {
      q[t] <- q[t - 1] * (1 + tech_rate)
    }

    VirBioN <- sum(VirInitN * matureN[1, ] * WeightAtAgeN[1, ])
    VirBioS <- sum(VirInitS * matureS[1, ] * WeightAtAgeS[1, ])

    VirSPR <- VirBioN + VirBioS

    ExploitBioN <- sum(VirInitN * vulnN[1, ] * WeightAtAgeN[1, ])
    ExploitBioS <- sum(VirInitS * vulnS[1, ] * WeightAtAgeS[1, ])

    #========================================================================
    # FIND MSY FOR THE POPULATION (should just make a popdym function, dummy)=
    #========================================================================
    if (CalcFMSY == 1)
    {
      mort_length <- 15
      #     browser()
      #     SearchFmort		<-seq(0.01,3*NatMn[1],(NatMn[1]-0.01)/mort_length)
      SearchFmort		<- seq(0.01, 3 * NatMn[1], length.out = mort_length)

      SearchYield		<- rep(0, length(SearchFmort))
      SearchBiomass	<- rep(0, length(SearchFmort))
      SearchRevenue	<- rep(0, length(SearchFmort))
      SearchEffort	<- rep(0, length(SearchFmort))

      eqrun <- 30
      for (p in 1:length(SearchFmort))
      {
        #       show(p)

        tempNn		<- matrix(ncol = MaxAge, nrow = eqrun)
        tempNn[1, ]		<- VirInitN
        tempNs		<- matrix(ncol = MaxAge, nrow = eqrun)
        tempNs[1, ]		<- VirInitS
        tempCatchN		<- rep(0, eqrun)
        tempCatchS		<- rep(0, eqrun)
        tempRecN		<- rep(0, eqrun)
        tempRecS		<- rep(0, eqrun)
        tempCatchAtAgeN	<- matrix(ncol = MaxAge, nrow = eqrun)
        tempCatchAtAgeS	<- matrix(ncol = MaxAge, nrow = eqrun)
        tempEffortN	<- rep(0, eqrun)
        tempEffortS	<- rep(0, eqrun)
        tempRevenueN	<- rep(0, eqrun)
        tempRevenueS	<- rep(0, eqrun)
        inFs <- SearchFmort[p] * CatchShareS
        inFn <- SearchFmort[p] * CatchShareN

        for (j in 2:eqrun)
        {
          tempNn[j, 2:(MaxAge - 1)] <-
            tempNn[j - 1, 1:(MaxAge - 2)] * exp(-inFn * vulnN[1, 1:(MaxAge - 2)]) *
            exp(-NatMn[1])

          tempNs[j, 2:(MaxAge - 1)] <-
            tempNs[j - 1, 1:(MaxAge - 2)] * exp(-inFs * vulnS[1, 1:(MaxAge - 2)]) *
            exp(-NatMs[1])
          tempNn[j, MaxAge]	<-
            (tempNn[j - 1, (MaxAge - 1)]) * exp(-inFn * vulnN[1, MaxAge]) * exp(-NatMn[1]) + tempNn[j -
                                                                                                      1, MaxAge] * exp(-inFn * vulnN[1, MaxAge]) * exp(-NatMn[1])
          tempNs[j, MaxAge]	<-
            (tempNs[j - 1, (MaxAge - 1)]) * exp(-inFs * vulnS[1, MaxAge]) * exp(-NatMs[1]) + tempNs[j -
                                                                                                      1, MaxAge] * exp(-inFs * vulnS[1, MaxAge]) * exp(-NatMs[1])
          EggsN			<- sum(tempNn[j - 1, ] * matureN[1, ] * WeightAtAgeN[1, ])
          EggsS			<- sum(tempNs[j - 1, ] * matureS[1, ] * WeightAtAgeS[1, ])
          tempNn[j, 1]		<-
            Recruitment(
              EggsIN = EggsN,
              steepnessIN = steepnessN[1],
              RzeroIN = RzeroN[1],
              RecErrIN = RecErrN[1, 1],
              recType = "BH",
              NatMin = NatMn[1],
              vulnIN = vulnN[1, ],
              matureIN = matureN[1, ],
              weightIN = WeightAtAgeN[1, ],
              LenAtAgeIN = LenAtAgeN[1, ],
              MaxAge = MaxAge
            )
          tempNs[j, 1]		<-
            Recruitment(
              EggsIN = EggsS,
              steepnessIN = steepnessS[1],
              RzeroIN = RzeroS[1],
              RecErrIN = RecErrS[1, 1],
              recType = "BH",
              NatMin = NatMs[1],
              vulnIN = vulnS[1, ],
              matureIN = matureS[1, ],
              weightIN = WeightAtAgeS[1, ],
              LenAtAgeIN = LenAtAgeS[1, ],
              MaxAge = MaxAge
            )
          tempRecN[j]		<- tempNn[j, 1]
          tempRecS[j]		<- tempNs[j, 1]
          tempCatchAtAgeN[j, ]	<-
            ((vulnN[1, ] * inFn) / (vulnN[1, ] * inFn + NatMn[1])) * (1 - exp(-(vulnN[1, ] *
                                                                                  inFn + NatMn[1]))) * tempNn[j - 1, ]
          tempCatchN[j]		<- sum(tempCatchAtAgeN[j, ] * WeightAtAgeN[1, ])
          tempEffortN[j]  <- (1 - exp(-inFn)) / q[1]
          tempRevenueN[j] <-
            price * tempCatchN[j] #- cost * tempEffortN[j]^cost_beta
          tempCatchAtAgeS[j, ]	<-
            ((vulnS[1, ] * inFs) / (vulnS[1, ] * inFs + NatMs[1])) * (1 - exp(-(vulnS[1, ] *
                                                                                  inFs + NatMs[1]))) * tempNs[j - 1, ]
          tempCatchS[j]		<-
            sum(tempCatchAtAgeS[j, ] * WeightAtAgeS[1, ]) #catch in weight
          tempEffortS[j]  <- (1 - exp(-inFs)) / q[1]
          tempRevenueS[j] <-
            price * tempCatchS[j] #- cost * tempEffortS[j]^cost_beta
        }

        SearchYield[p]	<- tempCatchS[j] + tempCatchN[j]
        SearchBiomass[p]	<- EggsN + EggsS
        SearchEffort[p] <- tempEffortN[j] + tempEffortS[j]
        SearchRevenue[p] <- tempRevenueN[j] + tempRevenueS[j]
      } # close SearchFmort
      trueFMSY <- SearchFmort[which.max(SearchYield)]
      trueUMSY <- 1 - (exp(-trueFMSY))
      trueBMSY <- SearchBiomass[which.max(SearchYield)]
      trueEMSY <- SearchEffort[which.max(SearchYield)]
      trueMSY	<- max(SearchYield)

      cost <-
        (cr_ratio * SearchRevenue[which.max(SearchYield)]) / (trueEMSY ^ cost_beta)
      SearchProfits <- SearchRevenue - cost * SearchEffort ^ cost_beta

      trueFMEY <- SearchFmort[which.max(SearchProfits)]
      trueUMEY <- 1 - (exp(-trueFMEY))
      trueBMEY <- SearchBiomass[which.max(SearchProfits)]
      trueEMEY <- SearchEffort[which.max(SearchProfits)]
      trueMEY	<- SearchYield[which.max(SearchProfits)]
      trueProfitsMEY <- SearchProfits[which.max(SearchProfits)]
      trueProfitsMSY <- SearchProfits[which.max(SearchYield)]
      scaled_phi <- (phi * trueEMSY) # / (1.05 * trueProfitsMSY)

    }

    #=========================================================================
    # INITALIZE THE POPULATION
    # Option 1: Set the initial conditions by scaling the specified F series
    # that will put the population at the designated depletion
    #
    # Option 2: Set depletion to 0 and use the input time series of F to
    # initialize the population
    #==========================================================================

    if (depletion == 0)
    {
      tempNn	<- matrix(ncol = MaxAge, nrow = InitYear)
      tempNn[1, ]	<- VirInitN
      tempNs	<- matrix(ncol = MaxAge, nrow = InitYear)
      tempNs[1, ]	<- VirInitS
      tempCatchN	<- rep(0, InitYear)
      tempCatchS	<- rep(0, InitYear)
      tempEffortN	<- rep(0, InitYear)
      tempEffortS	<- rep(0, InitYear)
      tempProfitsN	<- rep(0, InitYear)
      tempProfitsS	<- rep(0, InitYear)
      tempRecN	<- rep(0, InitYear)
      tempRecS	<- rep(0, InitYear)
      tempCatchAtAgeN	<- matrix(ncol = MaxAge, nrow = InitYear)
      tempCatchAtAgeS	<- matrix(ncol = MaxAge, nrow = InitYear)
      tempSPR	<- rep(NA, InitYear)

      HistoricalF <- HistoricalF * trueFMSY
      HistoricalFs <- HistoricalF * CatchShareS
      HistoricalFn <- HistoricalF * CatchShareN

      HistoricalEfforts <- (1 - exp(-HistoricalFs)) / q
      HistoricalEffortn <- (1 - exp(-HistoricalFn)) / q

      EggsN			<- sum(tempNn[1, ] * matureN[1, ] * WeightAtAgeN[1, ])
      EggsS			<- sum(tempNs[1, ] * matureS[1, ] * WeightAtAgeS[1, ])

      tempNn[1, 1]		<-
        Recruitment(
          EggsIN = EggsN,
          steepnessIN = steepnessN[1],
          RzeroIN = RzeroN[1],
          RecErrIN = RecErrN[1, 1],
          recType = "BH",
          NatMin = NatMn[1],
          vulnIN = vulnN[1, ],
          matureIN = matureN[1, ],
          weightIN = WeightAtAgeN[1, ],
          LenAtAgeIN = LenAtAgeN[1, ],
          MaxAge = MaxAge
        )
      tempNs[1, 1]		<-
        Recruitment(
          EggsIN = EggsS,
          steepnessIN = steepnessS[1],
          RzeroIN = RzeroS[1],
          RecErrIN = RecErrS[1, 1],
          recType = "BH",
          NatMin = NatMs[1],
          vulnIN = vulnS[1, ],
          matureIN = matureS[1, ],
          weightIN = WeightAtAgeS[1, ],
          LenAtAgeIN = LenAtAgeS[1, ],
          MaxAge = MaxAge
        )

      sprN			<- sum(tempNn[1, ] * matureN[1, ] * WeightAtAgeN[1, ])
      sprS			<- sum(tempNs[1, ] * matureS[1, ] * WeightAtAgeS[1, ])
      tempSPR[1] <- sprN + sprS

      tempRecN[1]		<- tempNn[1, 1]
      tempRecS[1]		<- tempNs[1, 1]
      tempCatchAtAgeN[1, ]	<-
        ((vulnN[1, ] * HistoricalFn[1]) / (vulnN[1, ] * HistoricalFn[1] + NatMn[1])) * (1 -
                                                                                          exp(-(vulnN[1, ] * HistoricalFn[1] + NatMn[1]))) * tempNn[1, ]
      tempCatchN[1]		<- sum(tempCatchAtAgeN[1, ] * WeightAtAgeN[1, ])
      tempEffortN[1]  <- (1 - exp(-HistoricalFn[1])) / q[1]
      tempProfitsN[1] <-
        price * tempCatchN[1] - cost * tempEffortN[1] ^ cost_beta


      tempCatchAtAgeS[1, ]	<-
        ((vulnS[1, ] * HistoricalFs[1]) / (vulnS[1, ] * HistoricalFs[1] + NatMs[1])) * (1 -
                                                                                          exp(-(vulnS[1, ] * HistoricalFs[1] + NatMs[1]))) * tempNs[1, ]
      tempCatchS[1]		<- sum(tempCatchAtAgeS[1, ] * WeightAtAgeS[1, ])
      tempEffortS[1]  <- (1 - exp(-HistoricalFs[1])) / q[1]
      tempProfitsS[1] <-
        price * tempCatchS[1] - cost * tempEffortS[1] ^ cost_beta


      for (j in 2:InitYear)
      {
        if (fleetmodel != 'None') {
          HistoricalFn[j] <-
            simfleet(
              fleetmodel = fleetmodel,
              prior_profits = tempProfitsN[j - 1],
              msy_profits = trueProfitsMEY,
              prior_effort = HistoricalEffortn[j -
                                                 1],
              q = q[j],
              phi = scaled_phi
            )

          HistoricalFs[j] <-
            simfleet(
              fleetmodel = fleetmodel,
              prior_profits = tempProfitsS[j - 1],
              msy_profits = trueProfitsMEY,
              prior_effort = HistoricalEfforts[j -
                                                 1],
              q = q[j],
              phi = scaled_phi
            )
        }

        tempNn[j, 2:(MaxAge - 1)] <-
          tempNn[j - 1, 1:(MaxAge - 2)] * exp(-HistoricalFn[j] * vulnN[1, 1:(MaxAge - 2)]) *
          exp(-NatMn[1])

        tempNs[j, 2:(MaxAge - 1)] <-
          tempNs[j - 1, 1:(MaxAge - 2)] * exp(-HistoricalFs[j] * vulnS[1, 1:(MaxAge - 2)]) *
          exp(-NatMs[1])

        tempNn[j, MaxAge]	<-
          (tempNn[j - 1, (MaxAge - 1)]) * exp(-HistoricalFn[j] * vulnN[1, MaxAge]) *
          exp(-NatMn[j]) + tempNn[j - 1, MaxAge] * exp(-HistoricalFn[j] * vulnN[1, MaxAge]) *
          exp(-NatMn[j])
        tempNs[j, MaxAge]	<-
          (tempNs[j - 1, (MaxAge - 1)]) * exp(-HistoricalFs[j] * vulnS[1, MaxAge]) *
          exp(-NatMs[j]) + tempNs[j - 1, MaxAge] * exp(-HistoricalFs[j] * vulnS[1, MaxAge]) *
          exp(-NatMs[j])

        EggsN			<- sum(tempNn[j - 1, ] * matureN[1, ] * WeightAtAgeN[1, ])
        EggsS			<- sum(tempNs[j - 1, ] * matureS[1, ] * WeightAtAgeS[1, ])


        tempNn[j, 1]		<-
          Recruitment(
            EggsIN = EggsN,
            steepnessIN = steepnessN[1],
            RzeroIN = RzeroN[1],
            RecErrIN = RecErrN[1, j],
            recType = "BH",
            NatMin = NatMn[j],
            vulnIN = vulnN[1, ],
            matureIN = matureN[1, ],
            weightIN = WeightAtAgeN[1, ],
            LenAtAgeIN = LenAtAgeN[1, ],
            MaxAge = MaxAge
          )
        tempNs[j, 1]		<-
          Recruitment(
            EggsIN = EggsS,
            steepnessIN = steepnessS[1],
            RzeroIN = RzeroS[1],
            RecErrIN = RecErrS[1, j],
            recType = "BH",
            NatMin = NatMs[j],
            vulnIN = vulnS[1, ],
            matureIN = matureS[1, ],
            weightIN = WeightAtAgeS[1, ],
            LenAtAgeIN = LenAtAgeS[1, ],
            MaxAge = MaxAge
          )
        tempRecN[j]		<- tempNn[j, 1]
        tempRecS[j]		<- tempNs[j, 1]

        sprN			<- sum(tempNn[j, ] * matureN[1, ] * WeightAtAgeN[1, ])
        sprS			<- sum(tempNs[j, ] * matureS[1, ] * WeightAtAgeS[1, ])
        tempSPR[j] <- sprN + sprS
        tempCatchAtAgeN[j, ]	<-
          ((vulnN[1, ] * HistoricalFn[j]) / (vulnN[1, ] * HistoricalFn[j] + NatMn[j])) * (1 -
                                                                                            exp(-(vulnN[1, ] * HistoricalFn[j] + NatMn[j]))) * tempNn[j - 1, ]
        tempCatchN[j]		<- sum(tempCatchAtAgeN[j, ] * WeightAtAgeN[1, ])
        tempEffortN[j]  <- (1 - exp(-HistoricalFn[j])) / q[j]
        tempProfitsN[j] <-
          price * tempCatchN[j] - cost * tempEffortN[j] ^ cost_beta
        tempCatchAtAgeS[j, ]	<-
          ((vulnS[1, ] * HistoricalFs[j]) / (vulnS[1, ] * HistoricalFs[j] + NatMs[j])) * (1 -
                                                                                            exp(-(vulnS[1, ] * HistoricalFs[j] + NatMs[j]))) * tempNs[j - 1, ]
        tempCatchS[j]		<- sum(tempCatchAtAgeS[j, ] * WeightAtAgeS[1, ])
        tempEffortS[j]  <- (1 - exp(-HistoricalFs[j])) / q[j]
        tempProfitsS[j] <-
          price * tempCatchS[j] - cost * tempEffortS[j] ^ cost_beta

      } #close year loop
    }
    #===============================================================
    # BEGIN SIMULATION OF ASSESSMENT AND HARVEST
    #===============================================================
    #==tempNn and tempNs from above are the starting points
    projNn	<- array(dim = c(SimYear, MaxAge, Nsim))
    projNs	<- array(dim = c(SimYear, MaxAge, Nsim))

    projCatchAtAgeN	<- array(dim = c(SimYear, MaxAge, Nsim))
    projCatchAtAgeS	<- array(dim = c(SimYear, MaxAge, Nsim))

    projCatchN	<- matrix(nrow = Nsim, ncol = SimYear)
    projCatchS	<- matrix(nrow = Nsim, ncol = SimYear)
    projEffortN	<- matrix(nrow = Nsim, ncol = SimYear)
    projEffortS	<- matrix(nrow = Nsim, ncol = SimYear)
    projBn	<- matrix(nrow = Nsim, ncol = SimYear)
    projBs	<- matrix(nrow = Nsim, ncol = SimYear)
    projSSBn	<- matrix(nrow = Nsim, ncol = SimYear)
    projSSBs	<- matrix(nrow = Nsim, ncol = SimYear)
    projExpBn	<- matrix(nrow = Nsim, ncol = SimYear)
    projExpBs	<- matrix(nrow = Nsim, ncol = SimYear)
    projSurvN	<- matrix(nrow = Nsim, ncol = SimYear)
    projSurvS	<- matrix(nrow = Nsim, ncol = SimYear)
    projRecN	<- matrix(nrow = Nsim, ncol = SimYear)
    projRecS	<- matrix(nrow = Nsim, ncol = SimYear)
    projFmortN	<- matrix(nrow = Nsim, ncol = SimYear)
    projFmortS	<- matrix(nrow = Nsim, ncol = SimYear)

    projPopLenFreqN	<- array(dim = c(SimYear, length(LengthBinsMid), Nsim))
    projPopLenFreqS	<- array(dim = c(SimYear, length(LengthBinsMid), Nsim))
    projCatLenFreqN	<- array(dim = c(SimYear, length(LengthBinsMid), Nsim))
    projCatLenFreqS	<- array(dim = c(SimYear, length(LengthBinsMid), Nsim))
    projSurvLenFreqN	<-
      array(dim = c(SimYear, length(LengthBinsMid), Nsim))
    projSurvLenFreqS	<-
      array(dim = c(SimYear, length(LengthBinsMid), Nsim))


    #==============================================================
    # calculate the population proportion at (length) for assessment
    #==============================================================

    tempPopAtLenN <- matrix(nrow = MaxAge, ncol = length(LengthBinsMid))
    tempPopAtLenS <- matrix(nrow = MaxAge, ncol = length(LengthBinsMid))

    for (x in 1:Nsim)
      for (y in 2:InitYear)
      {
        #==make length frequencies for catch==
        for (w in 1:MaxAge)
        {
          probtemp <-
            dnorm(LengthBinsMid,
                  mean = LenAtAgeN[y, w],
                  sd = GrowthCVn * LenAtAgeN[y, w]) #Constant CV at age, so SD increases with age
          ProbN <- probtemp / sum(probtemp)
          tempPopAtLenN[w, ] <- tempNn[y, w]  * ProbN
          probtemp <-
            dnorm(LengthBinsMid,
                  mean = LenAtAgeS[y, w],
                  sd = GrowthCVs * LenAtAgeS[y, w]) #Constant CV at age, so SD increases with age
          ProbS <- probtemp / sum(probtemp)
          tempPopAtLenS[w, ] <- tempNs[y, w] * ProbS
        } #close length bin loop
        projPopLenFreqN[y, , x] <- colSums(tempPopAtLenN)
        projPopLenFreqS[y, , x] <- colSums(tempPopAtLenS)
      } # close year loop
    #==============================================================
    # calculate the catch proportion at (length) for assessment
    #==============================================================
    tempCatAtLenN <- matrix(nrow = MaxAge, ncol = length(LengthBinsMid))
    tempCatAtLenS <- matrix(nrow = MaxAge, ncol = length(LengthBinsMid))

    for (x in 1:Nsim)
      for (y in 1:InitYear)
      {
        #==make length frequencies for catch==
        for (w in 1:MaxAge)
        {
          probtemp <-
            dnorm(LengthBinsMid,
                  mean = LenAtAgeN[y, w],
                  sd = GrowthCVn * LenAtAgeN[y, w])
          ProbN <- probtemp / sum(probtemp)
          tempCatAtLenN[w, ] <- tempCatchAtAgeN[y, w]  * ProbN
          probtemp <-
            dnorm(LengthBinsMid,
                  mean = LenAtAgeS[y, w],
                  sd = GrowthCVs * LenAtAgeS[y, w])
          ProbS <- probtemp / sum(probtemp)
          tempCatAtLenS[w, ] <- tempCatchAtAgeS[y, w] * ProbS
        } #close length bin loop
        projCatLenFreqN[y, , x] <- colSums(tempCatAtLenN)
        projCatLenFreqS[y, , x] <- colSums(tempCatAtLenS)
      } # close year loop
    #==============================================================
    # calculate the catch survey proportion at length for assessment
    #==============================================================


    tempSurvAtLenN <- matrix(nrow = MaxAge, ncol = length(LengthBinsMid))
    tempSurvAtLenS <- matrix(nrow = MaxAge, ncol = length(LengthBinsMid))

    for (x in 1:Nsim)
      for (y in 2:InitYear)
      {
        #==make length frequencies for catch==
        for (w in 1:MaxAge)
        {
          probtemp <-
            dnorm(LengthBinsMid,
                  mean = LenAtAgeN[y, w],
                  sd = GrowthCVn * LenAtAgeN[y, w])
          ProbN <- probtemp / sum(probtemp)
          tempSurvAtLenN[w, ] <-
            (tempCatchAtAgeN[y, w] * (p_sampled * rlnorm(1, 0, sd_sample))) * ProbN *
            survSelN[y, w]
          probtemp <-
            dnorm(LengthBinsMid,
                  mean = LenAtAgeS[y, w],
                  sd = GrowthCVs * LenAtAgeS[y, w])
          ProbS <- probtemp / sum(probtemp)
          tempSurvAtLenS[w, ] <-
            (tempCatchAtAgeN[y, w] * (p_sampled * rlnorm(1, 0, sd_sample))) * ProbS *
            survSelS[y, w]
        } #close length bin loop
        projSurvLenFreqN[y, , x] <- colSums(tempSurvAtLenN)
        projSurvLenFreqS[y, , x] <- colSums(tempSurvAtLenS)
        #       projCatLenFreqN[y,,x]<-apply(tempCatAtLenN,2,sum)
        #       projCatLenFreqS[y,,x]<-apply(tempCatAtLenS,2,sum)
      } # close year loop
    #==transfer historical time series from above
    for (x in 1:Nsim)
    {
      projNn[1:InitYear, , x]			<- tempNn
      projNs[1:InitYear, , x]			<- tempNs
      projCatchN[x, 1:InitYear]		<- tempCatchN
      projCatchS[x, 1:InitYear]		<- tempCatchS
      projEffortN[x, 1:InitYear]		<- tempEffortN
      projEffortS[x, 1:InitYear]		<- tempEffortS
      projRecN[x, 1:InitYear]			<- tempRecN
      projRecS[x, 1:InitYear]			<- tempRecS
      projFmortN[x, 1:InitYear]		<- HistoricalFn
      projFmortS[x, 1:InitYear]		<- HistoricalFs
      projSurvN[x, 1:InitYear]			<-
        apply(tempNn * survSelN[1:InitYear, ] * WeightAtAgeN[1:InitYear, ], 1, sum)
      projSurvS[x, 1:InitYear]			<-
        apply(tempNs * survSelS[1:InitYear, ] * WeightAtAgeS[1:InitYear, ], 1, sum)
    }

    for (x in 1:InitYear)
    {
      projBn[, x] <- sum(projNn[x, , 1] * WeightAtAgeN[1, ])
      projBs[, x]	<- sum(projNs[x, , 1] * WeightAtAgeS[1, ])
      projSSBn[, x]	<- sum(projNn[x, , 1] * matureN[1, ] * WeightAtAgeN[1, ])
      projSSBs[, x]	<- sum(projNs[x, , 1] * matureS[1, ] * WeightAtAgeS[1, ])
      projExpBn[, x]	<- sum(projNn[x, , 1] * vulnN[1, ] * WeightAtAgeN[1, ])
      projExpBs[, x]	<- sum(projNs[x, , 1] * vulnS[1, ] * WeightAtAgeS[1, ])
    }

    CatchErrorN		<-
      matrix(rnorm(Nsim * SimYear, 0, CatchCVn),
             nrow = Nsim,
             ncol = SimYear)
    CatchErrorS		<-
      matrix(rnorm(Nsim * SimYear, 0, CatchCVs),
             nrow = Nsim,
             ncol = SimYear)
    CatchAssessN	<- projCatchN * exp(CatchErrorN)
    CatchAssessS	<- projCatchS * exp(CatchErrorS)

    CPUEErrorN		<-
      matrix(rnorm(Nsim * SimYear, 0, IndexCVn),
             nrow = Nsim,
             ncol = SimYear)
    CPUEErrorS		<-
      matrix(rnorm(Nsim * SimYear, 0, IndexCVs),
             nrow = Nsim,
             ncol = SimYear)
    CPUEAssessN		<- (projCatchN / projEffortN) * exp(CPUEErrorN)
    CPUEAssessS		<- (projCatchS / projEffortS) * exp(CPUEErrorS)

    SurvErrorN		<-
      matrix(rnorm(Nsim * SimYear, 0, IndexCVn),
             nrow = Nsim,
             ncol = SimYear)
    SurvErrorS		<-
      matrix(rnorm(Nsim * SimYear, 0, IndexCVs),
             nrow = Nsim,
             ncol = SimYear)
    SurvAssessN		<- projSurvN * exp(SurvErrorN)
    SurvAssessS		<- projSurvS * exp(SurvErrorS)

    #==storage for management and assessment quantities
    FMSY <- matrix(nrow = Nsim, ncol = SimYear)
    BMSY <- matrix(nrow = Nsim, ncol = SimYear)
    TAC <- matrix(nrow = Nsim, ncol = SimYear)
    CurBio <- matrix(nrow = Nsim, ncol = SimYear)

    # Tidy Up Data ------------------------------------------------------------

    # Deal with Numbers at age

    total_numbers_n <-
      plyr::adply(projNn, c(3)) %>% #convert array to dataframe with index for iteration
      dplyr::mutate(year = 1:SimYear) %>%
      gather('age', 'total_numbers', 2:(MaxAge + 1)) %>%
      dplyr::rename(iteration = X1) %>%
      dplyr::mutate(region = 'north')

    total_numbers_s <-
      plyr::adply(projNs, c(3)) %>% #convert array to dataframe with index for iteration
      dplyr::mutate(year = 1:SimYear) %>%
      gather('age', 'total_numbers', 2:(MaxAge + 1)) %>%
      dplyr::rename(iteration = X1) %>%
      dplyr::mutate(region = 'south')

    total_numbers <- rbind(total_numbers_n, total_numbers_s)
    #   numbers <- left_join(total_numbers, survey_numbers, by = c('iteration','year','age','region')) %>%
    numbers <- total_numbers %>%
      gather('number_type', 'numbers_at_age', contains('_numbers')) #%>%

    length_at_age_n <- as.data.frame(LenAtAgeN) %>%
      dplyr::mutate(year = 1:SimYear, region = 'north') %>%
      gather('age', 'mean_length', contains('V')) %>%
      dplyr::mutate(age = gsub('V', '', age))

    length_at_age_s <- as.data.frame(LenAtAgeS) %>%
      dplyr::mutate(year = 1:SimYear, region = 'south') %>%
      gather('age', 'mean_length', contains('V')) %>%
      dplyr::mutate(age = gsub('V', '', age))

    length_at_age <- rbind(length_at_age_n, length_at_age_s)

    age_structure <- numbers %>%
      left_join(length_at_age, by = c('year', 'region', 'age')) %>%
      dplyr::mutate(age = as.numeric(age)) %>%
      arrange(age)

    # Deal with length frequencies

    length_mid_key <-
      data_frame(length_bin_index = 1:length(LengthBinsMid),
                 LengthBinsMid = LengthBinsMid)

    survey_lenfreq_n <-
      plyr::adply(projSurvLenFreqN, c(3)) %>% #convert array to dataframe with index for iteration
      dplyr::mutate(year = 1:SimYear) %>%
      gather('length_bin_index', 'survey_numbers', 2:(length(LengthBinsMid) +
                                                        1)) %>%
      dplyr::rename(iteration = X1) %>%
      dplyr::mutate(region = 'north',
                    length_bin_index = as.numeric(length_bin_index)) %>%
      left_join(length_mid_key, by = 'length_bin_index') %>%
      dplyr::select(-length_bin_index)

    survey_lenfreq_s <-
      plyr::adply(projSurvLenFreqS, c(3)) %>% #convert array to dataframe with index for iteration
      dplyr::mutate(year = 1:SimYear) %>%
      gather('length_bin_index', 'survey_numbers', 2:(length(LengthBinsMid) +
                                                        1)) %>%
      dplyr::rename(iteration = X1) %>%
      dplyr::mutate(region = 'south',
                    length_bin_index = as.numeric(length_bin_index)) %>%
      left_join(length_mid_key, by = 'length_bin_index') %>%
      dplyr::select(-length_bin_index)


    survey_length_frequencies <-
      rbind(survey_lenfreq_n, survey_lenfreq_s)

    catch_lenfreq_n <-
      plyr::adply(projCatLenFreqN, c(3)) %>% #convert array to dataframe with index for iteration
      dplyr::mutate(year = 1:SimYear) %>%
      gather('length_bin_index', 'catch_numbers', 2:(length(LengthBinsMid) +
                                                       1)) %>%
      dplyr::rename(iteration = X1) %>%
      dplyr::mutate(region = 'north',
                    length_bin_index = as.numeric(length_bin_index)) %>%
      left_join(length_mid_key, by = 'length_bin_index') %>%
      dplyr::select(-length_bin_index)


    catch_lenfreq_s <-
      plyr::adply(projCatLenFreqS, c(3)) %>% #convert array to dataframe with index for iteration
      dplyr::mutate(year = 1:SimYear) %>%
      gather('length_bin_index', 'catch_numbers', 2:(length(LengthBinsMid) +
                                                       1)) %>%
      dplyr::rename(iteration = X1) %>%
      dplyr::mutate(region = 'south',
                    length_bin_index = as.numeric(length_bin_index)) %>%
      left_join(length_mid_key, by = 'length_bin_index') %>%
      dplyr::select(-length_bin_index)


    catch_length_frequencies <-
      rbind(catch_lenfreq_n, catch_lenfreq_s)

    pop_lenfreq_n <-
      plyr::adply(projPopLenFreqN, c(3)) %>% #convert array to dataframe with index for iteration
      dplyr::mutate(year = 1:SimYear) %>%
      gather('length_bin_index', 'population_numbers', 2:(length(LengthBinsMid) +
                                                            1)) %>%
      dplyr::rename(iteration = X1) %>%
      dplyr::mutate(region = 'north',
                    length_bin_index = as.numeric(length_bin_index)) %>%
      left_join(length_mid_key, by = 'length_bin_index') %>%
      dplyr::select(-length_bin_index)


    pop_lenfreq_s <-
      plyr::adply(projPopLenFreqS, c(3)) %>% #convert array to dataframe with index for iteration
      dplyr::mutate(year = 1:SimYear) %>%
      gather('length_bin_index', 'population_numbers', 2:(length(LengthBinsMid) +
                                                            1)) %>%
      dplyr::rename(iteration = X1) %>%
      dplyr::mutate(region = 'south',
                    length_bin_index = as.numeric(length_bin_index)) %>%
      left_join(length_mid_key, by = 'length_bin_index') %>%
      dplyr::select(-length_bin_index)


    population_length_frequencies <-
      rbind(pop_lenfreq_n, pop_lenfreq_s)


    length_frequencies <-
      left_join(
        catch_length_frequencies,
        survey_length_frequencies,
        by = c('iteration', 'year', 'region', 'LengthBinsMid')
      ) %>%
      left_join(
        population_length_frequencies,
        by = c('iteration', 'year', 'region', 'LengthBinsMid')
      ) %>%
      gather('lenfreq_type', 'numbers', contains('_numbers')) %>%
      dplyr::mutate(lenfreq_type = gsub("\\_.*", "", lenfreq_type))

    LengthDat <- length_frequencies %>%
      filter(lenfreq_type == 'survey') %>%
      group_by(iteration, year, LengthBinsMid) %>%
      summarise(total_numbers = sum(numbers, na.rm = T))
    # Deal with recruits

    recruits_n <-
      data_frame(
        year = 1:SimYear,
        recruits =  as.vector(projRecN),
        region = 'north'
      )

    recruits_s <-
      data_frame(
        year = 1:SimYear,
        recruits =  as.vector(projRecS),
        region = 'south'
      )

    recruits <- rbind(recruits_n, recruits_s)

    # Deal with Biomass

    biomass_n <-
      data_frame(
        year = 1:SimYear,
        region = 'north',
        total_biomass = as.vector(projBn)
      )

    biomass_s <-
      data_frame(
        year = 1:SimYear,
        region = 'south',
        total_biomass = as.vector(projBs)
      )

    total_biomass <- rbind(biomass_n, biomass_s)

    ss_biomass_n <-
      data_frame(
        year = 1:SimYear,
        region = 'north',
        ss_biomass = as.vector(projSSBn)
      )

    ss_biomass_s <-
      data_frame(
        year = 1:SimYear,
        region = 'south',
        ss_biomass = as.vector(projSSBs)
      )

    ss_biomass <- rbind(ss_biomass_n, ss_biomass_s)

    exp_biomass_n <-
      data_frame(
        year = 1:SimYear,
        region = 'north',
        exp_biomass = as.vector(projExpBn)
      )

    exp_biomass_s <-
      data_frame(
        year = 1:SimYear,
        region = 'south',
        exp_biomass = as.vector(projExpBs)
      )

    exp_biomass <- rbind(exp_biomass_n, exp_biomass_s)

    survey_biomass_n <-
      data_frame(
        year = 1:SimYear,
        region = 'north',
        survey_biomass = as.vector(SurvAssessN)
      )

    survey_biomass_s <-
      data_frame(
        year = 1:SimYear,
        region = 'south',
        survey_biomass = as.vector(SurvAssessS)
      )

    survey_biomass <- rbind(survey_biomass_n, survey_biomass_s)

    biomass <-
      left_join(total_biomass, ss_biomass, by = c('year', 'region')) %>%
      left_join(exp_biomass, by = c('year', 'region')) %>%
      left_join(survey_biomass, by = c('year', 'region')) %>%
      gather('biomass_type', 'biomass', contains('_biomass')) %>%
      dplyr::mutate(biomass_type = gsub("\\_.*", "", biomass_type))

    # Deal with catch

    catch_n <-
      data_frame(
        year = 1:SimYear,
        region = 'north',
        total_catch = as.vector(projCatchN)
      )

    catch_s <-
      data_frame(
        year = 1:SimYear,
        region = 'south',
        total_catch = as.vector(projCatchS)
      )

    total_catch <- rbind(catch_n, catch_s)

    surv_catch_n <-
      data_frame(
        year = 1:SimYear,
        region = 'north',
        survey_catch = as.vector(CatchAssessN)
      )

    surv_catch_s <-
      data_frame(
        year = 1:SimYear,
        region = 'south',
        survey_catch = as.vector(CatchAssessS)
      )

    surv_catch <- rbind(surv_catch_n, surv_catch_s)

    catch <-
      left_join(total_catch, surv_catch, by = c('year', 'region')) %>%
      gather('catch_type', 'catch', contains('_catch')) %>%
      dplyr::mutate(catch_type = gsub("\\_.*", "", catch_type))

    # Deal with SPR

    spr <-
      data_frame(year = 1:SimYear,
                 SP =  tempSPR,
                 SPR = tempSPR / VirSPR)

    spr_plot <- ggplot(spr, aes(year, SPR)) +
      geom_point(
        fill = 'red',
        color = 'black',
        shape = 21,
        size = 3
      ) +
      xlab('Year') +
      ylab('SPR') +
      plot_theme +
      ylim(c(0, 1))

    # Deal with reference points

    f_n <-
      data_frame(year = 1:SimYear,
                 f = as.vector(projFmortN),
                 region = 'north')

    f_s <-
      data_frame(year = 1:SimYear,
                 f = as.vector(projFmortS),
                 region = 'north')

    reference_points <- rbind(f_n, f_s) %>%
      left_join(biomass, by = c('year', 'region')) %>%
      left_join(catch, by = c('year', 'region')) %>%
      filter(biomass_type == 'ss' & catch_type == 'total') %>%
      group_by(year) %>%
      summarise(
        FvFmsy = mean(f / trueFMSY),
        BvBmsy = sum(biomass) / trueBMSY,
        catch_v_msy = sum(catch) / trueMSY
      )

    reference_plot <- reference_points %>%
      ggplot(aes(BvBmsy, FvFmsy, fill = catch_v_msy)) +
      scale_fill_gradient2(
        name = 'Catch/MSY',
        low = 'blue',
        mid = 'green',
        high = 'red',
        midpoint = 1,
        limits = c(0, 2)
      ) +
      geom_path(size = 0.75, alpha = 0.6) +
      geom_point(shape = 21, size = 3) +
      geom_hline(aes(yintercept = 1), linetype = 'longdash') +
      geom_vline(aes(xintercept = 1), linetype = 'longdash') +
      xlab('B/Bmsy') +
      ylab('F/Fmsy') +
      #     xlim(c(0,4)) +
      #     ylim(0,5) +
      plot_theme +
      theme(legend.text = element_text(size = 8))

    # Deal with CPUE


    cpue_n <-
      data_frame(
        year = 1:SimYear,
        region = 'north',
        cpue = as.vector(CPUEAssessN)
      )

    cpue_s <-
      data_frame(
        year = 1:SimYear,
        region = 'south',
        cpue = as.vector(CPUEAssessS)
      )

    cpue <- rbind(cpue_n, cpue_s)

    # Deal with Effort --------------------------------------------------------

    effort_n <-
      data_frame(
        year = 1:SimYear,
        region = 'north',
        effort = as.vector(tempEffortN)
      )

    effort_s <-
      data_frame(
        year = 1:SimYear,
        region = 'south',
        effort = as.vector(tempEffortS)
      )

    effort <- rbind(effort_n, effort_s) %>%
      group_by(year) %>%
      summarise(total_effort = sum(effort))

    #   browser()
    #   effort_at_age <- as.data.frame(vulnN * effort$total_effort) %>%
    #     mutate(year = 1:SimYear) %>%
    #     gather('age','effort', contains('V')) %>%
    #     dplyr::mutate(age = gsub('V','', age))


    # Catch Curves --------------------------------------------------




    length_at_age_key <-
      data_frame(age = 1:MaxAge, mean_length = LenAtAgeS[1, ])

    calc_lopt <-
      data_frame(age = 1:MaxAge,
                 virgin_bio = VirInitN * WeightAtAgeN[1, ]) %>%
      left_join(length_at_age_key, by = 'age') %>%
      mutate(max_bio = virgin_bio == max(virgin_bio, na.rm = T)) %>%
      filter(max_bio == T)

    # Calculate Pmat: proportion of catch that is mature

    lopt <- calc_lopt$mean_length

    length_50_mat <-
      LinfN[1] * (1 - exp(-VonKn[1] * (mean(c(
        mat50n, mat50s
      )) - t0n[1])))

    length_95_mat <-
      LinfN[1] * (1 - exp(-VonKn[1] * (mean(c(
        mat95n, mat95s
      )) - t0n[1])))

    Fish <-
      list(
        Linf = mean(LinfN),
        vbk = mean(VonKn),
        t0 = mean(t0n),
        LengthError = 0,
        MvK = mean(NatMn / VonKn),
        MinMvK = mean(.75 * NatMn / VonKn),
        MaxMvK = mean(1.25 * NatMn / VonKn),
        LengthMatRatio = mean(length_50_mat / LinfN),
        MinLengthMatRatio = mean(0.75 * length_50_mat / LinfN),
        MaxLengthMatRatio = mean(1.25 * length_50_mat / LinfN),
        Alpha = 0.5,
        mat50 = length_50_mat,
        mat95 = length_95_mat
      )

    catch_curve_fits <-
      catch_curve(
        LengthDat = LengthDat,
        ManualM = 1,
        Fish = Fish,
        LifeError = 0
      )

    # head(catch_curve_fits$it_results)
    catch_curve_plot <- catch_curve_fits$it_results %>%
      filter(year == max(year)) %>%
      ggplot() +
      geom_point(
        aes(age, log_counts),
        shape = 21,
        fill = 'red',
        size = 2
      ) +
      geom_line(aes(age, predicted_log_count)) +
      facet_wrap( ~ year) +
      plot_theme +
      xlab('Age') +
      ylab('Log Numbers') +
      xlim(0, MaxAge + 1) +
      geom_label(
        aes(x = MaxAge - 3, y = .9 * max(log_counts)),
        label = paste('F = ', round(
          last(catch_curve_fits$it_results$f), 2
        ), sep = ''),
        size = 12
      )

    f_trend <-
      data_frame(
        year = 1:SimYear,
        real_f = HistoricalFn,
        real_m = NatMn,
        real_fmsy = trueFMSY
      )

    damnit <- fm_ratio

    catch_curve_fvm_trend_plot <- catch_curve_fits$it_results %>%
      left_join(f_trend, by = 'year') %>%
      group_by(year) %>%
      summarise(
        mean_z = mean(z),
        mean_f = mean(f),
        mean_m = mean(m),
        mean_real_f = mean(real_f),
        mean_real_m = mean(real_m),
        mean_real_fmsy = mean(real_fmsy)
      ) %>%
      ggplot() +
      geom_point(aes(year, pmax(0, mean_f / (mean_m * damnit)), fill = 'Predicted'),
                 shape = 21,
                 size = 2) +
      geom_point(
        aes(year, mean_real_f / mean_real_fmsy, fill = 'Real'),
        shape = 21,
        size = 2
      ) +
      plot_theme +
      ylab('F/Fmsy') +
      xlab('Años') +
      geom_hline(aes(yintercept = 1), linetype = 'longdash') +
      scale_fill_discrete(name = '')

    catch_curve_trend_plot <- catch_curve_fits$it_results %>%
      left_join(f_trend, by = 'year') %>%
      group_by(year) %>%
      summarise(
        mean_z = mean(z),
        mean_f = mean(f),
        mean_m = mean(m),
        mean_real_f = mean(real_f),
        mean_real_m = mean(real_m),
        mean_real_fmsy = mean(real_fmsy)
      ) %>%
      ggplot() +
      geom_point(aes(year, pmax(0, mean_f), fill = 'Predicted'),
                 shape = 21,
                 size = 2) +
      geom_point(aes(year, mean_real_f, fill = 'Real'),
                 shape = 21,
                 size = 2) +
      plot_theme +
      ylab('F') +
      xlab('Años') +
      scale_fill_discrete(name = '')


    # Make froese indicators --------------------------------------------------
    froese_dat <- catch_length_frequencies %>%
      group_by(iteration, year, LengthBinsMid) %>%
      summarise(catch_at_length = sum(catch_numbers, na.rm = T)) %>%
      ungroup() %>%
      group_by(iteration, year) %>%
      mutate(
        total_catch = sum(catch_at_length, na.rm = T) ,
        is_mature = LengthBinsMid >= length_50_mat,
        is_opt =  LengthBinsMid >= (0.9 * lopt) &
          LengthBinsMid <= (1.1 * lopt),
        is_mega =  LengthBinsMid >= (1.1 * lopt) &
          LengthBinsMid <= mean(c(LinfN, LinfS))
      ) %>%
      ungroup() %>%
      mutate(vuln =  1 / (1 + exp(
        -1 * log(19) * (LengthBinsMid - sel50n[1]) / (sel95n[1] - sel50n[1])
      ))) %>%
      group_by(year) %>%
      mutate(scaled_vuln = vuln / sum(vuln)) %>%
      ungroup() %>%
      left_join(effort, by = 'year') %>%
      mutate(effort_at_length = total_effort * scaled_vuln)


    froese_dat$froese_cat <- NA

    froese_dat$froese_cat[froese_dat$is_mature == T] <- 'Mature'

    froese_dat$froese_cat[froese_dat$is_opt == T] <- 'Opt'

    froese_dat$froese_cat[froese_dat$is_mega == T] <- 'Mega'

    froese_dat$froese_cat[is.na(froese_dat$froese_cat)] <- 'Immature'

    cpue_by_group <- froese_dat %>%
      ungroup() %>%
      group_by(year, froese_cat) %>%
      summarise(cpue = sum(catch_at_length, na.rm = T) / sum(effort_at_length, na.rm = T))

    cpue_by_group_plot <- cpue_by_group %>%
      filter(froese_cat != 'Opt' & year > 1) %>%
      ggplot(aes(year, cpue, color = froese_cat)) +
      geom_point(
        size = 2,
        shape = 21,
        aes(fill = froese_cat),
        color = 'black'
      ) +
      geom_line() +
      xlab('Year') +
      ylab('CPUE') +
      scale_color_discrete(name = '') +
      plot_theme

    froese_indicators <-  froese_dat %>%
      mutate(
        pl_mat = (catch_at_length / total_catch) * is_mature,
        pl_opt = (catch_at_length / total_catch) * is_opt,
        pl_mega = (catch_at_length / total_catch) * is_mega
      ) %>%
      #   filter(LengthBinsMid > length_50_mat & total_catch >0)
      group_by(year) %>%
      summarise(
        p_mat = sum(pl_mat, na.rm = T),
        p_opt = sum(pl_opt, na.rm = T),
        p_mega = sum(pl_mega, na.rm = T)
      ) %>%
      ungroup() %>%
      mutate(p_obj = p_mat + p_opt + p_mega)


    joint_metrics <- biomass %>%
      filter(biomass_type == 'ss') %>%
      left_join(filter(catch, catch_type == 'total'), by = c('year', 'region')) %>%
      left_join(cpue, by = c('year', 'region')) %>%
      group_by(year) %>%
      summarise(
        total_biomass = sum(biomass),
        total_catch = sum(catch),
        mean_cpue = mean(cpue)
      ) %>%
      left_join(froese_indicators, by = c('year'))

    bvcpue_plot <-
      ggplot(joint_metrics, aes(total_biomass, mean_cpue)) +
      geom_point(
        shape = 21,
        fill = 'red',
        color = 'black',
        size = 3
      ) +
      xlab('Biomasa') +
      ylab('CPUE') +
      plot_theme +
      theme(legend.text = element_text(size = 8))


    bvcatch_plot <-
      ggplot(joint_metrics, aes(total_biomass, total_catch)) +
      geom_point(
        shape = 21,
        fill = 'red',
        color = 'black',
        size = 3
      ) +
      xlab('Biomasa') +
      ylab('CPUE') +
      plot_theme +
      theme(legend.text = element_text(size = 8))


    bvfroese_plot <- ggplot(joint_metrics, aes(total_biomass, p_obj)) +
      geom_point(
        shape = 21,
        fill = 'red',
        color = 'black',
        size = 3
      ) +
      xlab('Biomasa') +
      ylab('Froese Indicators') +
      plot_theme +
      theme(legend.text = element_text(size = 8))


    b_v_cpue_v_time_plot <- ggplot(joint_metrics) +
      geom_point(aes(year, mean_cpue / max(mean_cpue), color = 'CPUE')) +
      geom_point(aes(year, total_biomass / max(total_biomass, na.rm = T), color = 'Biomasa')) +
      plot_theme +
      xlab('Tiempo') +
      ylab('Valor') +
      scale_color_discrete(name = 'Metrico') +
      theme(legend.text = element_text(size = 8))

    cope_punt_plot <- froese_indicators %>%
      gather('Indicator', 'Value', contains('p_')) %>%
      ggplot(aes(year, Value, fill = Indicator)) +
      geom_line(aes(color = Indicator), size = 1, alpha = 0.75) +
      geom_point(shape = 21, size = 2) +
      plot_theme +
      xlab('Year') +
      ylab('Indicator')
    #   scale_fill_economist() +
    #   scale_color_economist()


    # Make Plots --------------------------------------------------------------


    length_ogive <- length_at_age %>%
      mutate(age = as.numeric(age)) %>%
      group_by(age) %>%
      summarise(mean_length = mean(mean_length, na.rm = T))

    maturity_ogive <- as.data.frame(matureS) %>%
      mutate(year = 1:SimYear) %>%
      gather('age', 'maturity', 1:MaxAge) %>%
      mutate(age = gsub('V', '', age)) %>%
      mutate(age = as.numeric(age)) %>%
      group_by(age) %>%
      summarise(mean_maturity = mean(maturity))

    weight_ogive <- as.data.frame(WeightAtAgeN) %>%
      mutate(year = 1:SimYear) %>%
      gather('age', 'weight', 1:MaxAge) %>%
      mutate(age = gsub('V', '', age)) %>%
      mutate(age = as.numeric(age)) %>%
      group_by(age) %>%
      summarise(mean_weight = mean(weight))

    movement_ogive <- as.data.frame(MovementS) %>%
      mutate(year = 1:SimYear) %>%
      gather('age', 'movement', 1:MaxAge) %>%
      mutate(age = gsub('V', '', age)) %>%
      mutate(age = as.numeric(age)) %>%
      group_by(age) %>%
      summarise(mean_movement = mean(movement))

    selectivity_ogive <- as.data.frame(vulnN) %>%
      mutate(year = 1:SimYear) %>%
      gather('age', 'selectivity', 1:MaxAge) %>%
      mutate(age = gsub('V', '', age)) %>%
      mutate(age = as.numeric(age)) %>%
      group_by(age) %>%
      summarise(mean_selectivity = mean(selectivity))

    life_plot <- length_ogive %>%
      left_join(maturity_ogive, by = 'age') %>%
      left_join(weight_ogive, by = 'age') %>%
      left_join(movement_ogive, by = 'age') %>%
      left_join(selectivity_ogive, by = 'age') %>%
      gather('metric', 'value', contains('mean_')) %>%
      ggplot(aes(age, value)) +
      geom_line(size = 2) +
      facet_wrap( ~ metric, scales = 'free_y') +
      plot_theme

    ssb <- seq(1, VirBioN, VirBioN / 100)
    record <- ssb
    for (x in 1:length(record))
    {
      record[x] <-
        Recruitment(
          EggsIN = ssb[x],
          steepnessIN = steepnessN[1],
          RzeroIN = RzeroN[1],
          RecErrIN = RecErrN[1],
          recType = "BH",
          NatMin = NatMn[1],
          vulnIN = vulnN[1, ],
          matureIN = matureN[1, ],
          weightIN = WeightAtAgeN[1, ],
          LenAtAgeIN = LenAtAgeN[1, ],
          MaxAge = MaxAge
        )
    }

    recruitment_plot <- data_frame(ssb = ssb, recruits = record) %>%
      ggplot(aes(ssb, recruits)) +
      geom_line(size = 2) +
      plot_theme

    #   plot(record~ssb)

    catch_plot <- catch %>%
      group_by(year, catch_type) %>%
      summarise(total_catch = sum(catch)) %>%
      filter(catch_type == 'total') %>%
      ggplot(aes(year, total_catch)) +
      #     geom_smooth(size = 0.75, alpha = 0.75) +
      geom_point(
        shape = 21,
        size = 2,
        fill = 'red',
        color = 'black'
      ) +
      xlab('Year') +
      ylab('Catch') +
      plot_theme

    cpue_plot <- cpue %>%
      group_by(year) %>%
      summarise(mean_cpue = mean(cpue)) %>%
      ggplot(aes(year, mean_cpue)) +
      #     geom_smooth(size = .75, alpha = 0.75) +
      geom_point(shape = 21,
                 size = 2,
                 fill = 'red') +
      xlab('Year') +
      ylab('CPUE') +
      plot_theme

    biomass_plot <- biomass %>%
      group_by(year, biomass_type) %>%
      summarise(total_biomass = sum(biomass))  %>%
      ggplot(aes(year, total_biomass, fill = biomass_type)) +
      geom_line(size = 0.75, alpha = 0.75, aes(color = biomass_type)) +
      geom_point(shape = 21,
                 alpha = 0.75,
                 size = 2) +
      xlab('Year') +
      ylab('Biomass') +
      plot_theme

    recruits_plot <- recruits %>%
      group_by(year) %>%
      summarise(total_recruits = sum(recruits))  %>%
      ggplot(aes(year, total_recruits)) +
      geom_point(
        shape = 21,
        alpha = 0.75,
        size = 3,
        fill = 'red'
      ) +
      xlab('Year') +
      ylab('# of Recruits') +
      plot_theme #+


    length_plot <- length_frequencies %>%
      ungroup() %>%
      group_by(iteration, year, LengthBinsMid, lenfreq_type) %>%
      summarize(total_numbers = sum(numbers, na.rm = T)) %>%
      ungroup() %>%
      group_by(iteration, year, lenfreq_type) %>%
      mutate(proportional_numbers = total_numbers / sum(total_numbers, na.rm = T)) %>%
      filter(is.na(proportional_numbers) == FALSE) %>%
      ggplot(aes(LengthBinsMid / 10, proportional_numbers, fill = lenfreq_type)) +
      geom_density(stat = 'identity',
                   alpha = 0.6,
                   color = 'black') +
      geom_vline(aes(xintercept = length_50_mat / 10),
                 color = 'red',
                 linetype = 'longdash') +
      scale_fill_discrete(name = 'Length Source') +
      facet_wrap( ~ year) +
      xlab('Length Bin (cm)') +
      ylab('Numbers') +
      plot_theme +
      theme(axis.text.x = element_text(size = 7)) +
      theme(axis.text.y = element_text(size = 7))



    age_structure_plot <- age_structure %>%
      ungroup() %>%
      group_by(iteration, year, age, number_type) %>%
      summarise(total_numbers = sum(numbers_at_age, na.rm = T)) %>%
      ggplot(aes(age, total_numbers, fill = number_type)) +
      geom_density(stat = 'identity',
                   alpha = 0.6,
                   color = 'black') +
      facet_wrap( ~ year) +
      xlab('Age (years)') +
      ylab('Numbers') +
      plot_theme

    local_files <- ls()

    plot_files <- local_files[grep('_plot', local_files, fixed = T)]

    plot_list <- list()

    for (i in 1:length(plot_files))
    {
      eval(parse(
        text = paste('plot_list$', plot_files[i], ' <- ', plot_files[i], sep = '')
      ))
    }


    return(
      list(
        catch_data = catch,
        cpue_data = cpue,
        age_data = age_structure,
        length_data = length_frequencies,
        biomass_data = biomass,
        plots = plot_list,
        joint_metrics = joint_metrics,
        spr = spr,
        catch_curve = catch_curve_fits,
        LengthDat = LengthDat,
        Fish = Fish,
        f_trend = f_trend,
        recruits = recruits
      )
    )
  }
