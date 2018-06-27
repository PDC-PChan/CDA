###################################################################
#                           Initialization
###################################################################
{
    rm(list = ls())
    wd = 'C:/ProgramData/MySQL/MySQL Server 5.7/Uploads'
    setwd(wd)
    set.seed(2018)
    mainDir = 'T:/CallAnalysis/OutputData'
    #mainDir = 'C:/Users/Samuel/Desktop/TmpAnalysis/OutputData'
    require(survival)
    require(pracma)
    require(data.table)
    #require(gbm)
    #require(caret)
    #require(ranger)
    #install.packages('gbm')
    #install.packages('ranger')
}


###################################################################
#                           Read in Data
###################################################################
{
    Data.Raw = read.csv('Data_v20180627140343.csv', header = T)
    Data.Raw$REMARK[Data.Raw$REMARK == '\\N'] <- NA
    Data = data.frame(
        CDONetName = as.character(Data.Raw$CDONETNAME),
        Label = as.character(Data.Raw$LABEL),
        Rating = as.character(Data.Raw$M_RATING),
        LSCnt = as.numeric(as.character(Data.Raw$LAST_SEEN_CNT)),
        LSRank = as.numeric(as.character(Data.Raw$LAST_SEEN_RANK)),
        LSRemark = as.factor(as.character(Data.Raw$REMARK)),
        Called = as.numeric(as.character(Data.Raw$CALLED)),
        Judge_Date = as.Date(Data.Raw$JUDGE_DATE, '%Y-%m-%d'),
        StartDate = as.Date(Data.Raw$STARTDATE, '%Y-%m-%d'),
        StartTime = as.Date(Data.Raw$START_TIME, '%Y-%m-%d'),
        ReportDate = as.Date(Data.Raw$REPORTDATE, '%Y-%m-%d'),
        WAS = as.numeric(as.character(Data.Raw$WAS)),
        WAL = as.numeric(as.character(Data.Raw$WAL)),
        CpnSprd = as.numeric(as.character(Data.Raw$CPNSPRD)),
        CpnSprd_AAA = as.numeric(as.character(Data.Raw$AAA_SPRD)),
        NAV_D1 = as.Date(Data.Raw$NAV_DATE1, '%Y-%m-%d'),
        NAV_D2 = as.Date(Data.Raw$NAV_DATE2, '%Y-%m-%d'),
        NAV_D3 = as.Date(Data.Raw$NAV_DATE3, '%Y-%m-%d'),
        NAV1 = as.numeric(as.character(Data.Raw$NAV1)),
        NAV2 = as.numeric(as.character(Data.Raw$NAV2)),
        NAV3 = as.numeric(as.character(Data.Raw$NAV3)),
        AAA_D1 = as.Date(Data.Raw$AAA_Date1, '%Y-%m-%d'),
        AAA_D2 = as.Date(Data.Raw$AAA_Date2, '%Y-%m-%d'),
        AAA1 = as.numeric(as.character(Data.Raw$AAA_DM1)),
        AAA2 = as.numeric(as.character(Data.Raw$AAA_DM2)),
        JPM_D1 = as.Date(Data.Raw$RATING_Date1, '%Y-%m-%d'),
        JPM_D2 = as.Date(Data.Raw$RATING_Date2, '%Y-%m-%d'),
        JPM1 = as.numeric(as.character(Data.Raw$RATING_DM1)),
        JPM2 = as.numeric(as.character(Data.Raw$RATING_DM2))
        )
}


###################################################################
#                           Preparing Data
###################################################################
{
    # Initalize All Types
    Data$Type = -1
    # ClsgLeg
    Data$Type[(Data$LSCnt == 1) | (Data$LSRank == 1)] = 0
    # Refi Leg
    Data$Type[(Data$LSCnt == 2 & Data$LSRank == 2 & Data$LSRemark == 1) | (Data$LSCnt == 3 & Data$LSRank == 2 & Data$LSRemark == 0)] = 1
    # Reset Leg
    Data$Type[(Data$LSCnt == 2 & Data$LSRank == 2 & Data$LSRemark == 0) | (Data$LSCnt == 3 & Data$LSRank == 3 & Data$LSRemark == 0)] = 2

    Data$StartDate[Data$Type == 0] = Data$StartDate[Data$Type == 0] + 90
    Data$AAA_Delta = Data$AAA1 - Data$CpnSprd_AAA*100
    Data$AAA_Chg = Data$AAA1 - Data$AAA2
    Data$Tranche_Delta = Data$JPM1 - Data$CpnSprd*100
    Data$Tranche_Chg = Data$JPM1 - Data$JPM2
    Data$NAV_Chg1 = Data$NAV1 - Data$NAV2
    Data$NAV_Chg2 = Data$NAV1 - Data$NAV3
    Data$tstart = pmax(as.numeric(Data$StartTime - Data$StartDate),0)
    Data$tstop = pmax(as.numeric(Data$ReportDate - Data$StartDate),0)
    Data$Death = as.numeric(((Data$Judge_Date - Data$ReportDate) < 1) & (Data$Called))
    Data$TrancheID = paste0(Data$CDONetName, '.', Data$Label)
    #Data$Age = (Data$tstop + Data$tstart) / 2
    #Data$tstart[Data$Type == 0] = pmax(Data$tstart[Data$Type == 0] - 80, 0)
}


###################################################################
#                           Fit Model Data
###################################################################
{
    Ratings = c('AAA', 'AA', 'A', 'BBB', 'BB', 'B')
    Types = c(0,1)

    # Model by Ratings
    All.Models = lapply(Ratings, function(Rating) {
        RatingModel = lapply(Types, function(i) {
            Model.Data = Data[Data$Type == i & Data$Rating == Rating,]
            if (Rating == 'AAA') {
                #return(coxph(Surv(tstart, tstop, Death) ~ CpnSprd + Tranche_Delta + Tranche_Chg + NAV1 + NAV_Chg1 + NAV_Chg2 + +cluster(TrancheID), data = Model.Data))
                #return(coxph(Surv(tstart, tstop, Death) ~ Tranche_Delta + Tranche_Chg + WAS + NAV1 + NAV_Chg1 + NAV_Chg2 + +cluster(TrancheID), data = Model.Data))
                return(coxph(Surv(tstart, tstop, Death) ~ Tranche_Delta + Tranche_Chg + WAS + WAL + NAV1 + NAV_Chg1 + NAV_Chg2 + +cluster(TrancheID), data = Model.Data))
            } else {
                #return(coxph(Surv(tstart, tstop, Death) ~ CpnSprd + Tranche_Delta + Tranche_Chg + NAV1 + NAV_Chg1 + NAV_Chg2 + +cluster(TrancheID), data = Model.Data))
                #return(coxph(Surv(tstart, tstop, Death) ~ CpnSprd_AAA + AAA_Delta + AAA_Chg + CpnSprd + Tranche_Delta + Tranche_Chg + NAV1 + NAV_Chg1 + NAV_Chg2 + +cluster(TrancheID), data = Model.Data))
                #return(coxph(Surv(tstart, tstop, Death) ~ AAA_Delta + AAA_Chg + CpnSprd + Tranche_Delta + Tranche_Chg + WAS + NAV1 + NAV_Chg1 + NAV_Chg2 + +cluster(TrancheID), data = Model.Data))
                return(coxph(Surv(tstart, tstop, Death) ~ AAA_Delta + AAA_Chg + CpnSprd + Tranche_Delta + Tranche_Chg + WAS + WAL + NAV1 + NAV_Chg1 + NAV_Chg2 + +cluster(TrancheID), data = Model.Data))
            }
            }
        )
        names(RatingModel) = paste0('Type', Types)
        return (RatingModel)
    })
    names(All.Models) = Ratings

    # Model ignoring Ratings
    Total.Models = lapply(Types, function(i) {
        Model.Data = Data[Data$Type == i ,]
        return(coxph(Surv(tstart, tstop, Death) ~ CpnSprd_AAA + AAA_Delta + AAA_Chg + CpnSprd + Tranche_Delta + Tranche_Chg + NAV1 + NAV_Chg1 + NAV_Chg2 + +cluster(TrancheID), data = Model.Data))
    })
    names(Total.Models) = paste0('Type', Types)
}


###################################################################
#                           Function declaration
###################################################################
{
    # Sensitivity
    Eval.TimePoint = c(180, 360, 540, 720)

    # Get metrics function
    GetMetrics = function(metricName) {
        Metrics = lapply(Ratings, function(Rating) {
            subMetrics = lapply(Types, function(type) summary(All.Models[[Rating]][[paste0('Type', type)]])[metricName])
            names(subMetrics) = paste0('Type', Types)
            return(subMetrics)
        })
        names(Metrics) = Ratings
        return(Metrics)
    }

    # Expected life
    expLife = function(fit.Obj) {
        return(trapz(c(0, fit.Obj$time), c(1, fit.Obj$surv)))
    }

    # Get death times
    DeathTimes = function(fit.Obj,n,by) {
        CDFunction = data.frame(Time = fit.Obj$time, CDF = 1 - fit.Obj$surv)
        set.seed(2018)
        return(approx(CDFunction$CDF,CDFunction$Time,runif(n)))
    }

    # Digest New bond Data to analys-able input
    Digest.Bond = function(Bond,Rating) {
        # Tranche_Delta + Tranche_Chg + WAS + NAV1 + NAV_Chg1 + NAV_Chg2
        # AAA_Delta + AAA_Chg + CpnSprd + Tranche_Delta + Tranche_Chg + WAS + NAV1 + NAV_Chg1 + NAV_Chg2
        AAA_Delta = Bond[['JPM0']]['AAA'] - Bond[['Spreads']]['AAA'] * 100
        AAA_Chg = Bond[['JPM0']]['AAA'] - Bond[['JPM1']]['AAA']
        CpnSprd = Bond[['Spreads']][Rating]
        Tranche_Delta = Bond[['JPM0']][Rating] - Bond[['Spreads']][Rating] * 100
        Tranche_Chg = Bond[['JPM0']][Rating] - Bond[['JPM1']][Rating]
        WAS = Bond[['WAS']]
        NAV1 = Bond[['NAVs']]['NAV1']
        NAV_Chg1 = Bond[['NAVs']]['NAV1'] - Bond[['NAVs']]['NAV2']
        NAV_Chg2 = Bond[['NAVs']]['NAV1'] - Bond[['NAVs']]['NAV3']

        return(data.frame(
                AAA_Delta,
                AAA_Chg,
                CpnSprd,
                Tranche_Delta,
                Tranche_Chg,
                WAS,
                NAV1,
                NAV_Chg1,
                NAV_Chg2
        ))
    }

    # Sample Bond
    #Bond.Sample = list(
        #Spreads = c(AAA = 1.12, AA = 1.55, A = 1.95, BBB = 3.02, BB = 5.88, B = 8.02),
        #NAVs = c(NAV1 = 27.06, NAV2 = 26.16, NAV3 = 30.65),
        #JPM0 = c(AAA = 102.15, AA = 136.37, A = 194.96, BBB = 290.5167, BB = 560.8668, B = 751.9877),
        #JPM1 = c(AAA = 91.5462, AA = 137.8065, A = 196.528, BBB = 295.1281, BB = 563.3536, B = 739.6365),
        #WAS = 3.25
        #)

    #Bond.Base = list(
        #Spreads = c(AAA = 1.17, AA = 1.9, A = 2.6, BBB = 4, BB = 7.15, B = 8.35),
        #NAVs = c(NAV1 = 48.52, NAV2 = 46.7975, NAV3 = 30),
        #JPM0 = c(AAA = 102.15, AA = 136.37, A = 194.96, BBB = 290.5167, BB = 560.8668, B = 751.9877),
        #JPM1 = c(AAA = 91.5462, AA = 137.8065, A = 196.528, BBB = 295.1281, BB = 563.3536, B = 739.6365),
        #WAS = 3.25
        #)

    #Bond.WiderAAA = list(
        #Spreads = c(AAA = 1.37, AA = 1.9, A = 2.6, BBB = 4, BB = 7.15, B = 8.35),
        #NAVs = c(NAV1 = 48.52, NAV2 = 46.7975, NAV3 = 30),
        #JPM0 = c(AAA = 102.15, AA = 136.37, A = 194.96, BBB = 290.5167, BB = 560.8668, B = 751.9877),
        #JPM1 = c(AAA = 91.5462, AA = 137.8065, A = 196.528, BBB = 295.1281, BB = 563.3536, B = 739.6365),
        #WAS = 3.25
        #)

    #Bond.TighterAAA = list(
        #Spreads = c(AAA = 1.07, AA = 1.9, A = 2.6, BBB = 4, BB = 7.15, B = 8.35),
        #NAVs = c(NAV1 = 48.52, NAV2 = 46.7975, NAV3 = 30),
        #JPM0 = c(AAA = 102.15, AA = 136.37, A = 194.96, BBB = 290.5167, BB = 560.8668, B = 751.9877),
        #JPM1 = c(AAA = 91.5462, AA = 137.8065, A = 196.528, BBB = 295.1281, BB = 563.3536, B = 739.6365),
        #WAS = 3.25
        #)

    #Bond.HighNAV = list(
        #Spreads = c(AAA = 1.17, AA = 1.9, A = 2.6, BBB = 4, BB = 7.15, B = 8.35),
        #NAVs = c(NAV1 = 68.52, NAV2 = 46.7975, NAV3 = 30),
        #JPM0 = c(AAA = 102.15, AA = 136.37, A = 194.96, BBB = 290.5167, BB = 560.8668, B = 751.9877),
        #JPM1 = c(AAA = 91.5462, AA = 137.8065, A = 196.528, BBB = 295.1281, BB = 563.3536, B = 739.6365),
        #WAS = 3.25
        #)

    #Bond.lowNAV = list(
        #Spreads = c(AAA = 1.17, AA = 1.9, A = 2.6, BBB = 4, BB = 7.15, B = 8.35),
        #NAVs = c(NAV1 = 28.52, NAV2 = 46.7975, NAV3 = 30),
        #JPM0 = c(AAA = 102.15, AA = 136.37, A = 194.96, BBB = 290.5167, BB = 560.8668, B = 751.9877),
        #JPM1 = c(AAA = 91.5462, AA = 137.8065, A = 196.528, BBB = 295.1281, BB = 563.3536, B = 739.6365),
        #WAS = 3.25
        #)

    #Bond.HighMktAAA = list(
        #Spreads = c(AAA = 1.17, AA = 1.9, A = 2.6, BBB = 4, BB = 7.15, B = 8.35),
        #NAVs = c(NAV1 = 48.52, NAV2 = 46.7975, NAV3 = 30),
        #JPM0 = c(AAA = 122.15, AA = 136.37, A = 194.96, BBB = 290.5167, BB = 560.8668, B = 751.9877),
        #JPM1 = c(AAA = 111.5462, AA = 137.8065, A = 196.528, BBB = 295.1281, BB = 563.3536, B = 739.6365),
        #WAS = 3.25
        #)

    #Bond.LowMktAAA = list(
        #Spreads = c(AAA = 1.17, AA = 1.9, A = 2.6, BBB = 4, BB = 7.15, B = 8.35),
        #NAVs = c(NAV1 = 48.52, NAV2 = 46.7975, NAV3 = 30),
        #JPM0 = c(AAA = 92.15, AA = 136.37, A = 194.96, BBB = 290.5167, BB = 560.8668, B = 751.9877),
        #JPM1 = c(AAA = 81.5462, AA = 137.8065, A = 196.528, BBB = 295.1281, BB = 563.3536, B = 739.6365),
        #WAS = 3.25
        #)

    #Bond.HighMktBB = list(
        #Spreads = c(AAA = 1.17, AA = 1.9, A = 2.6, BBB = 4, BB = 7.15, B = 8.35),
        #NAVs = c(NAV1 = 48.52, NAV2 = 46.7975, NAV3 = 30),
        #JPM0 = c(AAA = 92.15, AA = 136.37, A = 194.96, BBB = 290.5167, BB = 760.8668, B = 851.9877),
        #JPM1 = c(AAA = 91.5462, AA = 137.8065, A = 196.528, BBB = 295.1281, BB = 763.3536, B = 739.6365),
        #WAS = 3.25
        #)

    #Bond.LowMktBB = list(
        #Spreads = c(AAA = 1.17, AA = 1.9, A = 2.6, BBB = 4, BB = 7.15, B = 8.35),
        #NAVs = c(NAV1 = 48.52, NAV2 = 46.7975, NAV3 = 30),
        #JPM0 = c(AAA = 92.15, AA = 136.37, A = 194.96, BBB = 290.5167, BB = 510.8668, B = 851.9877),
        #JPM1 = c(AAA = 91.5462, AA = 137.8065, A = 196.528, BBB = 295.1281, BB = 513.3536, B = 739.6365),
        #WAS = 3.25
        #)

    #Bond.HighWAS = list(
        #Spreads = c(AAA = 1.17, AA = 1.9, A = 2.6, BBB = 4, BB = 7.15, B = 8.35),
        #NAVs = c(NAV1 = 48.52, NAV2 = 46.7975, NAV3 = 30),
        #JPM0 = c(AAA = 92.15, AA = 136.37, A = 194.96, BBB = 290.5167, BB = 510.8668, B = 851.9877),
        #JPM1 = c(AAA = 91.5462, AA = 137.8065, A = 196.528, BBB = 295.1281, BB = 563.3536, B = 739.6365),
        #WAS = 4.00
        #)

    #Bond.LowWAS = list(
        #Spreads = c(AAA = 1.17, AA = 1.9, A = 2.6, BBB = 4, BB = 7.15, B = 8.35),
        #NAVs = c(NAV1 = 48.52, NAV2 = 46.7975, NAV3 = 30),
        #JPM0 = c(AAA = 92.15, AA = 136.37, A = 194.96, BBB = 290.5167, BB = 510.8668, B = 851.9877),
        #JPM1 = c(AAA = 91.5462, AA = 137.8065, A = 196.528, BBB = 295.1281, BB = 563.3536, B = 739.6365),
        #WAS = 2.75
        #)

    #BondList = c(
        #'Bond.Base',
        #'Bond.WiderAAA','Bond.TighterAAA',
        #'Bond.HighNAV','Bond.lowNAV',
        #'Bond.HighMktAAA','Bond.LowMktAAA',
        #'Bond.HighMktBB','Bond.LowMktBB',
        #'Bond.HighWAS', 'Bond.LowWAS')

    BondList = c('Bond.Sample')

    All.Bonds = mget(BondList)
}


###################################################################
#                     Evaluate Model - Basic info
###################################################################
{

    # All Concordance
    All.CI = lapply(Ratings, function(Rating) {
        subMetrics = lapply(Types, function(type)(summary(All.Models[[Rating]][[paste0('Type', type)]])$concordance)[1])
        names(subMetrics) = paste0('Type', Types)
        return(subMetrics)
    })
    names(All.CI) = Ratings

    # All P values
    All.PValues = lapply(Ratings, function(Rating) {
        subMetrics = lapply(Types, function(type)(summary(All.Models[[Rating]][[paste0('Type', type)]])$coefficients)[, 6])
        names(subMetrics) = paste0('Type', Types)
        return(subMetrics)
    })
    names(All.PValues) = Ratings


    # All Fitted objects
    All.Fits = lapply(Ratings, function(Rating) {
        Fits = lapply(Types, function(i) {
            Model.Data = Data[Data$Type == i & Data$Rating == Rating,]
            mFit = survfit(All.Models[[Rating]][[paste0('Type', i)]], data = Model.Data)
        }
        )
        names(Fits) = paste0('Type', Types)
        return(Fits)
    })
    names(All.Fits) = Ratings


    # All Expected life
    All.ExpectedLife = lapply(Ratings, function(Rating) {
        ExLife = lapply(Types, function(i) {
            return(expLife(All.Fits[[Rating]][[paste0('Type', i)]]))
        }
        )
        names(ExLife) = paste0('Type', Types)
        return(ExLife)
    })
    names(All.ExpectedLife) = Ratings


}


###################################################################
#                           Sensitivity Analysis
###################################################################
{
    # Construct Centered Values
    Centered.Values = list(
        AAA = list(Type0 = as.vector(Digest.Bond(Bond.Sample, 'AAA')), Type1 = as.vector(Digest.Bond(Bond.Sample, 'AAA'))),
        AA = list(Type0 = as.vector(Digest.Bond(Bond.Sample, 'AA')), Type1 = as.vector(Digest.Bond(Bond.Sample, 'AA'))),
        A = list(Type0 = as.vector(Digest.Bond(Bond.Sample, 'A')), Type1 = as.vector(Digest.Bond(Bond.Sample, 'A'))),
        BBB = list(Type0 = as.vector(Digest.Bond(Bond.Sample, 'BBB')), Type1 = as.vector(Digest.Bond(Bond.Sample, 'BBB'))),
        BB = list(Type0 = as.vector(Digest.Bond(Bond.Sample, 'BB')), Type1 = as.vector(Digest.Bond(Bond.Sample, 'BB'))),
        B = list(Type0 = as.vector(Digest.Bond(Bond.Sample, 'B')), Type1 = as.vector(Digest.Bond(Bond.Sample, 'B')))
        )

    # Constructed Variations
    AAA.Var = c(AAA_Delta = -5, AAA_Chg = 0, CpnSprd = .05, Tranche_Delta = -5, Tranche_Chg = 5, WAS = -.10, NAV1 = 0, NAV_Chg1 = 0, NAV_Chg2 = 10)
    IG.Var = c(AAA_Delta = -5, AAA_Chg = 0, CpnSprd = .05, Tranche_Delta = -5, Tranche_Chg = 5, WAS = -.10, NAV1 = 0, NAV_Chg1 = 0, NAV_Chg2 = 10)
    HY.Var = c(AAA_Delta = -5, AAA_Chg = 0, CpnSprd = 0, Tranche_Delta = -5, Tranche_Chg = 5, WAS = -.10, NAV1 = 0, NAV_Chg1 = 0, NAV_Chg2 = 10)
    Delta.Values = list(
        AAA = list(Type0 = AAA.Var, Type1 = AAA.Var),
        AA = list(Type0 = IG.Var, Type1 = IG.Var),
        A = list(Type0 = IG.Var, Type1 = IG.Var),
        BBB = list(Type0 = IG.Var, Type1 = IG.Var),
        BB = list(Type0 = HY.Var, Type1 = HY.Var),
        B = list(Type0 = HY.Var, Type1 = HY.Var)
        )

    
    Multiplier = 2

    # Surv Prob Sensitivity
    AllSurvProb = lapply(Ratings, function(Rating) {
        SurvProbs = lapply(Types, function(i) {

            Type = paste0('Type', i)

            Deltas = Delta.Values[[Rating]][[Type]]

            CreateVariations = function(Rating, Type, multiplier) {
                Center.Vec = as.numeric(Centered.Values[[Rating]][[Type]])
                Delta.Matrix = t(cbind(multiplier * diag(Deltas)))
                DF = data.frame(matrix(rep(Center.Vec, nrow(Delta.Matrix)), nrow = nrow(Delta.Matrix), byrow = T) + Delta.Matrix)
                names(DF) = names(Deltas)
                return(DF)
            }

            Prob_Positive = survfit(All.Models[[Rating]][[Type]], newdata = CreateVariations(Rating, Type, Multiplier))

            SurvProb = data.frame(summary(Prob_Positive, times = Eval.TimePoint)$surv)
            names(SurvProb) = names(Deltas)
            rownames(SurvProb) = Eval.TimePoint
            return(SurvProb)
        })
        names(SurvProbs) = paste0('Type', Types)
        return(SurvProbs)
    })
    names(AllSurvProb) = Ratings

    # Surv Prob Delta Sensitivity
    AllSurvProb.Delta = lapply(Ratings, function(Rating) {
        SurvProbs = lapply(Types, function(i) {

            Type = paste0('Type', i)

            Deltas = Delta.Values[[Rating]][[Type]]

            CreateVariations = function(Rating, Type, multiplier) {
                Center.Vec = as.numeric(Centered.Values[[Rating]][[Type]])
                Delta.Matrix = t(multiplier * diag(Deltas))
                DF = data.frame(matrix(rep(Center.Vec, nrow(Delta.Matrix)), nrow = nrow(Delta.Matrix), byrow = T) + Delta.Matrix)
                names(DF) = names(Deltas)
                return(DF)
            }

            Prob_Positive = survfit(All.Models[[Rating]][[Type]], newdata = CreateVariations(Rating, Type, Multiplier))
            Prob_Negative = survfit(All.Models[[Rating]][[Type]], newdata = CreateVariations(Rating, Type, - Multiplier))

            SurvProb = data.frame((summary(Prob_Positive, times = Eval.TimePoint)$surv - summary(Prob_Negative, times = Eval.TimePoint)$surv)/2)
            names(SurvProb) = names(Deltas)
            return(SurvProb)
        })
        names(SurvProbs) = paste0('Type', Types)
        return(SurvProbs)
    })
    names(AllSurvProb.Delta) = Ratings

    # Expected Life Sensitivity
    AllSurvExp = lapply(Ratings, function(Rating) {

        SurvExps = lapply(Types, function(i) {

            Type = paste0('Type', i)

            Deltas = Delta.Values[[Rating]][[Type]]

            CreateVariations = function(Rating, Type, multiplier) {
                Center.Vec = as.numeric(Centered.Values[[Rating]][[Type]])
                Delta.Matrix = t(multiplier * diag(Deltas))
                DF = data.frame(matrix(rep(Center.Vec, nrow(Delta.Matrix)), nrow = nrow(Delta.Matrix), byrow = T) + Delta.Matrix)
                names(DF) = names(Deltas)
                return(DF)
            }

            Prob_Positive = survfit(All.Models[[Rating]][[Type]], newdata = CreateVariations(Rating, Type, Multiplier))

            SurvProb = sapply(1:length(Centered.Values[[Rating]][[Type]]), function(i) {
                return(expLife(Prob_Positive[i]))
            })
            names(SurvProb) = names(Deltas)
            return(SurvProb)
        })
        names(SurvExps) = paste0('Type', Types)
        return(SurvExps)
    })
    names(AllSurvExp) = Ratings

    # Expected Life Delta Sensitivity
    AllSurvExp.Delta = lapply(Ratings, function(Rating) {

        SurvExps = lapply(Types, function(i) {

            Type = paste0('Type', i)

            Deltas = Delta.Values[[Rating]][[Type]]

            CreateVariations = function(Rating, Type, multiplier) {
                Center.Vec = as.numeric(Centered.Values[[Rating]][[Type]])
                Delta.Matrix = t(cbind(multiplier * diag(Deltas), rep(0, length(Deltas))))
                DF = data.frame(matrix(rep(Center.Vec, nrow(Delta.Matrix)), nrow = nrow(Delta.Matrix), byrow = T) + Delta.Matrix)
                names(DF) = names(Deltas)
                return(DF)
            }

            Prob_Positive = survfit(All.Models[[Rating]][[Type]], newdata = CreateVariations(Rating, Type, Multiplier))
            Prob_Negative = survfit(All.Models[[Rating]][[Type]], newdata = CreateVariations(Rating, Type, - Multiplier))

            SurvProb = sapply(1:length(Centered.Values[[Rating]][[Type]]), function(i) {
                return(expLife(Prob_Positive[i]) - expLife(Prob_Negative[i])) / 2
            })
            names(SurvProb) = names(Deltas)
            return(SurvProb)
        })
        names(SurvExps) = paste0('Type', Types)
        return(SurvExps)
    })
    names(AllSurvExp.Delta) = Ratings
}


###################################################################
#                           Pricing
###################################################################
{
    # Get Prob dist of each tranche
    All.Prob.Dist.Function = function(MyBond) {
        Prob.Dists = lapply(Ratings, function(Rating) {
            SurvExps = lapply(Types, function(i) {
                Type = paste0('Type', i)
                sf = survfit(All.Models[[Rating]][[Type]], newdata = Digest.Bond(MyBond, Rating))

                # Obtain Data
                CDF = data.frame(Time = summary(sf)$time, cp = 1 - summary(sf)$surv)
                CDF = rbind(c(0, 0), CDF, c(max(CDF$Time) + 1, 1))
                set.seed(2018)
                Samples = approx(x = CDF$cp, y = CDF$Time, runif(100000))$y

                # Smoothen distribution
                adjustV = 2.2
                kernelType = "cosine"
                N = ceiling(max(Samples, na.rm = T) / 90) + 1
                split = seq(0, by = 90, length.out = N)
                h <- density(Samples, kernel = kernelType, adjust = adjustV)$bw
                w <- 1 / pnorm(0, mean = Samples, sd = h, lower.tail = FALSE)
                d <- density(Samples, bw = h, kernel = kernelType, weights = w / length(Samples), adjust = adjustV)
                d$y[d$Samples < 0] <- 0
                d.New = data.frame(x = d$x, y = d$y)
                d.Anchors = data.frame(x = split)
                d.Anchors$y = approx(x = d$x, y = d$y, d.Anchors$x)$y
                d.New = rbind(d.New, d.Anchors)
                d.New = d.New[order(d.New$x),]

                # Consolidate and return Results
                Bucket = data.frame(tStart = split[-N], tEnd = split[-1])
                Ps = apply(Bucket, 1, function(x) {
                    a = x['tStart']
                    b = x['tEnd']
                    Xs = d.New$x[d.New$x >= a & d.New$x <= b]
                    Ys = d.New$y[d.New$x >= a & d.New$x <= b]
                    return(trapz(Xs, Ys))
                })
                Bucket$Ps = Ps / sum(Ps)
                return(Bucket)
            })
            names(SurvExps) = paste0('Type', Types)
            return(SurvExps)
        })
        names(Prob.Dists) = Ratings
        return(Prob.Dists)
    }


    # Get Ex Life of each Bond
    All.Lifes.Function = function(MyBond) {
        Lifes = lapply(Ratings, function(Rating) {
            SurvExps = lapply(Types, function(i) {

                Type = paste0('Type', i)
                sf = survfit(All.Models[[Rating]][[Type]], newdata = Digest.Bond(MyBond, Rating))

                return(expLife(sf))
            })
            names(SurvExps) = paste0('Type', Types)
            return(SurvExps)
        })
        names(Lifes) = Ratings
        return(Lifes)
    }

}



###################################################################
#                           Output Results
###################################################################
{
    dir.create(file.path(mainDir, Sys.Date()), showWarnings = FALSE)
    setwd(file.path(mainDir, Sys.Date()))

    ## Write P.Values
    #AAA.P_Values = do.call('rbind', All.PValues[['AAA']])
    #Other.P_Values  = rbind(
            #do.call('rbind', All.PValues[['AA']]),
            #do.call('rbind', All.PValues[['A']]),
            #do.call('rbind', All.PValues[['BBB']]),
            #do.call('rbind', All.PValues[['BB']]),
            #do.call('rbind', All.PValues[['B']]))
    #All.P_Values = rbind(
        #AAA.P_Values[, match(colnames(Other.P_Values), colnames(AAA.P_Values))],
        #Other.P_Values)
    #colnames(All.P_Values) = colnames(Other.P_Values)
    #write.csv(All.P_Values, 'All_PValues.csv')

    ## Write Concordance
    #All.Concordance = rbind(
        #do.call(rbind, All.CI[['AAA']]),
        #do.call(rbind, All.CI[['AA']]),
        #do.call(rbind, All.CI[['A']]),
        #do.call(rbind, All.CI[['BBB']]),
        #do.call(rbind, All.CI[['BB']]),
        #do.call(rbind, All.CI[['B']])
        #)
    #write.csv(All.Concordance, 'All_Concordance.csv')

    ## Write Factor's Sensitivity
    #All.Sensitivity = do.call('rbind', lapply(
        #Ratings, function(Rating) {
            #return(do.call('rbind', AllSurvProb[[Rating]]))
        #}
    #))
    #write.csv(All.Sensitivity, 'All_Sensitivity.csv')

    ## Write Factor's Expected Life
    #All.ExpLifes = do.call('rbind', lapply(
        #Ratings, function(Rating) {
            #return(do.call('rbind', AllSurvExp[[Rating]]))
        #}
    #))
    #write.csv(All.ExpLifes, 'All_ExpLifes.csv')


    # Write Bonds Life Distrbtuions
    AllBondsProbDist = lapply(All.Bonds, All.Prob.Dist.Function)
    AllBondsLifes = lapply(All.Bonds, All.Lifes.Function)
    Life.DF = data.frame(rbindlist(AllBondsLifes, fill = TRUE))
    #write.csv(apply(Life.DF, 2, as.character), 'All_Bond_Lifes.csv')
    #dummy = lapply(BondList, function(BondName) {
        #write.csv(AllBondsProbDist[[BondName]][['AAA']][['Type0']], paste0('BondDist_AAA_Type0-',BondName,'.csv'))
        #write.csv(AllBondsProbDist[[BondName]][['BB']][['Type0']], paste0('BondDist_BB_Type0-',BondName,'.csv'))
    #})

}
