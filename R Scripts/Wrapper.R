

wd = 'T:/CallAnalysis/Yield Curve'
setwd(wd)
FileName = '0607'
BondList = read.csv(paste0(FileName,'.csv'),header =  F)
BondNames = as.character(BondList[, 1])
TrancheNames = as.character( BondList[, 2])

rtg = 'BB'

All.Lives = data.frame(DealName = as.character(), Tranche = as.character(), Type = as.character(), Exp.Life = as.numeric())

for (j in (1:(length(BondNames)))) {

    rm(list = setdiff(ls(), c('BondNames','TrancheNames','rtg','j','All.Lives','FileName','wd')))
    BondName = BondNames[j]
    TrancheName = TrancheNames[j]
    BondRating = rtg

    source('T:/CallAnalysis/CDA/CDA/Simple_Automate.R')

    All.Lives = rbind(All.Lives, data.frame(DealName = BondName, Tranche = TrancheName, Type = type, Exp.Lif = AllBondsLifes[['Bond.Sample']][[rtg]][[type]]))
}

setwd(wd)
write.csv(All.Lives, paste0(FileName, '_result.csv'))