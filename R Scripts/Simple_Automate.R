require(RMySQL)
require(plyr)

rm(list = ls())
BondName = 'CGMS14-5'
TrancheName = 'D'
BondRating = 'BB'
AsOfDate = as.Date('2018-06-07')

# Db info
username = 'root'
password = 'password'
dbname = 'clo_universe'
host = 'localhost'

# Determine Type
NC_Query = paste0("SELECT `NCEDATE` FROM MONTHLY_DEAL_DATA WHERE `CDONETNAME` = '", BondName, "' AND `REPORTDATE` <= '",AsOfDate,"' ORDER BY `REPORTDATE` DESC;")
mydb = dbConnect(MySQL(), user = username, password = password, dbname = dbname, host = host)
rs = dbSendQuery(mydb, NC_Query)
NCD = fetch(rs, n = 1)
dbHasCompleted(rs)
dbClearResult(rs)
dbDisconnect(mydb)

i=0
TypeQuery1 = paste0("CREATE TEMPORARY TABLE WAC_TABLE AS ",
            "SELECT `REPORTDATE`,ROUND(SUM(`ORIGFACE` * `CPNSPRD`)/SUM(`ORIGFACE`),4) AS `WAC` FROM TRANCHES ",
            "WHERE `CDONETNAME` = '",BondName,"' AND `LABEL` NOT LIKE '%X%' AND SUBORDPRIORITY > 0 AND `REPORTDATE` <= '",AsOfDate,"' GROUP BY `REPORTDATE`;")
TypeQuery2 = paste0("CREATE TEMPORARY TABLE WAC_TABLE2 AS SELECT * FROM WAC_TABLE;")
TypeQuery3 = paste0("CREATE TEMPORARY TABLE CLEAN_WAC AS ",
            "SELECT `REPORTDATE`,WAC_TABLE.`WAC`,`CNT` FROM WAC_TABLE INNER JOIN (SELECT `WAC`,COUNT(*) AS `CNT` FROM WAC_TABLE2 GROUP BY `WAC`) T ",
            "ON WAC_TABLE.`WAC` = T.`WAC` WHERE `CNT` > 1;")
TypeQuery4 = paste0("CREATE TEMPORARY TABLE CLEAN_WAC2 AS SELECT * FROM CLEAN_WAC;")
TypeQuery5 = paste0("SELECT DISTINCT(`WAC`) FROM CLEAN_WAC;")
TypeQuery6 = paste0("SET @D_DATE = (SELECT MIN(`REPORTDATE`) FROM CLEAN_WAC WHERE `WAC` = (SELECT `WAC` FROM CLEAN_WAC2 ORDER BY `REPORTDATE` DESC LIMIT 0,1));")
TypeQuery7 = paste0("SET @DN = '", BondName, "'");
TypeQuery8 = paste0("SET @RP_DATE1 = (SELECT MIN(`REPORTDATE`) FROM MONTHLY_DEAL_DATA WHERE `CDONETNAME` = @DN ",
"AND `REPORTDATE` > DATE_ADD(@D_DATE, INTERVAL 20 DAY) AND `REDATE` IS NOT NULL);")
TypeQuery9 = paste0("SET@RP_DATE2 = (SELECT MAX(`REPORTDATE`) FROM MONTHLY_DEAL_DATA WHERE `CDONETNAME` = @DN ",
"AND `REPORTDATE` < DATE_ADD(@D_DATE, INTERVAL - 20 - 60 DAY) AND `REDATE` IS NOT NULL);")
TypeQuery10 = paste0("SET @SAME_WTF = (SELECT COUNT(*) = 0 FROM ",
"(SELECT (T1F = T2F) AS THE_SAME FROM  (SELECT `SUBORDPRIORITY`,SUM(`ORIGFACE`) AS T1F FROM TRANCHES WHERE `CDONETNAME` = @DN AND ",
"`REPORTDATE` = @RP_DATE1 AND SUBORDPRIORITY > 0 AND ISNULLIFIED = 0 AND ISCOMBINATION = 0 GROUP BY `SUBORDPRIORITY`) AS T1 ",
"LEFT JOIN (SELECT `SUBORDPRIORITY`,SUM(`ORIGFACE`) AS T2F FROM TRANCHES WHERE `CDONETNAME` = @DN AND ",
"`REPORTDATE` = @RP_DATE2 AND SUBORDPRIORITY > 0 AND ISNULLIFIED = 0 AND ISCOMBINATION = 0 GROUP BY `SUBORDPRIORITY`) AS T2 ",
"ON T1.`SUBORDPRIORITY` = T2.`SUBORDPRIORITY`) AS T_COMPARE WHERE THE_SAME <> 1);")
TypeQuery11 = paste0("SELECT @SAME_WTF")
TypeQuery12 = paste0("SELECT COUNT(DISTINCT `REDATE`) FROM monthly_deal_data WHERE CDONETNAME = @DN AND (`REPORTDATE` = @RP_DATE1 OR `REPORTDATE` = @RP_DATE2);")

mydb = dbConnect(MySQL(), user = username, password = password, dbname = dbname, host = host)
rs = dbSendQuery(mydb, TypeQuery1)
rs = dbSendQuery(mydb, TypeQuery2)
rs = dbSendQuery(mydb, TypeQuery3)
rs = dbSendQuery(mydb, TypeQuery4)
rs = dbSendQuery(mydb, TypeQuery5)
#WAC_resultLength = length(fetch(rs, n = -1)[[1]])
WAC_Changed = length(fetch(rs, n = -1)[[1]])>1
rs = dbSendQuery(mydb, TypeQuery6)
rs = dbSendQuery(mydb, TypeQuery7)
rs = dbSendQuery(mydb, TypeQuery8)
rs = dbSendQuery(mydb, TypeQuery9)
rs = dbSendQuery(mydb, TypeQuery10)
rs = dbSendQuery(mydb, TypeQuery11)
WTF_Changed = (fetch(rs, n = -1)[[1]] != 1)
if (WAC_Changed && !WTF_Changed) { i = 1 }
rs = dbSendQuery(mydb, TypeQuery12)
if (WTF_Changed && fetch(rs, n = 1)[[1]] == 1) { i = 2 }
dbHasCompleted(rs)
dbClearResult(rs)
dbDisconnect(mydb)
#i = 0
type = paste0('Type', i)


# Get Bond Info from DB
StructureQuery1 = "DROP TABLE IF EXISTS TRANCHE_INFO;"
StructureQuery2 = paste0("CREATE TEMPORARY TABLE TRANCHE_INFO AS SELECT * FROM TRANCHES WHERE `CDONETNAME` = '", BondName, "' AND ",
"`REPORTDATE` = (SELECT MAX(`REPORTDATE`) FROM TRANCHES WHERE `CDONETNAME` = '",BondName,"' AND `REPORTDATE` <= '",AsOfDate,"')",
"AND `SUBORDPRIORITY` > 0 AND `COUPONTYPE` = 'FLOAT' AND LABEL NOT LIKE '%X%';")
StructureQuery3 = paste0("ALTER TABLE TRANCHE_INFO ADD `C_RATING` VARCHAR(20);")
StructureQuery4 = paste0("UPDATE TRANCHE_INFO  SET `C_RATING` =  ",
"(SELECT IF(`ORTG-MDY` <> 'N/R',`ORTG-MDY`,IF(`ORTG-SP` <> 'N/R',`ORTG-SP`,IF(`ORTG-FH` <> 'N/R',`ORTG-FH`,",
"IF(`CRTG-MDY` <> 'N/R',`CRTG-MDY`,IF(`CRTG-SP` <> 'N/R',`CRTG-SP`,`CRTG-FH`))))));")
StructureQuery5 = paste0("ALTER TABLE TRANCHE_INFO ADD `M_RATING` VARCHAR(20);")
StructureQuery6 = paste0("UPDATE TRANCHE_INFO  SET `M_RATING` =  ",
"CONCAT((SELECT LENGTH(`C_RATING`)+(`C_RATING` REGEXP '([abc])$')-1),(SELECT LEFT(`C_RATING`,1)));")
StructureQuery7 = paste0("UPDATE TRANCHE_INFO SET `M_RATING` = (SELECT REPEAT(SUBSTRING(`M_RATING`,2,1),SUBSTRING(`M_RATING`,1,1)));")
StructureQuery8 = paste0("SELECT * FROM TRANCHE_INFO;")


mydb = dbConnect(MySQL(), user = username, password = password, dbname = dbname, host = host)
rs = dbSendQuery(mydb, StructureQuery1)
rs = dbSendQuery(mydb, StructureQuery2)
rs = dbSendQuery(mydb, StructureQuery3)
rs = dbSendQuery(mydb, StructureQuery4)
rs = dbSendQuery(mydb, StructureQuery5)
rs = dbSendQuery(mydb, StructureQuery6)
rs = dbSendQuery(mydb, StructureQuery7)
rs = dbSendQuery(mydb, StructureQuery8)
CLO.Structure = fetch(rs, n = -1)
dbHasCompleted(rs)
dbClearResult(rs)
dbDisconnect(mydb)

All.Ratings = c('AAA', 'AA', 'A', 'BBB', 'BB', 'B')

RatingSpread = ddply(CLO.Structure, .(M_RATING), function(x) data.frame(wret = weighted.mean(x$CpnSprd, x$OrigFace)))
if (c('B') %in% RatingSpread$M_RATING) {
    B_sprd = RatingSpread$wret[RatingSpread$M_RATING=='B']
} else {
    B_sprd = 8.0
}

# Get NAV from DB
NAV1_Query = paste0("SELECT `DATE`,`NAV` FROM WAP_NAV WHERE `CDONETNAME` = '", BondName, "' AND `REPORTDATE` <= '",AsOfDate,"' ORDER BY `DATE` DESC;")
mydb = dbConnect(MySQL(), user = username, password = password, dbname = dbname, host = host)
rs = dbSendQuery(mydb, NAV1_Query)
NAV1 = fetch(rs, n = 1)
dbHasCompleted(rs)
dbClearResult(rs)
dbDisconnect(mydb)

NAV2_Query = paste0("SELECT `DATE`,`NAV` FROM WAP_NAV WHERE `CDONETNAME` = '", BondName, "' AND `DATE` < '", as.Date(NAV1$DATE)-28, "' ORDER BY `DATE` DESC;")
mydb = dbConnect(MySQL(), user = username, password = password, dbname = dbname, host = host)
rs = dbSendQuery(mydb, NAV2_Query)
NAV2 = fetch(rs, n = 1)
dbHasCompleted(rs)
dbClearResult(rs)
dbDisconnect(mydb)

NAV3_Query = paste0("SELECT `DATE`,`NAV` FROM WAP_NAV WHERE `CDONETNAME` = '", BondName, "' AND `DATE` < '", as.Date(NAV1$DATE)-364, "' ORDER BY `DATE` DESC;")
mydb = dbConnect(MySQL(), user = username, password = password, dbname = dbname, host = host)
rs = dbSendQuery(mydb, NAV3_Query)
NAV3 = fetch(rs, n = 1)
dbHasCompleted(rs)
dbClearResult(rs)
dbDisconnect(mydb)

NAVs = rbind(NAV1, NAV2, NAV3)
NAVs$DAYS = as.numeric(as.Date(NAV1$DATE) - as.Date(NAVs$DATE))
NAV.1 = NA
NAV.2 = NA
NAV.3 = NA
NAV.1 = NAV1$NAV

tryCatch({
    NAV.2 = approx(x = NAVs$DAYS, y = NAVs$NAV, 28)$y
    NAV.3 = approx(x = NAVs$DAYS, y = NAVs$NAV, 364)$y
    }, error = function(err) {

    },finally = {
        if (is.na(NAV.1) || length(NAV.1) == 0) {
            NAV.1 = 50
        }

                if (is.na(NAV.2)) {
            NAV.2 = NAV.1
        }

                if (is.na(NAV.3)) {
            NAV.3 = min(90, NAV.1 + 10)
        }
    })




# Get JPM DMs
mydb = dbConnect(MySQL(), user = username, password = password, dbname = dbname, host = host)
All.JPMs = lapply(All.Ratings, function(rating) {
    JPM.query1 = paste0("SELECT `VALUE` FROM CLOIE WHERE `TICKER` = (SELECT `TICKER` FROM CLOIE_TYPE WHERE `VINTAGE` = 2 AND `RATING` = '", rating, "' AND `VALUETYPE` = 'DM') AND `DATE` <= '",AsOfDate,"' ORDER BY `DATE` DESC;")
    rs = dbSendQuery(mydb, JPM.query1)
    JPM1 = fetch(rs, n = 1)
    dbHasCompleted(rs)
    dbClearResult(rs)
    JPM.query2 = paste0("SELECT `VALUE` FROM CLOIE WHERE `TICKER` = (SELECT `TICKER` FROM CLOIE_TYPE WHERE `VINTAGE` = 2 AND `RATING` = '", rating, "' AND `VALUETYPE` = 'DM') AND `DATE` < DATE_ADD('",AsOfDate,"',INTERVAL -91 DAY) ORDER BY `DATE` DESC;")
    rs = dbSendQuery(mydb, JPM.query2)
    JPM2 = fetch(rs, n = 1)
    dbHasCompleted(rs)
    dbClearResult(rs)
    return(c(JPM1, JPM2))
})
names(All.JPMs) = All.Ratings
dbDisconnect(mydb)


# GET WAS
WAS_Query = paste0("SELECT `WAS` FROM WAS_OC_CREDIT WHERE `CDONETNAME` = '", BondName, "' AND `DATE` <= '",AsOfDate,"' ORDER BY `DATE` desc;")
mydb = dbConnect(MySQL(), user = username, password = password, dbname = dbname, host = host)
rs = dbSendQuery(mydb, WAS_Query)
WAS = fetch(rs, n = 1)
dbHasCompleted(rs)
dbClearResult(rs)
dbDisconnect(mydb)


Bond.Sample = list(
Spreads = c(
    AAA = 1.05, #RatingSpread$wret[RatingSpread$M_RATING == 'AAA'],
    AA = RatingSpread$wret[RatingSpread$M_RATING == 'AA'],
    A = RatingSpread$wret[RatingSpread$M_RATING == 'A'],
    BBB = RatingSpread$wret[RatingSpread$M_RATING == 'BBB'],
    BB = 5.7, #RatingSpread$wret[RatingSpread$M_RATING == 'BB'],
    B = B_sprd),
NAVs = c(NAV1 = NAV.1, NAV2 = NAV.2, NAV3 = NAV.3),
JPM0 = c(AAA = as.numeric(All.JPMs[['AAA']][1]), AA = as.numeric(All.JPMs[['AA']][1]), A = as.numeric(All.JPMs[['A']][1]), BBB = as.numeric(All.JPMs[['BBB']][1]), BB = as.numeric(All.JPMs[['BB']][1]), B = as.numeric(All.JPMs[['B']][1])),
JPM1 = c(AAA = as.numeric(All.JPMs[['AAA']][2]), AA = as.numeric(All.JPMs[['AA']][2]), A = as.numeric(All.JPMs[['A']][2]), BBB = as.numeric(All.JPMs[['BBB']][2]), BB = as.numeric(All.JPMs[['BB']][2]), B = as.numeric(All.JPMs[['B']][2])),
WAS = as.numeric(WAS)
)


# Run script
source('T:/CallAnalysis/CDA/CDA/script.R')

write.csv(AllBondsProbDist[['Bond.Sample']][[BondRating]][[type]], paste0(BondName, '-ProbDist', '.csv'))

