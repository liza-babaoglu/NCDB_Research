#####COLISION ELEMENTS

###DATA CLEANING
miced_All <- readRDS(file = "miced_All.Rds")
datasetOriginalAll00000 <- read.csv("collision-data.csv", header = T, stringsAsFactors = F, sep = ",", na = c("U", "UU", "X","Q", "QQ", "XX", "UUUU", "XXXX"))

removedData <- datasetOriginalAll00000[which(!is.na(datasetOriginalAll00000$P_ISEV) & (datasetOriginalAll00000$P_ISEV != "N")),]

#11 predictors (collision elements) and 1 target label called C_SEV
#11 independent variables, and 1 P_ISEV dependent variable
data <- cbind(miced_All[1:12], removedData[23])

#selects unique collisions
collisionData <- data[!duplicated(data$C_CASE),]
summary(collisionData)
collisionData <- collisionData[-13]


###DATA MINING
allFatalCol <- apriori(collisionData, parameter = list(minlen = 2, support = 0.001, conf = 0.05),appearance = list(rhs= c("C_SEV=1"), default = "lhs")) #fatal collision

inspect(head(sort(allFatalCol, by="lift")))
#lhs                                rhs       support     confidence coverage  
#[1] {C_CONF=31,C_RCFG=1,C_RSUR=1}   => {C_SEV=1} 0.001499086 0.1376219  0.01089279
#[2] {C_CONF=31,C_RSUR=1,C_TRAF=18}  => {C_SEV=1} 0.001636038 0.1245483  0.01313577
#[3] {C_CONF=31,C_RCFG=1,C_WTHR=1}   => {C_SEV=1} 0.001490137 0.1229219  0.01212264
#[4] {C_CONF=31,C_RCFG=1,C_TRAF=18}  => {C_SEV=1} 0.002573697 0.1140321  0.02256994
#[5] {C_VEHS=two,C_CONF=31,C_RCFG=1} => {C_SEV=1} 0.002190463 0.1127714  0.01942392
#[6] {C_CONF=31,C_RCFG=1}            => {C_SEV=1} 0.002606378 0.1126507  0.02313681
#    lift     count
#[1] 8.588363 3853 
#[2] 7.772500 4205 
#[3] 7.671002 3830 
#[4] 7.116228 6615 
#[5] 7.037557 5630 
#[6] 7.030024 6699


nonFatalCol <- apriori(collisionData, parameter = list(minlen = 2, support = 0.001, conf = 0.05),appearance = list(rhs= c("C_SEV=0"), default = "lhs"))
inspect(head(sort(nonFatalCol, by="lift")))


