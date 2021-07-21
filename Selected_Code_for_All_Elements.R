#####ALL ELEMENTS

###DATA CLEANING
miced_All <- readRDS(file = "miced_All.Rds")
datasetOriginalAll00000 <- read.csv("collision-data.csv", header = T, stringsAsFactors = F, sep = ",", 
                                    na = c("U", "UU", "X","Q", "QQ", "XX", "UUUU", "XXXX"))

removedData <- datasetOriginalAll00000[which(!is.na(datasetOriginalAll00000$P_ISEV) & (datasetOriginalAll00000$P_ISEV != "N")),]

#Remove C_SEV (column 12) to eliminate bias over the dependent variable. Remove the dependent element: P_ISEV (column 18).
alldata <- cbind(miced_All[-c(12,18)], removedData[20])

#18 independent variables, and 1 P_ISEV dependent variable
alldata[,c(1:5,15)] <- lapply(alldata[,c(1:5,15)] , factor)
ddd <- alldata[which(alldata$P_ISEV == 3),]

##TREND
barplot(table(alldata$C_YEAR)/1000, ylab="Number of individuals in thousands", xlab="Years", ylim=c(200,400), xpd = FALSE) #all
barplot(table(ddd$C_YEAR)/1000, ylab="Number of individuals in thousands", xlab="Years", ylim=c(1,3), xpd = FALSE) ##ddd is fatal persons

#Organize P_ISEV into two categories
alldata$P_ISEV <- as.factor(alldata$P_ISEV)
levels(alldata$P_ISEV) <- list(
  "0" =  c("1", "2"), #no death
  "1" = "3" #death
)


###DATA MINING
#library(arules)
allFatal <- apriori(alldata, parameter = list(minlen = 2, support = 0.001, conf = 0.05),
                       appearance = list(rhs= c("P_ISEV=1"), default = "lhs")) #fatal person

inspect(head(sort(allFatal, by="lift")))
# lhs                       rhs        support     confidence coverage   lift      count
# [1] {P_SAFE=01,P_USER=1}   => {P_ISEV=1} 0.001117994 0.07732178 0.01445898 10.650124 7086 
# [2] {C_VEHS=one,P_SAFE=01} => {P_ISEV=1} 0.001162013 0.07168930 0.01620902  9.874319 7365 
# [3] {C_RCFG=1,P_SAFE=01}   => {P_ISEV=1} 0.001315371 0.07105055 0.01851317  9.786339 8337 
# [4] {P_PSN=11,P_SAFE=01}   => {P_ISEV=1} 0.001164222 0.06650084 0.01750688  9.159673 7379 
# [5] {C_TRAF=18,P_SAFE=01}  => {P_ISEV=1} 0.001553926 0.06553592 0.02371106  9.026767 9849 
# [6] {V_TYPE=01,P_SAFE=01}  => {P_ISEV=1} 0.001394258 0.06284715 0.02218491  8.656421 8837 


nonFatal <- apriori(alldata, parameter = list(minlen = 2, support = 0.001, conf = 0.05),
                    appearance = list(rhs= c("P_ISEV=0"), default = "lhs")) 
inspect(head(sort(nonFatal, by="lift")))


###SAMPLING AND PREDICTION MODELS
#70% training set, 30% testing test
index <- sample(1:nrow(alldata), 0.7*nrow(alldata)) 
allData.train <- alldata[index,] 
allData.test <- alldata[-index,]

##SAMPLING
#undersampling
nodeath <-  allData.train[which(allData.train$P_ISEV == 0),]
death <- allData.train[which(allData.train$P_ISEV == 1),]
alldata0 <- nodeath[sample(nrow(nodeath), 25860, replace = FALSE),]
new.train <- rbind(alldata0, death)

#oversampling
#library(ROSE)
alldata.rose <- ROSE(P_ISEV ~ ., data = allData.train, N=200000, seed = 3)$data
new.train <- alldata.rose

#both under and over sampling
alldata.both <- ovun.sample(P_ISEV ~ ., data = allData.train, method = "both", p=0.5, N=200000, seed = 3)$data
table(alldata.both$P_ISEV)
new.train <- alldata.both

##PREDICTION MODELS
#Lasso Regression
#library(caret)
#library(glmnet)
lasso <- train(P_ISEV ~., data = new.train, method = 'glmnet', trControl = trainControl(method = 'cv', number = 5))
Predictlasso <- predict(lasso,newdata = allData.test)
ConfusionMatrixLAS <- table(actual = allData.test$P_ISEV, predicted = Predictlasso)
sum(diag(ConfusionMatrixLAS))/nrow(allData.test)

#XGBoost
x <- data.matrix(new.train[-19])
y <- t(new.train[19])
ALL_XGB.test1 <- data.matrix(allData.test[-19])
#require(xgboost)
ALL_XGBModel <- xgboost(data = x, label = y, max_depth = 3,
                        eta = 0.5, nrounds = 1200, objective = 'binary:logistic') 
importance(ALL_XGBModel)
XGBPredict <- predict(ALL_XGBModel , ALL_XGB.test1)
XGBPredict<- ifelse(XGBPredict>=0.5, 1, 0)
ConfusionMatrixXGB <- table(actual = allData.test$P_ISEV, predicted = XGBPredict)
sum(diag(ConfusionMatrixXGB))/nrow(allData.test) 
ConfusionMatrixXGB

