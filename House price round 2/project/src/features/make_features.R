library(data.table)
set.seed(77)

train<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/House price round 2/project/volumn/data/raw/Stat_380_train.csv")
test<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/House price round 2/project/volumn/data/raw/Stat_380_test.csv")

train$y<-1
test$SalePrice<-1999
test$y<-0

master<-rbind(train,test)

#convert categorical data to numerical
master$BldgType<-as.numeric(as.factor(master$BldgType))
master$Heating<-as.numeric(as.factor(master$Heating))
master$CentralAir<-as.numeric(as.factor(master$CentralAir))

#standardize the data
master$LotFrontage<-scale(master$LotFrontage)
master$LotArea<-scale(master$LotArea)
master$BldgType<-scale(master$BldgType)
master$OverallQual<-scale(master$OverallQual)
master$OverallCond<-scale(master$OverallCond)
master$FullBath<-scale(master$FullBath)
master$HalfBath<-scale(master$HalfBath)
master$TotRmsAbvGrd<-scale(master$TotRmsAbvGrd)
master$YearBuilt<-scale(master$YearBuilt)
master$TotalBsmtSF<-scale(master$TotalBsmtSF)
master$BedroomAbvGr<-scale(master$BedroomAbvGr)
master$Heating<-scale(master$Heating)
master$CentralAir<-scale(master$CentralAir)
master$GrLivArea<-scale(master$GrLivArea)
master$PoolArea<-scale(master$PoolArea)
master$YrSold<-scale(master$YrSold)

#split train and test
train<-master[y==1]
test<-master[y==0]

train$y<-NULL
test$y<-NULL

fwrite(train,"/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/House price round 2/project/volumn/data/interim/train.csv")
fwrite(test,"/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/House price round 2/project/volumn/data/interim/test.csv")



