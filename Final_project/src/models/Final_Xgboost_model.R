library(data.table)
library(caret)
library(Metrics)
library(xgboost)

options(scipen = 999) 

train<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Final_project/volumn/data/interim/train.csv")
test<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Final_project/volumn/data/interim/test.csv")

train_y<-train$reddit_num

dummies<-dummyVars(reddit_num~ .,data=train)
x.train<-predict(dummies,newdata=train)
x.test<-predict(dummies, newdata = test)

dtrain <- xgb.DMatrix(x.train,label=train_y)
dtest <- xgb.DMatrix(x.test)

param <- list(  objective           = "multi:softprob",
                gamma               =0.2,
                num_class           =10,
                booster             = "gbtree",
                eval_metric         = "mlogloss",
                eta                 = 0.1,
                max_depth           = 15,
                min_child_weight    = 1,
                subsample           = 0.8,
                colsample_bytree    = 1.0,
                tree_method = 'hist'
)


XGBm<-xgb.cv( params=param,nfold=5,nrounds=9000,data=dtrain,print_every_n=10)

watchlist <- list( train = dtrain)

XGBm<-xgb.train( params=param,nrounds=9000,data=dtrain,watchlist=watchlist,print_every_n=10)

pred<-predict(XGBm, newdata = dtest,reshape=T)


pred<-data.table(pred)

submit<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Final_project/volumn/data/raw/example_sub.csv")

submit$subredditcars<-pred$V1
submit$subredditCooking<-pred$V2
submit$subredditMachineLearning<-pred$V3
submit$subredditmagicTCG<-pred$V4
submit$subredditpolitics<-pred$V5
submit$subredditReal_Estate<-pred$V6
submit$subredditscience<-pred$V7
submit$subredditStockMarket<-pred$V8
submit$subreddittravel<-pred$V9
submit$subredditvideogames<-pred$V10

fwrite(submit,"/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Final_project/volumn/data/processed/submit.csv")








