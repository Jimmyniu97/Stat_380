library(data.table)
library(caret)
library(Metrics)
library(xgboost)

train<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/House price round 2/project/volumn/data/interim/train.csv")
test<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/House price round 2/project/volumn/data/interim/test.csv")

y.train<-train$SalePrice

dummies<-dummyVars(SalePrice~ .,data=train)
x.train<-predict(dummies,newdata=train)
x.test<-predict(dummies, newdata = test)


dtrain <- xgb.DMatrix(x.train,label=y.train,missing=NA)
dtest <- xgb.DMatrix(x.test,missing = NA)

param <- list(  objective           = "reg:linear",
                gamma               =0.02,
                booster             = "gbtree",
                eval_metric         = "rmse",
                eta                 = 0.02,
                max_depth           = 10,
                min_child_weight    = 1,
                subsample           = 1.0,
                colsample_bytree    = 1.0,
                tree_method = 'hist'
)


XGBm<-xgb.cv( params=param,nfold=5,nrounds=4000,missing=NA,data=dtrain,print_every_n=1)

watchlist <- list( train = dtrain)

XGBm<-xgb.train( params=param,nrounds=3500,missing=NA,data=dtrain,watchlist=watchlist,print_every_n=1)

pred<-predict(XGBm, newdata = dtest)

submit<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/House price round 2/project/volumn/data/raw/Stat_380_sample_submission.csv")

submit$SalePrice<-pred

fwrite(submit,"/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/House price round 2/project/volumn/data/processed/submit.csv")



