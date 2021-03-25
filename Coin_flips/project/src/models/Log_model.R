library(caret) #http://topepo.github.io/caret/index.html
library(data.table)
library(Metrics)

set.seed(77)

train<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Coin_flips/project/volumn/data/interim/train.csv")
test<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Coin_flips/project/volumn/data/interim/test.csv")

train_y=train$result

dummies <- dummyVars(result ~., data = train)
train<-predict(dummies, newdata = train)

train<-data.table(train)
train$result<-train_y
test<-data.table(test)

glm_model<-glm(result~.,family=binomial,data=train)

summary(glm_model)

coef(glm_model)

saveRDS(dummies,"/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Coin_flips/project/volumn/models/lm.dummies")
saveRDS(glm_model,"/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Coin_flips/project/volumn/models/lm.model")

test$result<-predict(glm_model,newdata = test,type="response")

submit<-test[,.(result)]

submit$id<-seq.int(nrow(submit))

submit<-submit[,c(2,1)]

fwrite(submit,"/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Coin_flips/project/volumn/data/processed/submit.csv")


