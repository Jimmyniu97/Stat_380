library(data.table)
library(ISLR)
set.seed(77)

train<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Coin_flips/project/volumn/data/raw/train_file.csv")
test<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Coin_flips/project/volumn/data/raw/test_file.csv")

train$id<-NULL
test$id<-NULL

fwrite(train,"/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Coin_flips/project/volumn/data/interim/train.csv")
fwrite(test,"/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Coin_flips/project/volumn/data/interim/test.csv")
