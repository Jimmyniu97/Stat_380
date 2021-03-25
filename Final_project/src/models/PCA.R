library(httr)
library(data.table)
library(Rtsne)
library(ggplot2)

set.seed(77)

train<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Final_project/volumn/data/raw/train_emb.csv")
test<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Final_project/volumn/data/raw/test_emb.csv")

tsne<-Rtsne(test,perplexity=30)

tsne_dt<-data.table(tsne$Y)

ggplot(tsne_dt,aes(x=V1,y=V2))+geom_point()
