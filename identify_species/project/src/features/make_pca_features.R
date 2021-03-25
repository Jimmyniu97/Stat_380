library(data.table)
library(caret)

set.seed(1234)

raw<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/identify_species/project/volumn/data/raw/Gene_data.csv")

raw$id<-NULL

raw$locus_1<-as.character(raw$locus_1)                                                                                                 
raw$locus_2<-as.character(raw$locus_2)                                                                                                 
raw$locus_3<-as.character(raw$locus_3)                                                                                                 
raw$locus_4<-as.character(raw$locus_4)                                                                                                 
raw$locus_5<-as.character(raw$locus_5)                                                                                                 
raw$locus_6<-as.character(raw$locus_6)                                                                                                 
raw$locus_7<-as.character(raw$locus_7)                                                                                                 
raw$locus_8<-as.character(raw$locus_8)                                                                                                 
raw$locus_9<-as.character(raw$locus_9)                                                                                                 
raw$locus_10<-as.character(raw$locus_10)                                                                                               
raw$locus_11<-as.character(raw$locus_11)                                                                                               
raw$locus_12<-as.character(raw$locus_12)                                                                                               
raw$locus_13<-as.character(raw$locus_13)                                                                                               
raw$locus_14<-as.character(raw$locus_14)                                                                                               
raw$locus_15<-as.character(raw$locus_15) 
raw$y<-0.3

dummies<-dummyVars(y~.,data=raw)
data<-predict(dummies,newdata=raw)
data<-data.table(data)

fwrite(data,"/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/identify_species/project/volumn/data/interim/data.csv")

