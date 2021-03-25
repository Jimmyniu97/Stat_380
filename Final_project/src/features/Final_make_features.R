library(httr)
library(data.table)
library(Rtsne)
library(ClusterR)
library(ggplot2)

set.seed(77)


train<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Final_project/volumn/data/raw/training_data.csv")
train_emb<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Final_project/volumn/data/raw/train_emb.csv")
test<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Final_project/volumn/data/raw/test_file.csv")
test_emb<-fread("/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Final_project/volumn/data/raw/test_emb.csv")

train$id<-sub(".+?_","",train$id)
train_target<-melt(data=train,id=c("id","text"))
train_tidy<-train_target[(train_target$value==1),]
train_tidy$value<-NULL
train_tidy$id<-as.integer(train_tidy$id)
train_tidy<-train_tidy[order(id)]

#convert the categorical data into numerical data
setnames(train_tidy,"variable","reddit_num")
train_tidy[train_tidy$reddit_num=="subredditcars"]$reddit_num<-'0'
train_tidy[train_tidy$reddit_num=="subredditCooking"]$reddit_num<-'1'
train_tidy[train_tidy$reddit_num=="subredditMachineLearning"]$reddit_num<-'2'
train_tidy[train_tidy$reddit_num=="subredditmagicTCG"]$reddit_num<-'3'
train_tidy[train_tidy$reddit_num=="subredditpolitics"]$reddit_num<-'4'
train_tidy[train_tidy$reddit_num=="subredditReal_Estate"]$reddit_num<-'5'
train_tidy[train_tidy$reddit_num=="subredditscience"]$reddit_num<-'6'
train_tidy[train_tidy$reddit_num=="subredditStockMarket"]$reddit_num<-'7'
train_tidy[train_tidy$reddit_num=="subreddittravel"]$reddit_num<-'8'
train_tidy[train_tidy$reddit_num=="subredditvideogames"]$reddit_num<-'9'


#create the master file
train2<-cbind(train_tidy,train_emb)
test2<-cbind(test,test_emb)
test2$reddit_num<-"11"
master<-rbind(train2,test2)

#create tsne columns
j_data<-data.frame(lapply(master[,4:515], jitter,factor=0.000001))
pca<-prcomp(j_data)
pca_dt<-data.table(unclass(pca)$x)
tsne<-Rtsne(pca_dt,pca = F,dims=3,perplexity=110)
tsne_dt<-data.table(tsne$Y)

k_bic<-Optimal_Clusters_GMM(tsne_dt,max_clusters = 10,criterion = "BIC")
delta_k<-k_bic[-1] - k_bic[-10]
del_k_tab<-data.table(delta_k=delta_k,k=2:10)
ggplot(del_k_tab,aes(x=k,y=-delta_k))+geom_point()+geom_line()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_text(aes(label=k),hjust=0, vjust=-1)

master$tsne1<-tsne_dt$V1
master$tsne2<-tsne_dt$V2
master$tsne3<-tsne_dt$V3
master<-master[,.(tsne1,tsne2,tsne3,reddit_num)]
master<-cbind(master,pca_dt[,1:10])

#split the train and test file
train3<-master[!(reddit_num=='11')]
test3<-master[reddit_num=='11']

fwrite(train3,"/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Final_project/volumn/data/interim/train.csv")
fwrite(test3,"/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/Final_project/volumn/data/interim/test.csv")
