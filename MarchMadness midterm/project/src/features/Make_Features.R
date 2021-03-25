library(data.table)
set.seed(77)

test<-fread('/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/MarchMadness midterm/project/volumn/data/raw/example_sub.csv')
season<-fread('/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/MarchMadness midterm/project/volumn/data/raw/season.csv')
tourney<-fread('/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/MarchMadness midterm/project/volumn/data/raw/tourney.csv')
ranks<-fread('/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/MarchMadness midterm/project/volumn/data/raw/ranks.csv')

# Clean test

#########
#add this to deal with the new format of the id column
#this removes everything before the first underscore

test$id<-sub(".+?_","",test$id)

#I do it again to remove the year
test$id<-sub(".+?_","",test$id)

test<-data.table(matrix(unlist(strsplit(test$id,"_")),ncol=2,byrow=T))
setnames(test,c("V1","V2"),c("team_1","team_2"))

test$Season<-2019
test$DayNum<-120

# in order to keep track of the order of the test file I add in this column

test$id_num<-1:nrow(test)

test<-test[,.(id_num,team_1,team_2,Season,DayNum)]

test$result<-0.5

# make train

train<-rbind(season,tourney)
train<-train[,.(WTeamID,LTeamID,Season,DayNum)]
setnames(train,c("WTeamID","LTeamID"),c("team_1","team_2"))

train$result<-1

train$id_num<-1:nrow(train)

# make master data file

master<-rbind(train,test)
master$team_1<-as.character(master$team_1)
master$team_2<-as.character(master$team_2)

ranks$DayNum<-ranks$RankingDayNum+1


system_lst<-c("POM","PIG","SAG","MOR","DOK")

for (i in 1:length(system_lst)){
  
  one_rank<-ranks[SystemName==system_lst[i]][,.(Season,DayNum,TeamID,OrdinalRank)]
  setnames(one_rank,"TeamID","team_1")
  
  one_rank$team_1<-as.character(one_rank$team_1)
  
  setkey(master,Season,team_1,DayNum)
  setkey(one_rank,Season,team_1,DayNum)
  
  master<-one_rank[master,roll=T]
  setnames(master,"OrdinalRank","team_1_rank")
  
  
  setnames(one_rank,"team_1","team_2")
  setkey(master,Season,team_2,DayNum)
  setkey(one_rank,Season,team_2,DayNum)
  
  master<-one_rank[master,roll=T]
  
  setnames(master,"OrdinalRank","team_2_rank")
  
  master$rank_dif<-master$team_2_rank-master$team_1_rank
  
  master$team_1_rank<-NULL
  master$team_2_rank<-NULL
  
  setnames(master,"rank_dif",paste0(system_lst[i],"_dif"))
  
}

master<-master[order(Season,DayNum)]

master<-master[,.(id_num,Season,DayNum,team_1,team_2,POM_dif,PIG_dif, SAG_dif,MOR_dif,DOK_dif,result)]

# make all the NA values zero, there may be a better way to deal with NAs

master[is.na(master$POM_dif)]$POM_dif<-0
master[is.na(master$PIG_dif)]$PIG_dif<-0
master[is.na(master$SAG_dif)]$SAG_dif<-0
master[is.na(master$MOR_dif)]$MOR_dif<-0
master[is.na(master$DOK_dif)]$DOK_dif<-0

#add the individual game statistical diff
all_games<-rbind(season,tourney)
Wstatsgames<-all_games[,.(Season,DayNum,WTeamID,WScore,WLoc,NumOT,WFGM,WFGA,WFGM3,WFGA3,WFTM,WFTA,WOR,WDR,WAst,WTO,WStl,WBlk,WPF)]
Lstatsgames<-all_games[,.(Season,DayNum,LTeamID,LScore,WLoc,NumOT,LFGM,LFGA,LFGM3,LFGA3,LFTM,LFTA,LOR,LDR,LAst,LTO,LStl,LBlk,LPF)]
setnames(Wstatsgames,c("WTeamID","WScore","WLoc","NumOT","WFGM","WFGA","WFGM3","WFGA3","WFTM","WFTA","WOR","WDR","WAst","WTO","WStl","WBlk","WPF"),
         c("TeamID","Score","Loc","NumOT","FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF"))
setnames(Lstatsgames,c("LTeamID","LScore","WLoc","NumOT","LFGM","LFGA","LFGM3","LFGA3","LFTM","LFTA","LOR","LDR","LAst","LTO","LStl","LBlk","LPF"),
         c("TeamID","Score","Loc","NumOT","FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF"))

#make the master stats table
master_stats<-rbind(Wstatsgames,Lstatsgames)
master_stats$Loc<-as.integer(as.factor(master_stats$Loc))

statsbyday<-NULL
for (i in 1:max(master_stats$DayNum)){
  submaster_stats<-master_stats[DayNum<i]
  teamstatbyday<-dcast(submaster_stats,TeamID+Season~.,mean,
                       value.var=c("Score","Loc","NumOT","FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF"))
  teamstatbyday$DayNum<-i
  statsbyday<-rbind(statsbyday,teamstatbyday)
}

setnames(statsbyday,"TeamID","team_1")
master$Season<-as.character(master$Season)
statsbyday$Season<-as.character(statsbyday$Season)
statsbyday$team_1<-as.character(statsbyday$team_1)
setkey(statsbyday,Season,team_1,DayNum)
setkey(master,Season,team_1,DayNum)

master<-statsbyday[master,roll=T]

setnames(statsbyday,"team_1","team_2")
setnames(statsbyday,c("Score","Loc","NumOT","FGM","FGA","FGM3","FGA3","FTM","FTA","OR","DR","Ast","TO","Stl","Blk","PF"),
         c("Score_2","Loc_2","NumOT_2","FGM_2","FGA_2","FGM3_2","FGA3_2","FTM_2","FTA_2","OR_2","DR_2","Ast_2","TO_2","Stl_2","Blk_2","PF_2"))
setkey(statsbyday,Season,team_2,DayNum)
setkey(master,Season,team_2,DayNum)

master<-statsbyday[master,roll=T]

master$Score<-master$Score_2-master$Score
master$Loc<-master$Loc_2-master$Loc
master$NumOT<-master$NumOT_2-master$NumOT
master$FGM<-master$FGM_2-master$FGM
master$FGA<-master$FGA_2-master$FGA
master$FGM3<-master$FGM3_2-master$FGM3
master$FGA3<-master$FGA3_2-master$FGA3
master$FTM<-master$FTM_2-master$FTM 
master$FTA<-master$FTA_2-master$FTA
master$OR<-master$OR_2-master$OR
master$DR<-master$DR_2-master$DR
master$Ast<-master$Ast_2-master$Ast 
master$TO<-master$TO_2-master$TO
master$Stl<-master$Stl_2-master$Stl
master$Blk<-master$Blk_2-master$Blk
master$PF<-master$PF_2-master$PF

master<-master[order(Season,DayNum)]
master<-master[,.(id_num,team_1,team_2,Score,Loc,FGM,FGA,FGM3,FGA3,FTM,FTA,OR,DR,Ast,TO,Stl,Blk,PF,
                  POM_dif,PIG_dif,SAG_dif,MOR_dif,DOK_dif,result)]
master<-master[!is.na(master$Score)]
master<-master[!is.na(master$Loc)]
master<-master[!is.na(master$FGM)]
master<-master[!is.na(master$FGA)]
master<-master[!is.na(master$FGM3)]
master<-master[!is.na(master$FGA3)]
master<-master[!is.na(master$FTM)]
master<-master[!is.na(master$FTA)]
master<-master[!is.na(master$OR)]
master<-master[!is.na(master$DR)]
master<-master[!is.na(master$Ast)]
master<-master[!is.na(master$TO)]
master<-master[!is.na(master$Stl)]
master<-master[!is.na(master$Blk)]
master<-master[!is.na(master$PF)]





BS_data<-replicate(500,sample(c(1,0),nrow(master),replace=T))
BS_data<-data.table(BS_data)

setnames(BS_data,paste0("V",1:500),paste0("BS_",1:500))

master<-cbind(master,BS_data)

#split the train and test set
test<-master[result==0.5]
train<-master[result==1]
test<-test[order(id_num)]

test$id_num<-NULL
train$id_num<-NULL

rand_inx<-sample(1:nrow(train),nrow(train)*0.5)
train_a<-train[rand_inx,]
train_b<-train[!rand_inx,]

train_b$result<-0
train_b$PIG_dif<-train_b$PIG_dif*-1
train_b$SAG_dif<-train_b$SAG_dif*-1
train_b$MOR_dif<-train_b$MOR_dif*-1
train_b$DOK_dif<-train_b$DOK_dif*-1
train_b$POM_dif<-train_b$POM_dif*-1
train_b$Score<-train_b$Score*-1
train_b$Loc<-train_b$Loc*-1
train_b$FGM<-train_b$FGM*-1
train_b$FGA<-train_b$FGA*-1
train_b$FGM3<-train_b$FGM3*-1
train_b$FGA3<-train_b$FGA3*-1
train_b$FTM<-train_b$FTM*-1
train_b$FTA<-train_b$FTA*-1
train_b$OR<-train_b$OR*-1
train_b$DR<-train_b$DR*-1
train_b$Ast<-train_b$Ast*-1
train_b$TO<-train_b$TO*-1
train_b$Stl<-train_b$Stl*-1
train_b$Blk<-train_b$Blk*-1
train_b$PF<-train_b$PF*-1

train<-rbind(train_a,train_b)

fwrite(test,'/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/PSU_Stat_380/Lectures/11. MTG_price wrapup/project/volume/data/interim/test.csv')
fwrite(train,'/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/PSU_Stat_380/Lectures/11. MTG_price wrapup/project/volume/data/interim/train.csv')



ggplot(train,aes(x=POM_dif,fill=as.factor(result)))+geom_density()
